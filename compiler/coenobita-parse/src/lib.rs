#![feature(rustc_private)]

extern crate rustc_ast;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_session;
extern crate rustc_span;

pub mod parse;
pub(crate) mod token;

use parse::CoenobitaParser;
use token::*;

use std::collections::{BTreeSet, HashMap};
use std::io;
use std::sync::Arc;

use coenobita_middle::set::Set;

use rustc_ast::token::{LitKind, TokenKind};
use rustc_ast::tokenstream::TokenStream;

use rustc_data_structures::sync;
use rustc_driver::DEFAULT_LOCALE_RESOURCES;

use rustc_errors::annotate_snippet_emitter_writer::AnnotateSnippetEmitter;
use rustc_errors::emitter::{stderr_destination, Emitter, HumanEmitter, HumanReadableErrorType};
use rustc_errors::json::JsonEmitter;
use rustc_errors::PResult;
use rustc_errors::{fallback_fluent_bundle, LazyFallbackBundle};

use rustc_middle::ty::TyCtxt;
use rustc_parse::parser::Parser;

use rustc_session::config::{ErrorOutputType, Options};
use rustc_session::parse::ParseSess;
use rustc_span::source_map::SourceMap;
use rustc_span::{BytePos, Span};

pub struct Param {
    pub left: Spanned<String>,
    pub bound: Spanned<Set>,
    pub right: HashMap<String, Span>,
}

pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    fn new(value: T, span: Span) -> Self {
        Spanned { value, span }
    }
}

pub struct Input {
    pub index: Spanned<usize>,
    pub integrity: [Spanned<Set>; 3],
    pub providers: Spanned<Set>,
    pub variables: HashMap<String, Span>,
}

pub struct Field {
    pub integrity: [Spanned<Set>; 3],
    pub providers: Spanned<Set>,
    pub variables: HashMap<String, Span>,
}

/// Used for `output` and `local` attributes
pub struct Other {
    pub integrity: [Spanned<Set>; 3],
    pub variables: HashMap<String, Span>,
}

impl<'cnbt> CoenobitaParser<'cnbt> {
    pub fn new(parser: Parser<'cnbt>) -> Self {
        CoenobitaParser {
            parser,
            variables: HashMap::new(),
        }
    }

    fn parse_other(&mut self) -> PResult<'cnbt, Other> {
        let integrity = [self.parse_set()?, self.parse_set()?, self.parse_set()?];

        Ok(Other {
            integrity,
            variables: self.variables.clone(),
        })
    }

    pub fn parse_local(&mut self) -> PResult<'cnbt, Other> {
        self.parse_other()
    }

    pub fn parse_output(&mut self) -> PResult<'cnbt, Other> {
        self.parse_other()
    }

    pub fn parse_field(&mut self) -> PResult<'cnbt, Field> {
        let integrity = [self.parse_set()?, self.parse_set()?, self.parse_set()?];
        self.parser.expect(PIPE)?;
        let providers = self.parse_set()?;

        Ok(Field {
            integrity,
            providers,
            variables: self.variables.clone(),
        })
    }

    pub fn parse_input(&mut self) -> PResult<'cnbt, Input> {
        let span = self.parser.token.span;

        let index = match self.parser.token.kind {
            TokenKind::Literal(lit) => match lit.kind {
                LitKind::Integer => {
                    let index: usize = lit.symbol.to_string().parse().map_err(|e| {
                        self.parser
                            .dcx()
                            .struct_span_err(self.parser.token.span, format!("invalid index {}", lit.symbol))
                    })?;

                    self.parser.bump();

                    index
                }

                _ => {
                    return Err(self.parser.dcx().struct_span_err(
                        self.parser.token.span,
                        format!("expected nonzero integer, found {}", lit.symbol),
                    ))
                }
            },

            _ => {
                return Err(self
                    .parser
                    .dcx()
                    .struct_span_err(self.parser.token.span, format!("expected nonzero integer")))
            }
        };

        let integrity = [self.parse_set()?, self.parse_set()?, self.parse_set()?];
        self.parser.expect(PIPE)?;
        let providers = self.parse_set()?;

        Ok(Input {
            index: Spanned::new(index, span),
            integrity,
            providers,
            variables: self.variables.clone(),
        })
    }

    pub fn parse_param(&mut self) -> PResult<'cnbt, Param> {
        let var = self.parser.parse_ident()?;
        self.parse_subset()?;
        let bound = self.parse_set()?;

        Ok(Param {
            left: Spanned::new(var.to_string(), var.span),
            bound,
            right: self.variables.clone(),
        })
    }

    pub fn parse_set(&mut self) -> PResult<'cnbt, Spanned<Set>> {
        let start = self.start();

        let left = if self.parser.eat(OPEN_BRACE) {
            let mut origins = BTreeSet::new();

            if let Ok(ident) = self.parser.parse_ident() {
                origins.insert(ident.to_string());
            }

            while !self.parser.eat(CLOSE_BRACE) {
                self.parser.expect(COMMA)?;

                if self.parser.eat(CLOSE_BRACE) {
                    break;
                }

                let next_origin = self.parser.parse_ident()?.to_string();
                origins.insert(next_origin);
            }

            if origins.contains("*") {
                Set::Universe
            } else {
                Set::Concrete(origins)
            }
        } else {
            let ident = self.parser.parse_ident()?;

            if !self.variables.contains_key(&ident.to_string()) {
                self.variables.insert(ident.to_string(), ident.span.clone());
            }

            Set::Variable(ident.to_string())
        };

        if self.eat_union() {
            let right = self.parse_set()?;
            return Ok(Spanned::new(left.union(right.value), self.end(start)));
        }

        Ok(Spanned::new(left, self.end(start)))
    }

    pub fn parse_subset(&mut self) -> PResult<'cnbt, ()> {
        // Check for "sub" identifier
        if let TokenKind::Ident(sym, _) = self.parser.token.kind {
            if sym.to_string() == "sub" {
                self.parser.bump();
            } else {
                return Err(self
                    .parser
                    .dcx()
                    .struct_span_err(self.parser.token.span, format!("expected `sub`, found `{}`", sym)));
            }
        } else {
            return Err(self.parser.dcx().struct_span_err(
                self.parser.token.span,
                format!("expected `sub`, found `{:?}`", self.parser.token.kind),
            ));
        }

        // Expect the `=` token
        self.parser.expect(EQUAL)?;

        Ok(())
    }

    pub fn eat_union(&mut self) -> bool {
        if let TokenKind::Ident(sym, _) = self.parser.token.kind {
            if sym.to_string() == "U" {
                self.parser.bump();
                return true;
            }
        }

        false
    }

    fn start(&mut self) -> BytePos {
        self.parser.token.span.lo()
    }

    fn end(&self, start: BytePos) -> Span {
        self.parser.prev_token.span.with_lo(start)
    }
}

pub fn create_psess(tcx: &TyCtxt) -> ParseSess {
    let opts = tcx.sess.opts.clone();
    let source_map = tcx.sess.psess.clone_source_map();
    let fallback_bundle = fallback_fluent_bundle(DEFAULT_LOCALE_RESOURCES.to_vec(), false);
    let emitter = emitter(opts, source_map.clone(), fallback_bundle);
    let dcx = rustc_errors::DiagCtxt::new(emitter);

    ParseSess::with_dcx(dcx, source_map)
}

pub fn create_parser<'cnbt>(psess: &'cnbt ParseSess, stream: TokenStream) -> CoenobitaParser<'cnbt> {
    // Set up the rustc parser and the custom CnbtParser
    let rustc_parser = Parser::new(&psess, stream, None);
    let parser = CoenobitaParser::new(rustc_parser);

    parser
}

pub fn emitter(
    opts: Options,
    source_map: Arc<SourceMap>,
    fallback_bundle: LazyFallbackBundle,
) -> Box<dyn Emitter + sync::DynSend> {
    let bundle = None;
    let track_diagnostics = opts.unstable_opts.track_diagnostics;

    match opts.error_format {
        ErrorOutputType::HumanReadable(err_type, color_config) => {
            if let HumanReadableErrorType::AnnotateSnippet = err_type {
                let emitter =
                    AnnotateSnippetEmitter::new(Some(source_map), None, fallback_bundle, false, false);
                Box::new(emitter)
            } else {
                let dst = stderr_destination(color_config);
                let emitter = HumanEmitter::new(dst, fallback_bundle)
                    .sm(Some(source_map))
                    .short_message(err_type.short())
                    .diagnostic_width(opts.diagnostic_width)
                    .track_diagnostics(track_diagnostics)
                    .terminal_url(opts.unstable_opts.terminal_urls);
                Box::new(emitter)
            }
        }
        ErrorOutputType::Json {
            pretty,
            json_rendered,
            color_config,
        } => Box::new(
            JsonEmitter::new(
                Box::new(io::BufWriter::new(io::stderr())),
                Some(source_map),
                fallback_bundle,
                pretty,
                json_rendered,
                color_config,
            )
            .fluent_bundle(bundle)
            .track_diagnostics(track_diagnostics)
            .diagnostic_width(opts.diagnostic_width)
            .terminal_url(opts.unstable_opts.terminal_urls),
        ),
    }
}
