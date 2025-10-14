#![feature(rustc_private)]

extern crate rustc_ast;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_session;
extern crate rustc_span;

use std::collections::{BTreeSet, HashSet};
use std::fmt::Debug;
use std::io;
use std::sync::Arc;

use coenobita_middle::property::Property;
use coenobita_middle::set::Set;
use std::collections::HashMap;
// use coenobita_middle::ty::{Integrity, Type};
use parse::{CoenobitaParser, Parse};
use rustc_ast::token::TokenKind::{CloseDelim, Ident, OpenDelim};
use rustc_ast::token::{Delimiter, LitKind, Token, TokenKind};
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

// use coenobita_ast::{TyAST, TypeFormAST};

use log::debug;

pub mod parse;
pub(crate) mod token;

use token::*;

use crate::parse::VarSpan;

pub struct Take {
    pub left: VarSpan,
    pub bound: Set,
    pub right: HashMap<String, Span>,
}

pub enum PassTarget {
    Infer,
    Return,
    Argument(usize),
}

pub struct Pass {
    pub left: (PassTarget, Span),
    pub set: Set,
    pub right: HashMap<String, Span>,
}

impl<'cnbt> CoenobitaParser<'cnbt> {
    pub fn new(parser: Parser<'cnbt>) -> Self {
        CoenobitaParser {
            parser,
            variables: HashMap::new(),
        }
    }

    pub fn parse_take(&mut self) -> PResult<'cnbt, Take> {
        let var = self.parser.parse_ident()?;
        self.parse_subset()?;
        let bound = self.parse_set()?;

        Ok(Take {
            left: (var.to_string(), var.span),
            bound,
            right: self.variables.clone(),
        })
    }

    pub fn parse_pass(&mut self) -> PResult<'cnbt, Pass> {
        let span = self.parser.token.span;

        let target = match self.parser.token.kind {
            TokenKind::RArrow => {
                self.parser.bump();
                PassTarget::Return
            }

            TokenKind::Literal(lit) => match lit.kind {
                LitKind::Integer => {
                    let index: usize = lit.symbol.to_string().parse().map_err(|e| {
                        self.parser
                            .dcx()
                            .struct_span_err(self.parser.token.span, format!("invalid index {}", lit.symbol))
                    })?;

                    self.parser.bump();

                    PassTarget::Argument(index)
                }

                _ => {
                    return Err(self.parser.dcx().struct_span_err(
                        self.parser.token.span,
                        format!("expected nonzero integer, found {}", lit.symbol),
                    ))
                }
            },

            TokenKind::Ident(symbol, _) => {
                if symbol.as_str() == "_" {
                    self.parser.bump();

                    PassTarget::Infer
                } else {
                    return Err(self
                        .parser
                        .dcx()
                        .struct_span_err(self.parser.token.span, format!("expected '_', found {}", symbol)));
                }
            }

            _ => {
                return Err(self
                    .parser
                    .dcx()
                    .struct_span_err(self.parser.token.span, format!("invalid pass target")))
            }
        };

        let set = self.parse_set()?;

        Ok(Pass {
            left: (target, span),
            set,
            right: self.variables.clone(),
        })
    }

    pub fn parse_set(&mut self) -> PResult<'cnbt, Set> {
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
            return Ok(left.union(right));
        }

        Ok(left)
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
