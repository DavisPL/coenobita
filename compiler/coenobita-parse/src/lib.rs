#![feature(rustc_private)]

extern crate rustc_ast;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_session;
extern crate rustc_span;

use std::collections::HashSet;
use std::fmt::Debug;
use std::io;
use std::sync::Arc;

use coenobita_middle::property::Property;
use coenobita_middle::set::Set;
use coenobita_middle::ty::{Integrity, Type};
use parse::{CoenobitaParser, Parse};
use rustc_ast::token::TokenKind::{CloseDelim, Ident, OpenDelim};
use rustc_ast::token::{Delimiter, TokenKind};
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

use coenobita_ast::{TyAST, TypeFormAST};

use log::debug;

pub mod parse;
pub(crate) mod token;

use token::*;

impl<'cnbt> CoenobitaParser<'cnbt> {
    pub fn new(parser: Parser<'cnbt>) -> Self {
        CoenobitaParser { parser }
    }

    pub fn parse_ty(&mut self) -> PResult<'cnbt, TyAST> {
        let start = self.start();

        Ok(TyAST {
            inner: Type::new(self.parse_integrity()?, self.parse_ty_kind()?.into()),
            span: self.end(start),
        })
    }

    pub fn parse_integrity(&self) -> PResult<'cnbt, Integrity> {
        todo!()
    }

    pub fn parse_ty_kind(&mut self) -> PResult<'cnbt, TypeFormAST> {
        if self.parser.eat_keyword(KW_FN) {
            // We are parsing a function type
            let mut set_variables = vec![];

            if self.parser.eat(OPEN_BRACKET) {
                // The function takes set parameters
                while !self.parser.eat(CLOSE_BRACKET) {
                    let variable = self.parser.parse_ident()?.to_string();

                    if self.eat_subset() {
                        debug!("Ate subset");
                        let right = self.parse_set_compound()?;

                        set_variables.push(format!("{variable} ⊆ {:?}", right));
                    } else {
                        set_variables.push(variable);
                    }

                    if self.parser.token != CloseDelim(Delimiter::Bracket) {
                        self.parser.expect(COMMA)?;
                    }
                }
            }

            debug!("Function takes set variables {:?}", set_variables);

            self.parser.expect(OPEN_PAREN)?;

            let mut args = vec![];
            while self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                args.push(self.parse_ty()?);

                if self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                    self.parser.expect(COMMA)?;
                }
            }

            self.parser.expect(CLOSE_PAREN)?;
            self.parser.expect(RARROW)?;

            Ok(TypeFormAST::Fn(vec![], args, Box::new(self.parse_ty()?)))
        } else if self.parser.eat_keyword(KW_STRUCT) {
            // We are parsing a struct type
            if self.parser.token.kind == OpenDelim(Delimiter::Brace) {
                // We are parsing a `{ ... }` struct
                self.parser.expect(OPEN_BRACE)?;

                let mut fields = vec![];
                while self.parser.token != CloseDelim(Delimiter::Brace) {
                    let ident = self.parser.parse_ident()?;
                    self.parser.expect(COLON)?;

                    fields.push((ident, self.parse_ty()?));

                    if self.parser.token != CloseDelim(Delimiter::Brace) {
                        self.parser.expect(COMMA)?;
                    }
                }

                self.parser.expect(CLOSE_BRACE)?;
                Ok(TypeFormAST::Struct(fields))
            } else {
                // We are parsing a `( ... )` struct
                self.parser.expect(OPEN_PAREN)?;

                let mut elements = vec![];
                while self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                    elements.push(self.parse_ty()?);

                    if self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                        self.parser.expect(COMMA)?;
                    }
                }

                self.parser.expect(CLOSE_PAREN)?;
                Ok(TypeFormAST::StructTuple(elements))
            }
        } else if self.parser.eat(OPEN_PAREN) {
            // We are parsing a tuple type
            let mut elements = vec![];
            while self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                elements.push(self.parse_ty()?);

                if self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                    self.parser.expect(COMMA)?;
                }
            }

            self.parser.expect(CLOSE_PAREN)?;
            Ok(TypeFormAST::Tuple(elements))
        } else if self.parser.eat(OPEN_BRACKET) {
            // We are parsing an array type
            let element = self.parse_ty()?;
            self.parser.expect(CLOSE_BRACKET)?;
            Ok(TypeFormAST::Array(Box::new(element)))
        } else {
            Ok(TypeFormAST::Opaque)
        }
    }

    pub fn parse_set(&mut self) -> PResult<'cnbt, Set> {
        let result = if self.parser.eat(OPEN_BRACE) {
            let first_origin = self.parser.parse_ident()?.to_string();

            if first_origin == "*" {
                Set::Concrete(None)
            } else {
                let mut origins = HashSet::new();
                origins.insert(first_origin);

                while !self.parser.eat(CLOSE_BRACE) {
                    self.parser.expect(COMMA)?;

                    let next_origin = self.parser.parse_ident()?.to_string();
                    origins.insert(next_origin);
                }

                Set::Concrete(Some(origins))
            }
        } else {
            Set::Variable(self.parser.parse_ident()?.to_string())
        };

        return Ok(result);
    }

    pub fn parse_set_compound(&mut self) -> PResult<'cnbt, SetCmpd> {
        let left = self.parse_set()?;

        if self.eat_union() {
            let right = self.parse_set_compound()?;

            debug!("> right is {:?}", right);

            return Ok(SetCmpd::Union(Box::new(SetCmpd::Atomic(left)), Box::new(right)));
        }

        return Ok(SetCmpd::Atomic(left));
    }

    pub fn eat_subset(&mut self) -> bool {
        if let TokenKind::Ident(sym, _) = self.parser.token.kind {
            if sym.to_string() == "sub" {
                self.parser.bump();
                return self.parser.eat(EQUAL);
            }
        }

        false
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
