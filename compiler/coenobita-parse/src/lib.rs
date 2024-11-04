#![feature(rustc_private)]

extern crate rustc_ast;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_session;
extern crate rustc_span;

use std::io;
use std::sync::Arc;

use rustc_ast::token::TokenKind::{self, BinOp, CloseDelim, Comma, OpenDelim};
use rustc_ast::token::{BinOpToken, Delimiter};
use rustc_ast::tokenstream::TokenStream;

use rustc_data_structures::sync;
use rustc_driver::DEFAULT_LOCALE_RESOURCES;

use rustc_errors::annotate_snippet_emitter_writer::AnnotateSnippetEmitter;
use rustc_errors::emitter::{stderr_destination, Emitter, HumanEmitter, HumanReadableErrorType};
use rustc_errors::json::JsonEmitter;
use rustc_errors::registry::Registry;
use rustc_errors::PResult;
use rustc_errors::{fallback_fluent_bundle, LazyFallbackBundle};

use rustc_middle::ty::TyCtxt;
use rustc_parse::parser::Parser;

use rustc_session::config::{ErrorOutputType, Options};
use rustc_session::parse::ParseSess;
use rustc_span::source_map::SourceMap;
use rustc_span::symbol::kw;
use rustc_span::{BytePos, Span};

use coenobita_ast::ast::{Ty, TyKind};
use coenobita_ast::flow::{FlowPair, FlowSet};
use coenobita_ast::provenance::{Provenance, ProvenancePair};

pub struct CoenobitaParser<'cnbt> {
    parser: Parser<'cnbt>,
}

impl<'cnbt> CoenobitaParser<'cnbt> {
    pub fn new(parser: Parser<'cnbt>) -> Self {
        CoenobitaParser { parser }
    }

    pub fn parse_ity(&mut self) -> PResult<'cnbt, Ty<FlowPair>> {
        let start = self.start();

        Ok(Ty {
            property: self.parse_flow_pair()?,
            kind: self.parse_ity_kind()?,
            span: self.end(start),
        })
    }

    pub fn parse_pty(&mut self) -> PResult<'cnbt, Ty<ProvenancePair>> {
        let start = self.start();

        Ok(Ty {
            property: self.parse_provenance_pair()?,
            kind: self.parse_pty_kind()?,
            span: self.end(start),
        })
    }

    pub fn parse_provenance_pair(&mut self) -> PResult<'cnbt, ProvenancePair> {
        let start = self.start();

        self.parser.expect(&OpenDelim(Delimiter::Parenthesis))?;

        let first = self.parse_provenance()?;
        self.parser.expect(&TokenKind::Comma)?;
        let last = self.parse_provenance()?;

        self.parser.expect(&CloseDelim(Delimiter::Parenthesis))?;

        Ok(ProvenancePair {
            first,
            last,
            span: self.end(start),
        })
    }

    pub fn parse_provenance(&mut self) -> PResult<'cnbt, Provenance> {
        let start = self.start();

        if self.parser.eat(&BinOp(BinOpToken::Star)) {
            Ok(Provenance::Universal(self.end(start)))
        } else {
            Ok(Provenance::Specific(self.parser.parse_ident()?, self.end(start)))
        }
    }

    pub fn parse_flow_pair(&mut self) -> PResult<'cnbt, FlowPair> {
        let start = self.start();

        Ok(FlowPair {
            explicit: self.parse_flow_set()?,
            implicit: self.parse_flow_set()?,
            span: self.end(start),
        })
    }

    pub fn parse_flow_set(&mut self) -> PResult<'cnbt, FlowSet> {
        let start = self.start();
        self.parser.expect(&OpenDelim(Delimiter::Brace))?;

        if self.parser.eat(&BinOp(BinOpToken::Star)) {
            self.parser.expect(&CloseDelim(Delimiter::Brace))?;
            Ok(FlowSet::Universal(self.end(start)))
        } else {
            let mut origins = vec![];
            while self.parser.token != CloseDelim(Delimiter::Brace) {
                origins.push(self.parser.parse_ident()?);

                if self.parser.token != CloseDelim(Delimiter::Brace) {
                    self.parser.expect(&Comma)?;
                }
            }

            self.parser.expect(&CloseDelim(Delimiter::Brace))?;
            Ok(FlowSet::Specific(origins, self.end(start)))
        }
    }

    pub fn parse_ity_kind(&mut self) -> PResult<'cnbt, TyKind<FlowPair>> {
        if self.parser.eat_keyword(kw::Fn) {
            // We are parsing a function type
            self.parser.expect(&OpenDelim(Delimiter::Parenthesis))?;

            let mut args = vec![];
            while self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                args.push(self.parse_ity()?);

                if self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                    self.parser.expect(&Comma)?;
                }
            }

            self.parser.expect(&CloseDelim(Delimiter::Parenthesis))?;
            self.parser.expect(&TokenKind::RArrow)?;

            Ok(TyKind::Fn(args, Box::new(self.parse_ity()?)))
        } else if self.parser.eat_keyword(kw::Struct) {
            // We are parsing a struct type
            if self.parser.token.kind == OpenDelim(Delimiter::Brace) {
                // We are parsing a `{ ... }` struct
                self.parser.expect(&OpenDelim(Delimiter::Brace))?;

                let mut fields = vec![];
                while self.parser.token != CloseDelim(Delimiter::Brace) {
                    let ident = self.parser.parse_ident()?;
                    self.parser.expect(&TokenKind::Colon)?;

                    fields.push((ident, self.parse_ity()?));

                    if self.parser.token != CloseDelim(Delimiter::Brace) {
                        self.parser.expect(&TokenKind::Comma)?;
                    }
                }

                self.parser.expect(&CloseDelim(Delimiter::Brace))?;
                Ok(TyKind::Struct(fields))
            } else {
                // We are parsing a `( ... )` struct
                self.parser.expect(&OpenDelim(Delimiter::Parenthesis))?;

                let mut elements = vec![];
                while self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                    elements.push(self.parse_ity()?);

                    if self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                        self.parser.expect(&TokenKind::Comma)?;
                    }
                }

                self.parser.expect(&CloseDelim(Delimiter::Parenthesis))?;
                Ok(TyKind::StructTuple(elements))
            }
        } else if self.parser.eat(&OpenDelim(Delimiter::Parenthesis)) {
            // We are parsing a tuple type
            let mut elements = vec![];
            while self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                elements.push(self.parse_ity()?);

                if self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                    self.parser.expect(&Comma)?;
                }
            }

            self.parser.expect(&CloseDelim(Delimiter::Parenthesis))?;
            Ok(TyKind::Tuple(elements))
        } else if self.parser.eat(&OpenDelim(Delimiter::Bracket)) {
            // We are parsing an array type
            let element = self.parse_ity()?;
            self.parser.expect(&CloseDelim(Delimiter::Bracket))?;
            Ok(TyKind::Array(Box::new(element)))
        } else {
            Ok(TyKind::Opaque)
        }
    }

    pub fn parse_pty_kind(&mut self) -> PResult<'cnbt, TyKind<ProvenancePair>> {
        if self.parser.eat_keyword(kw::Fn) {
            // We are parsing a function type
            self.parser
                .expect(&TokenKind::OpenDelim(Delimiter::Parenthesis))?;

            let mut args = vec![];
            while self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                args.push(self.parse_pty()?);

                if self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                    self.parser.expect(&Comma)?;
                }
            }

            self.parser.expect(&CloseDelim(Delimiter::Parenthesis))?;
            self.parser.expect(&TokenKind::RArrow)?;

            Ok(TyKind::Fn(args, Box::new(self.parse_pty()?)))
        } else if self.parser.eat(&OpenDelim(Delimiter::Parenthesis)) {
            // We are parsing a tuple type
            let mut items = vec![];
            while self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                items.push(self.parse_pty()?);

                if self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                    self.parser.expect(&Comma)?;
                }
            }

            self.parser.expect(&CloseDelim(Delimiter::Parenthesis))?;
            Ok(TyKind::Tuple(items))
        } else if self.parser.eat(&OpenDelim(Delimiter::Bracket)) {
            // We are parsing an array type
            todo!()
        } else {
            Ok(TyKind::Opaque)
        }
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
                source_map,
                fallback_bundle,
                pretty,
                json_rendered,
                color_config,
            )
            .registry(Some(Registry::new(&[])))
            .fluent_bundle(bundle)
            .track_diagnostics(track_diagnostics)
            .diagnostic_width(opts.diagnostic_width)
            .terminal_url(opts.unstable_opts.terminal_urls),
        ),
    }
}
