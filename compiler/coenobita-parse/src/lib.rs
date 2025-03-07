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

use coenobita_middle::property::Property;
use coenobita_middle::ty::Ty;
use parse::{CoenobitaParser, Parse};
use rustc_ast::token::Delimiter;
use rustc_ast::token::TokenKind::{self, CloseDelim, Comma, OpenDelim};
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

use coenobita_ast::{TyAST, TyKindAST};

pub mod parse;

impl<'cnbt> CoenobitaParser<'cnbt> {
    pub fn new(parser: Parser<'cnbt>) -> Self {
        CoenobitaParser { parser }
    }

    pub fn parse_ty<P: Property + Parse>(&mut self) -> PResult<'cnbt, TyAST<P>> {
        let start = self.start();

        Ok(TyAST {
            inner: Ty::new(P::parse(self)?, self.parse_ty_kind()?.into()),
            span: self.end(start),
        })
    }

    pub fn parse_ty_kind<P: Property + Parse>(&mut self) -> PResult<'cnbt, TyKindAST<P>> {
        if self.parser.eat_keyword(kw::Fn) {
            // We are parsing a function type
            self.parser.expect(&OpenDelim(Delimiter::Parenthesis))?;

            let mut args = vec![];
            while self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                args.push(self.parse_ty()?);

                if self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                    self.parser.expect(&Comma)?;
                }
            }

            self.parser.expect(&CloseDelim(Delimiter::Parenthesis))?;
            self.parser.expect(&TokenKind::RArrow)?;

            Ok(TyKindAST::Fn(args, Box::new(self.parse_ty()?)))
        } else if self.parser.eat_keyword(kw::Struct) {
            // We are parsing a struct type
            if self.parser.token.kind == OpenDelim(Delimiter::Brace) {
                // We are parsing a `{ ... }` struct
                self.parser.expect(&OpenDelim(Delimiter::Brace))?;

                let mut fields = vec![];
                while self.parser.token != CloseDelim(Delimiter::Brace) {
                    let ident = self.parser.parse_ident()?;
                    self.parser.expect(&TokenKind::Colon)?;

                    fields.push((ident, self.parse_ty()?));

                    if self.parser.token != CloseDelim(Delimiter::Brace) {
                        self.parser.expect(&TokenKind::Comma)?;
                    }
                }

                self.parser.expect(&CloseDelim(Delimiter::Brace))?;
                Ok(TyKindAST::Struct(fields))
            } else {
                // We are parsing a `( ... )` struct
                self.parser.expect(&OpenDelim(Delimiter::Parenthesis))?;

                let mut elements = vec![];
                while self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                    elements.push(self.parse_ty()?);

                    if self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                        self.parser.expect(&TokenKind::Comma)?;
                    }
                }

                self.parser.expect(&CloseDelim(Delimiter::Parenthesis))?;
                Ok(TyKindAST::StructTuple(elements))
            }
        } else if self.parser.eat(&OpenDelim(Delimiter::Parenthesis)) {
            // We are parsing a tuple type
            let mut elements = vec![];
            while self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                elements.push(self.parse_ty()?);

                if self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                    self.parser.expect(&Comma)?;
                }
            }

            self.parser.expect(&CloseDelim(Delimiter::Parenthesis))?;
            Ok(TyKindAST::Tuple(elements))
        } else if self.parser.eat(&OpenDelim(Delimiter::Bracket)) {
            // We are parsing an array type
            let element = self.parse_ty()?;
            self.parser.expect(&CloseDelim(Delimiter::Bracket))?;
            Ok(TyKindAST::Array(Box::new(element)))
        } else {
            Ok(TyKindAST::Opaque)
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
