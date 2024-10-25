#![feature(rustc_private)]

extern crate rustc_ast;
extern crate rustc_errors;
extern crate rustc_parse;
extern crate rustc_span;

use rustc_ast::token::TokenKind::{self, BinOp, CloseDelim, Comma, OpenDelim};
use rustc_ast::token::{BinOpToken, Delimiter};
use rustc_errors::PResult;
use rustc_parse::parser::Parser;
use rustc_span::symbol::kw;
use rustc_span::{BytePos, Span};

use coenobita_ast::ast::{Ty, TyKind};
use coenobita_ast::flow::{FlowPair, FlowSet};

pub struct CoenobitaParser<'cnbt> {
    parser: Parser<'cnbt>,
}

impl<'cnbt> CoenobitaParser<'cnbt> {
    pub fn new(parser: Parser<'cnbt>) -> Self {
        CoenobitaParser { parser }
    }

    pub fn parse_ty(&mut self) -> PResult<'cnbt, Ty> {
        let start = self.start();

        Ok(Ty {
            fpair: self.parse_flow_pair()?,
            kind: self.parse_ty_kind()?,
            span: self.end(start),
        })
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

    pub fn parse_ty_kind(&mut self) -> PResult<'cnbt, TyKind> {
        if self.parser.eat_keyword(kw::Fn) {
            // We are parsing a function type
            self.parser
                .expect(&TokenKind::OpenDelim(Delimiter::Parenthesis))?;

            let mut args = vec![];
            while self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                args.push(self.parse_ty()?);

                if self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                    self.parser.expect(&Comma)?;
                }
            }

            self.parser.expect(&CloseDelim(Delimiter::Parenthesis))?;
            self.parser.expect(&TokenKind::RArrow)?;

            Ok(TyKind::Fn(args, Box::new(self.parse_ty()?)))
        } else if self.parser.eat(&OpenDelim(Delimiter::Parenthesis)) {
            // We are parsing a tuple type
            let mut items = vec![];
            while self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                items.push(self.parse_ty()?);

                if self.parser.token != CloseDelim(Delimiter::Parenthesis) {
                    self.parser.expect(&Comma)?;
                }
            }

            self.parser.expect(&CloseDelim(Delimiter::Parenthesis))?;
            Ok(TyKind::Tup(items))
        } else {
            Ok(TyKind::Abstract)
        }
    }

    fn start(&mut self) -> BytePos {
        self.parser.token.span.lo()
    }

    fn end(&self, start: BytePos) -> Span {
        self.parser.prev_token.span.with_lo(start)
    }
}
