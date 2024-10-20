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
    start: BytePos,
}

impl<'cnbt> CoenobitaParser<'cnbt> {
    pub fn new(parser: Parser<'cnbt>) -> Self {
        let start = parser.token.span.lo();
        CoenobitaParser { parser, start }
    }

    pub fn parse_ty(&mut self) -> PResult<'cnbt, Ty> {
        self.start();

        Ok(Ty {
            flow_pair: self.parse_flow_pair()?,
            kind: self.parse_ty_kind()?,
            span: self.end(),
        })
    }

    pub fn parse_flow_pair(&mut self) -> PResult<'cnbt, FlowPair> {
        self.start();

        Ok(FlowPair {
            explicit: self.parse_flow_set()?,
            implicit: self.parse_flow_set()?,
            span: self.end(),
        })
    }

    pub fn parse_flow_set(&mut self) -> PResult<'cnbt, FlowSet> {
        self.start();
        self.parser.expect(&OpenDelim(Delimiter::Brace))?;

        if self.parser.eat(&BinOp(BinOpToken::Star)) {
            self.parser.expect(&CloseDelim(Delimiter::Brace))?;
            Ok(FlowSet::Universal(self.end()))
        } else {
            let mut origins = vec![];
            while self.parser.token != CloseDelim(Delimiter::Bracket) {
                origins.push(self.parser.parse_ident()?);

                if self.parser.token != CloseDelim(Delimiter::Bracket) {
                    self.parser.expect(&Comma)?;
                }
            }

            self.parser.expect(&CloseDelim(Delimiter::Brace))?;
            Ok(FlowSet::Specific(origins, self.end()))
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

    fn start(&mut self) {
        self.start = self.parser.token.span.lo()
    }

    fn end(&self) -> Span {
        self.parser.prev_token.span.with_lo(self.start)
    }
}
