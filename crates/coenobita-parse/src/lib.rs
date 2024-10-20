#![feature(rustc_private)]

extern crate rustc_errors;
extern crate rustc_parse;
extern crate rustc_span;

use rustc_errors::PResult;
use rustc_parse::parser::Parser;

use coenobita_ast::ast::{Ty, TyKind};
use coenobita_ast::flow::{FlowPair, FlowSet};

use rustc_span::{BytePos, Span};

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
        todo!()
    }

    pub fn parse_ty_kind(&mut self) -> PResult<'cnbt, TyKind> {
        todo!()
    }

    fn start(&mut self) {
        self.start = self.parser.token.span.lo()
    }

    fn end(&self) -> Span {
        self.parser.prev_token.span.with_lo(self.start)
    }
}
