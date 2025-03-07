use std::collections::HashSet;

use coenobita_middle::flow::{FlowPair, FlowSet};
use coenobita_middle::provenance::{Provenance, ProvenancePair};
use rustc_ast::token::TokenKind::{self, BinOp, CloseDelim, Comma, OpenDelim};
use rustc_ast::token::{BinOpToken, Delimiter};
use rustc_errors::PResult;
use rustc_parse::parser::Parser;

pub struct CoenobitaParser<'cnbt> {
    pub parser: Parser<'cnbt>,
}

pub trait Parse: Sized {
    fn parse<'cnbt>(parser: &mut CoenobitaParser<'cnbt>) -> PResult<'cnbt, Self>;
}

impl Parse for ProvenancePair {
    fn parse<'cnbt>(parser: &mut CoenobitaParser<'cnbt>) -> PResult<'cnbt, Self> {
        let start = parser.start();

        parser.parser.expect(&OpenDelim(Delimiter::Parenthesis))?;

        let first = Provenance::parse(parser)?;
        parser.parser.expect(&TokenKind::Comma)?;
        let last = Provenance::parse(parser)?;

        parser.parser.expect(&CloseDelim(Delimiter::Parenthesis))?;

        Ok(ProvenancePair(first, last))
    }
}

impl Parse for Provenance {
    fn parse<'cnbt>(parser: &mut CoenobitaParser<'cnbt>) -> PResult<'cnbt, Self> {
        let start = parser.start();

        if parser.parser.eat(&BinOp(BinOpToken::Star)) {
            Ok(Provenance::Universal)
        } else {
            let mut origins = HashSet::new();
            origins.insert(parser.parser.parse_ident()?.to_string());

            Ok(Provenance::Specific(origins))
        }
    }
}

impl Parse for FlowPair {
    fn parse<'cnbt>(parser: &mut CoenobitaParser<'cnbt>) -> PResult<'cnbt, Self> {
        let explicit = FlowSet::parse(parser)?;
        let implicit = FlowSet::parse(parser)?;

        Ok(FlowPair::new(explicit, implicit))
    }
}

impl Parse for FlowSet {
    fn parse<'cnbt>(parser: &mut CoenobitaParser<'cnbt>) -> PResult<'cnbt, Self> {
        parser.parser.expect(&OpenDelim(Delimiter::Brace))?;

        if parser.parser.eat(&BinOp(BinOpToken::Star)) {
            parser.parser.expect(&CloseDelim(Delimiter::Brace))?;
            Ok(FlowSet::Universal)
        } else {
            let mut origins = HashSet::new();
            while parser.parser.token != CloseDelim(Delimiter::Brace) {
                origins.insert(parser.parser.parse_ident()?.to_string());

                if parser.parser.token != CloseDelim(Delimiter::Brace) {
                    parser.parser.expect(&Comma)?;
                }
            }

            parser.parser.expect(&CloseDelim(Delimiter::Brace))?;
            Ok(FlowSet::Specific(origins))
        }
    }
}
