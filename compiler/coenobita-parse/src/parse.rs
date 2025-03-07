use std::collections::HashSet;

use coenobita_middle::flow::FlowPair;
use coenobita_middle::origin::OriginSet;
use coenobita_middle::provenance::ProvenancePair;
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

        let first = OriginSet::parse(parser)?;
        parser.parser.expect(&TokenKind::Comma)?;
        let last = OriginSet::parse(parser)?;

        parser.parser.expect(&CloseDelim(Delimiter::Parenthesis))?;

        Ok(ProvenancePair::new(first, last))
    }
}

impl Parse for FlowPair {
    fn parse<'cnbt>(parser: &mut CoenobitaParser<'cnbt>) -> PResult<'cnbt, Self> {
        let explicit = OriginSet::parse(parser)?;
        let implicit = OriginSet::parse(parser)?;

        Ok(FlowPair::new(explicit, implicit))
    }
}

impl Parse for OriginSet {
    fn parse<'cnbt>(parser: &mut CoenobitaParser<'cnbt>) -> PResult<'cnbt, Self> {
        parser.parser.expect(&OpenDelim(Delimiter::Brace))?;

        if parser.parser.eat(&BinOp(BinOpToken::Star)) {
            parser.parser.expect(&CloseDelim(Delimiter::Brace))?;
            Ok(OriginSet::Universal)
        } else {
            let mut origins = HashSet::new();
            while parser.parser.token != CloseDelim(Delimiter::Brace) {
                origins.insert(parser.parser.parse_ident()?.to_string());

                if parser.parser.token != CloseDelim(Delimiter::Brace) {
                    parser.parser.expect(&Comma)?;
                }
            }

            parser.parser.expect(&CloseDelim(Delimiter::Brace))?;
            Ok(OriginSet::Specific(origins))
        }
    }
}
