use std::collections::HashSet;

use coenobita_middle::flow::FlowPair;
use coenobita_middle::origin::OriginSet;
use coenobita_middle::provenance::ProvenancePair;
use rustc_ast::token::Delimiter;
use rustc_ast::token::TokenKind::CloseDelim;
use rustc_errors::PResult;
use rustc_parse::parser::Parser;
use rustc_span::Span;
use std::collections::HashMap;

use crate::token::*;

pub type VarSpan = (String, Span);

pub struct CoenobitaParser<'cnbt> {
    pub variables: HashMap<String, Span>,
    pub parser: Parser<'cnbt>,
}

pub trait Parse: Sized {
    fn parse<'cnbt>(parser: &mut CoenobitaParser<'cnbt>) -> PResult<'cnbt, Self>;
}

impl Parse for ProvenancePair {
    fn parse<'cnbt>(parser: &mut CoenobitaParser<'cnbt>) -> PResult<'cnbt, Self> {
        parser.parser.expect(OPEN_PAREN)?;

        let first = OriginSet::parse(parser)?;
        parser.parser.expect(COMMA)?;
        let last = OriginSet::parse(parser)?;

        parser.parser.expect(CLOSE_PAREN)?;

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
        parser.parser.expect(OPEN_BRACE)?;

        if parser.parser.eat(STAR) {
            parser.parser.expect(CLOSE_BRACE)?;
            Ok(OriginSet::Universal)
        } else {
            let mut origins = HashSet::new();
            while parser.parser.token != CloseDelim(Delimiter::Brace) {
                origins.insert(parser.parser.parse_ident()?.to_string());

                if parser.parser.token != CloseDelim(Delimiter::Brace) {
                    parser.parser.expect(COMMA)?;
                }
            }

            parser.parser.expect(CLOSE_BRACE)?;
            Ok(OriginSet::Specific(origins))
        }
    }
}
