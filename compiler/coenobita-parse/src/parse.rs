use coenobita_ast::provenance::{Provenance, ProvenancePair};
use rustc_ast::token::TokenKind::{self, BinOp, CloseDelim, Comma, OpenDelim};
use rustc_ast::token::{BinOpToken, Delimiter};
use rustc_errors::PResult;
use rustc_parse::parser::Parser;

use coenobita_ast::flow::{FlowPair, FlowSet};

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

        Ok(ProvenancePair {
            first,
            last,
            span: parser.end(start),
        })
    }
}

impl Parse for Provenance {
    fn parse<'cnbt>(parser: &mut CoenobitaParser<'cnbt>) -> PResult<'cnbt, Self> {
        let start = parser.start();

        if parser.parser.eat(&BinOp(BinOpToken::Star)) {
            Ok(Provenance::Universal(parser.end(start)))
        } else {
            Ok(Provenance::Specific(
                parser.parser.parse_ident()?,
                parser.end(start),
            ))
        }
    }
}

impl Parse for FlowPair {
    fn parse<'cnbt>(parser: &mut CoenobitaParser<'cnbt>) -> PResult<'cnbt, Self> {
        let start = parser.start();

        Ok(FlowPair {
            explicit: FlowSet::parse(parser)?,
            implicit: FlowSet::parse(parser)?,
            span: parser.end(start),
        })
    }
}

impl Parse for FlowSet {
    fn parse<'cnbt>(parser: &mut CoenobitaParser<'cnbt>) -> PResult<'cnbt, Self> {
        let start = parser.start();
        parser.parser.expect(&OpenDelim(Delimiter::Brace))?;

        if parser.parser.eat(&BinOp(BinOpToken::Star)) {
            parser.parser.expect(&CloseDelim(Delimiter::Brace))?;
            Ok(FlowSet::Universal(parser.end(start)))
        } else {
            let mut origins = vec![];
            while parser.parser.token != CloseDelim(Delimiter::Brace) {
                origins.push(parser.parser.parse_ident()?);

                if parser.parser.token != CloseDelim(Delimiter::Brace) {
                    parser.parser.expect(&Comma)?;
                }
            }

            parser.parser.expect(&CloseDelim(Delimiter::Brace))?;
            Ok(FlowSet::Specific(origins, parser.end(start)))
        }
    }
}
