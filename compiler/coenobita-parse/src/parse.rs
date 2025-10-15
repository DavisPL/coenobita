use std::collections::HashSet;

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
