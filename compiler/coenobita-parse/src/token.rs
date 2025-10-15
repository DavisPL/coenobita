use rustc_ast::token::{
    BinOpToken, Delimiter,
    TokenKind::{self, BinOp, CloseDelim, Colon, Comma, OpenDelim, RArrow},
};
use rustc_parse::parser::{ExpKeywordPair, ExpTokenPair, TokenType};
use rustc_span::kw;

pub const KW_FN: ExpKeywordPair = ExpKeywordPair {
    kw: kw::Fn,
    token_type: TokenType::KwFn,
};
pub const KW_STRUCT: ExpKeywordPair = ExpKeywordPair {
    kw: kw::Struct,
    token_type: TokenType::KwStruct,
};

pub const PIPE: ExpTokenPair = ExpTokenPair {
    tok: &BinOp(BinOpToken::Or),
    token_type: TokenType::Or,
};

pub const OPEN_PAREN: ExpTokenPair = ExpTokenPair {
    tok: &OpenDelim(Delimiter::Parenthesis),
    token_type: TokenType::OpenParen,
};
pub const OPEN_BRACE: ExpTokenPair = ExpTokenPair {
    tok: &OpenDelim(Delimiter::Brace),
    token_type: TokenType::OpenBrace,
};
pub const OPEN_BRACKET: ExpTokenPair = ExpTokenPair {
    tok: &OpenDelim(Delimiter::Bracket),
    token_type: TokenType::OpenBracket,
};

pub const CLOSE_PAREN: ExpTokenPair = ExpTokenPair {
    tok: &CloseDelim(Delimiter::Parenthesis),
    token_type: TokenType::CloseParen,
};
pub const CLOSE_BRACE: ExpTokenPair = ExpTokenPair {
    tok: &CloseDelim(Delimiter::Brace),
    token_type: TokenType::CloseBrace,
};
pub const CLOSE_BRACKET: ExpTokenPair = ExpTokenPair {
    tok: &CloseDelim(Delimiter::Bracket),
    token_type: TokenType::CloseBracket,
};

pub const EQUAL: ExpTokenPair = ExpTokenPair {
    tok: &TokenKind::Eq,
    token_type: TokenType::Eq,
};

pub const COLON: ExpTokenPair = ExpTokenPair {
    tok: &Colon,
    token_type: TokenType::Colon,
};
pub const COMMA: ExpTokenPair = ExpTokenPair {
    tok: &Comma,
    token_type: TokenType::Comma,
};
pub const RARROW: ExpTokenPair = ExpTokenPair {
    tok: &RArrow,
    token_type: TokenType::RArrow,
};
pub const STAR: ExpTokenPair = ExpTokenPair {
    tok: &BinOp(BinOpToken::Star),
    token_type: TokenType::Star,
};
