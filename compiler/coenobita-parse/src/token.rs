use rustc_ast::token::TokenKind::{self, BinOp, CloseDelim, Comma, OpenDelim};
use rustc_ast::token::{BinOpToken, Delimiter};
use rustc_parse::parser::{ExpTokenPair, TokenType};

pub const PIPE: ExpTokenPair = ExpTokenPair {
    tok: &BinOp(BinOpToken::Or),
    token_type: TokenType::Or,
};

pub const OPEN_BRACE: ExpTokenPair = ExpTokenPair {
    tok: &OpenDelim(Delimiter::Brace),
    token_type: TokenType::OpenBrace,
};

pub const CLOSE_BRACE: ExpTokenPair = ExpTokenPair {
    tok: &CloseDelim(Delimiter::Brace),
    token_type: TokenType::CloseBrace,
};

pub const EQUAL: ExpTokenPair = ExpTokenPair {
    tok: &TokenKind::Eq,
    token_type: TokenType::Eq,
};

pub const COMMA: ExpTokenPair = ExpTokenPair {
    tok: &Comma,
    token_type: TokenType::Comma,
};
