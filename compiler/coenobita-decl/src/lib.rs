use coenobita_middle::set::Set;
use coenobita_middle::ty::{ProvType, SetVar, Type, TypeKind};
use std::collections::HashMap;

#[derive(Clone, Copy)]
struct Span {
    start: usize,
    end: usize,
}

struct Token {
    text: String,
    span: Span,
}

struct ParseError {
    message: String,
    span: Span,
}

type ParseResult<T> = Result<T, ParseError>;

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    input: String,
}

impl Parser {
    fn new(input: &str) -> Self {
        let tokens = tokenize(input);
        Parser {
            tokens,
            pos: 0,
            input: input.to_string(),
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn next(&mut self) -> ParseResult<Token> {
        match self.tokens.get(self.pos) {
            Some(tok) => {
                self.pos += 1;
                Ok(tok.clone())
            }
            None => {
                let span = if let Some(last) = self.tokens.last() {
                    Span {
                        start: last.span.end,
                        end: last.span.end,
                    }
                } else {
                    Span { start: 0, end: 0 }
                };
                Err(ParseError {
                    message: "unexpected end of input".to_string(),
                    span,
                })
            }
        }
    }

    fn expect(&mut self, expected: &str) -> ParseResult<()> {
        let tok = self.next()?;
        if tok.text == expected {
            Ok(())
        } else {
            Err(ParseError {
                message: format!("expected '{}', got '{}'", expected, tok.text),
                span: tok.span,
            })
        }
    }

    fn error(&self, message: String, span: Span) -> ParseError {
        ParseError { message, span }
    }

    fn parse_file(&mut self) -> ParseResult<HashMap<String, Type>> {
        let mut sigs = HashMap::new();

        while self.peek().is_some() {
            let (path, ty) = self.parse_signature()?;
            self.expect(";")?;
            sigs.insert(path, ty);
        }

        Ok(sigs)
    }

    fn parse_signature(&mut self) -> ParseResult<(String, Type)> {
        self.expect("fn")?;
        let path = self.parse_path()?;

        // Extract crate name (first segment of path)
        let krate = path.split("::").next().unwrap_or(&path).to_string();
        let krate_set = Set::Concrete(std::collections::BTreeSet::from([krate]));

        // Parse optional binder
        let (binder, var_to_idx) = if self.peek().map(|t| t.text.as_str()) == Some("[") {
            self.next()?;
            let mut binder = Vec::new();
            let mut var_to_idx = HashMap::new();
            let mut idx = 0;

            while self.peek().map(|t| t.text.as_str()) != Some("]") {
                let sv = self.parse_bound()?;
                var_to_idx.insert(sv.ident.clone(), idx);
                binder.push(sv);
                idx += 1;

                if self.peek().map(|t| t.text.as_str()) == Some(",") {
                    self.next()?;
                }
            }
            self.expect("]")?;
            (binder, var_to_idx)
        } else {
            (Vec::new(), HashMap::new())
        };

        // Parse parameters
        self.expect("(")?;
        let mut params = Vec::new();

        if self.peek().map(|t| t.text.as_str()) != Some(")") {
            loop {
                params.push(self.parse_pty()?);
                if self.peek().map(|t| t.text.as_str()) == Some(",") {
                    self.next()?;
                } else {
                    break;
                }
            }
        }
        self.expect(")")?;

        // Parse return type
        self.expect("->")?;
        let rty = self.parse_ty()?;

        Ok((
            path,
            Type {
                kind: TypeKind::Fn(params, Box::new(rty)),
                binder,
                var_to_idx,
                binder_idx: 0,
                intrinsic: [krate_set.clone(), krate_set.clone(), krate_set],
                intrinsic_idx: 0,
            },
        ))
    }

    fn parse_bound(&mut self) -> ParseResult<SetVar> {
        let var = self.next()?;
        self.expect("⊆")?;
        let bound = self.parse_set()?;
        Ok(SetVar::new(var.text, bound))
    }

    fn parse_pty(&mut self) -> ParseResult<ProvType> {
        let ty = self.parse_ty()?;
        self.expect("|")?;
        let providers = self.parse_set()?;
        Ok(ProvType { ty, providers })
    }

    fn parse_ty(&mut self) -> ParseResult<Type> {
        let s1 = self.parse_set()?;

        // Check that we can parse another set (not hitting | or other delimiters)
        if self.peek().map(|t| &t.text as &str) == Some("|")
            || self.peek().map(|t| &t.text as &str) == Some(")")
            || self.peek().map(|t| &t.text as &str) == Some(",")
        {
            let tok = self.peek().unwrap();
            return Err(ParseError {
                message: format!("expected a set (type requires 3 sets), found '{}'", tok.text),
                span: tok.span,
            });
        }

        let s2 = self.parse_set()?;

        // Check again before parsing the third set
        if self.peek().map(|t| &t.text as &str) == Some("|")
            || self.peek().map(|t| &t.text as &str) == Some(")")
            || self.peek().map(|t| &t.text as &str) == Some(",")
        {
            let tok = self.peek().unwrap();
            return Err(ParseError {
                message: format!("expected a set (type requires 3 sets), found '{}'", tok.text),
                span: tok.span,
            });
        }

        let s3 = self.parse_set()?;

        Ok(Type {
            kind: TypeKind::Opaque,
            binder: Vec::new(),
            var_to_idx: HashMap::new(),
            binder_idx: 0,
            intrinsic: [s1, s2, s3],
            intrinsic_idx: 0,
        })
    }

    fn parse_set(&mut self) -> ParseResult<Set> {
        let mut left = self.parse_set_atom()?;

        // Handle union (left associative)
        while self.peek().map(|t| t.text.as_str()) == Some("U") {
            self.next()?;
            let right = self.parse_set_atom()?;
            left = left.union(right);
        }

        Ok(left)
    }

    fn parse_set_atom(&mut self) -> ParseResult<Set> {
        match self.peek() {
            Some(tok) if tok.text == "{" => {
                self.next()?;
                let mut elems = std::collections::BTreeSet::new();

                while self.peek().map(|t| t.text.as_str()) != Some("}") {
                    let tok = self.next()?;

                    // Validate that element is a valid identifier
                    if !is_valid_identifier(&tok.text) {
                        return Err(ParseError {
                            message: format!("invalid identifier '{}' in set", tok.text),
                            span: tok.span,
                        });
                    }

                    elems.insert(tok.text);
                    if self.peek().map(|t| t.text.as_str()) == Some(",") {
                        self.next()?;
                    }
                }
                self.expect("}")?;
                Ok(Set::Concrete(elems))
            }
            Some(tok) => {
                let ident = self.next()?;

                // Validate that this is a valid identifier
                if !is_valid_identifier(&ident.text) {
                    return Err(ParseError {
                        message: format!("expected a valid identifier, found '{}'", ident.text),
                        span: ident.span,
                    });
                }

                Ok(Set::Variable(ident.text))
            }
            None => {
                let span = if let Some(last) = self.tokens.last() {
                    Span {
                        start: last.span.end,
                        end: last.span.end,
                    }
                } else {
                    Span { start: 0, end: 0 }
                };
                Err(ParseError {
                    message: "expected set".to_string(),
                    span,
                })
            }
        }
    }

    fn parse_path(&mut self) -> ParseResult<String> {
        let mut path = self.next()?.text;

        while self.peek().map(|t| t.text.as_str()) == Some("::") {
            self.next()?;
            path.push_str("::");
            path.push_str(&self.next()?.text);
        }

        Ok(path)
    }

    fn format_error(&self, err: &ParseError) -> String {
        let mut result = String::new();

        // Find the line containing the error
        let mut line_start = 0;
        let mut line_num = 1;
        let mut error_line_start = 0;

        for (i, ch) in self.input.char_indices() {
            if i >= err.span.start {
                error_line_start = line_start;
                break;
            }
            if ch == '\n' {
                line_start = i + 1;
                line_num += 1;
            }
        }

        // Find the end of the error line
        let mut line_end = self.input.len();
        for (i, ch) in self.input[error_line_start..].char_indices() {
            if ch == '\n' {
                line_end = error_line_start + i;
                break;
            }
        }

        let line = &self.input[error_line_start..line_end];

        // Calculate column in characters, not bytes
        let col_chars = self.input[error_line_start..err.span.start].chars().count();
        let len_chars = self.input[err.span.start..err.span.end].chars().count().max(1);

        result.push_str(&format!("error: {}\n", err.message));
        result.push_str(&format!("  --> line {}:{}\n", line_num, col_chars + 1));
        result.push_str(&format!("{:4} | {}\n", line_num, line));
        result.push_str(&format!(
            "     | {}{}",
            " ".repeat(col_chars),
            "^".repeat(len_chars)
        ));

        result
    }
}

impl Clone for Token {
    fn clone(&self) -> Self {
        Token {
            text: self.text.clone(),
            span: self.span,
        }
    }
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.char_indices().peekable();

    while let Some(&(pos, ch)) = chars.peek() {
        match ch {
            // Skip whitespace
            ' ' | '\t' | '\n' | '\r' => {
                chars.next();
            }
            // Single-char tokens
            '(' | ')' | '[' | ']' | '{' | '}' | ';' | ',' | '|' => {
                tokens.push(Token {
                    text: ch.to_string(),
                    span: Span {
                        start: pos,
                        end: pos + 1,
                    },
                });
                chars.next();
            }
            // Multi-char operators
            ':' => {
                let start = pos;
                chars.next();
                if chars.peek().map(|(_, c)| c) == Some(&':') {
                    chars.next();
                    tokens.push(Token {
                        text: "::".to_string(),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    });
                } else {
                    tokens.push(Token {
                        text: ":".to_string(),
                        span: Span {
                            start,
                            end: start + 1,
                        },
                    });
                }
            }
            '-' => {
                let start = pos;
                chars.next();
                if chars.peek().map(|(_, c)| c) == Some(&'>') {
                    chars.next();
                    tokens.push(Token {
                        text: "->".to_string(),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    });
                } else {
                    tokens.push(Token {
                        text: "-".to_string(),
                        span: Span {
                            start,
                            end: start + 1,
                        },
                    });
                }
            }
            'U' => {
                tokens.push(Token {
                    text: "U".to_string(),
                    span: Span {
                        start: pos,
                        end: pos + 1,
                    },
                });
                chars.next();
            }
            '⊆' => {
                let byte_len = '⊆'.len_utf8();
                tokens.push(Token {
                    text: "⊆".to_string(),
                    span: Span {
                        start: pos,
                        end: pos + byte_len,
                    },
                });
                chars.next();
            }
            // Identifiers
            _ if ch.is_alphabetic() || ch == '_' => {
                let start = pos;
                let mut ident = String::new();
                while let Some(&(_, c)) = chars.peek() {
                    if c.is_alphanumeric() || c == '_' {
                        ident.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let end = start + ident.len();
                tokens.push(Token {
                    text: ident,
                    span: Span { start, end },
                });
            }
            _ => {
                // Keep unknown characters as tokens so we can report errors
                let byte_len = ch.len_utf8();
                tokens.push(Token {
                    text: ch.to_string(),
                    span: Span {
                        start: pos,
                        end: pos + byte_len,
                    },
                });
                chars.next();
            }
        }
    }

    tokens
}

// Check if a string is a valid Rust identifier
fn is_valid_identifier(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }

    let mut chars = s.chars();

    // First character must be letter or underscore
    match chars.next() {
        Some(c) if c.is_alphabetic() || c == '_' => {}
        _ => return false,
    }

    // Rest can be alphanumeric or underscore
    for c in chars {
        if !c.is_alphanumeric() && c != '_' {
            return false;
        }
    }

    true
}

pub fn parse(input: &str) -> Result<HashMap<String, Type>, String> {
    let mut parser = Parser::new(input);
    match parser.parse_file() {
        Ok(result) => Ok(result),
        Err(err) => Err(parser.format_error(&err)),
    }
}
