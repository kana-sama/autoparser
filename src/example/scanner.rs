use crate::token::*;

use super::token::{self, Token, k};

pub fn scan(source: &str) -> Result<Vec<Token>, Error> {
    let mut iter = TokenIterator {
        source: source.as_bytes(),

        cursor: 0,
        column: 1,
        line:   1,

        token_start: 0,
    };

    let mut tokens = Vec::with_capacity(source.len());

    while let Some(token) = iter.next()? {
        tokens.push(token);
    }

    let eof_start = if tokens.is_empty() {
        source.len()
    } else {
        usize::min(source.len(), tokens.last().unwrap().loc().end)
    };

    tokens.push(TokenOfKind { kind: EOF, loc: eof_start..source.len() }.into());

    return Ok(tokens);
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    UnexpectedChar {
        found:    Option<char>,
        expected: Vec<char>,
    },
    MissingTagName,
}

struct TokenIterator<'source> {
    source: &'source [u8],

    cursor: usize,
    column: usize,
    line:   usize,

    token_start: usize,
}

impl<'source> TokenIterator<'source> {
    fn peek_any(&self) -> Option<u8> {
        self.source.get(self.cursor).copied()
    }

    fn peek_many<const N: usize>(&self) -> Option<&[u8; N]> {
        self.source.get(self.cursor..self.cursor + N)?.as_array()
    }

    fn shift_cursor(&mut self) {
        if let Some(c) = self.source.get(self.cursor) {
            self.cursor += 1;
            if *c == b'\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
    }

    fn take_any(&mut self) -> Option<u8> {
        let c = self.peek_any()?;
        self.shift_cursor();
        Some(c)
    }

    fn take(&mut self, expected: u8) -> Result<(), Error> {
        let char = self.take_any().ok_or_else(|| Error::UnexpectedChar {
            found:    None,
            expected: vec![expected as char],
        })?;

        if char == expected {
            Ok(())
        } else {
            Err(Error::UnexpectedChar {
                found:    Some(char as char),
                expected: vec![expected as char],
            })
        }
    }

    fn skip_while(&mut self, f: impl Fn(u8) -> bool) {
        while let Some(c) = self.peek_any() {
            if f(c) {
                self.shift_cursor();
            } else {
                break;
            }
        }
    }

    fn skip_whitespace(&mut self) {
        self.skip_while(|c| c.is_ascii_whitespace());
    }

    fn skip_comment(&mut self) {
        if self.peek_many::<2>() == Some(b"//") {
            self.skip_while(|c| c != b'\n');
        }
    }

    fn skip_to_next_significant_char(&mut self) {
        let mut cursor = self.cursor;
        loop {
            self.skip_whitespace();
            self.skip_comment();

            if cursor == self.cursor {
                break;
            } else {
                cursor = self.cursor;
            }
        }
    }

    fn start_new_token(&mut self) {
        self.token_start = self.cursor;
    }

    fn token_slice(&self) -> &[u8] {
        &self.source[self.token_start..self.cursor]
    }

    fn token<K: KindOfSet<token::Set>>(&self, kind: K) -> Token {
        K::to_token(TokenOfKind { kind, loc: self.token_start..self.cursor })
    }

    fn keyword(&self) -> Option<Token> {
        match std::str::from_utf8(&self.source[self.token_start..self.cursor]).ok()? {
            "begin" => Some(self.token(k!["begin"])),
            "end" => Some(self.token(k!["end"])),
            "let" => Some(self.token(k!["let"])),
            "else" => Some(self.token(k!["else"])),
            "return" => Some(self.token(k!["return"])),
            "case" => Some(self.token(k!["case"])),
            "of" => Some(self.token(k!["of"])),
            "enum" => Some(self.token(k!["enum"])),
            "function" => Some(self.token(k!["function"])),
            "returns" => Some(self.token(k!["returns"])),
            "type" => Some(self.token(k!["type"])),
            "module" => Some(self.token(k!["module"])),
            "public" => Some(self.token(k!["public"])),
            "interface" => Some(self.token(k!["interface"])),
            "implementation" => Some(self.token(k!["implementation"])),
            _ => None,
        }
    }

    fn next(&mut self) -> Result<Option<Token>, Error> {
        self.skip_to_next_significant_char();
        self.start_new_token();

        let char = match self.take_any() {
            Some(c) => c,
            None => return Ok(None),
        };

        return Ok(Some(match char {
            b'(' => self.token(k!["("]),
            b')' => self.token(k![")"]),

            b'=' => self.token(k!["="]),
            b':' => self.token(k![":"]),
            b';' => self.token(k![";"]),
            b',' => self.token(k![","]),
            b'-' => {
                self.take(b'>')?;
                self.token(k!["->"])
            }

            b'#' => {
                self.skip_while(is_ident_char);

                if self.token_slice().len() == 1 {
                    Err(Error::MissingTagName)?
                } else {
                    self.token(k!["#tag"])
                }
            }

            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                self.skip_while(is_ident_char);

                if self.token_slice() == b"_" {
                    self.token(k!["_"])
                } else if let Some(kw) = self.keyword() {
                    kw
                } else {
                    self.token(k!["ident"])
                }
            }

            char => Err(Error::UnexpectedChar { found: Some(char as char), expected: vec![] })?,
        }));
    }
}

fn is_ident_char(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_' || c == b'-' || c == b'?'
}
