// Copyright (c) 2025 Ludovico Rossi
// SPDX-License-Identifier: MPL-2.0

use crate::{
    token::{Keyword, Token, TokenKind},
    Error, ErrorKind, Severity,
};
use alloc::{string::String, vec::Vec};
use core::{iter::Peekable, str::Chars};

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    next: Option<Token>,
    line_number: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source: source.chars().peekable(),
            next: None,
            line_number: 1,
            column: 1,
        }
    }

    pub fn peek(&mut self) -> Result<Option<&Token>, Error> {
        if self.next.is_none() {
            self.next = self.advance()?;
        }
        Ok(self.next.as_ref())
    }

    pub fn next(&mut self) -> Result<Option<Token>, Error> {
        if let Some(token) = self.next.take() {
            Ok(Some(token))
        } else {
            self.advance()
        }
    }
}

impl Lexer<'_> {
    fn advance(&mut self) -> Result<Option<Token>, Error> {
        while let Some(c) = self.source.next() {
            match c {
                '\n' => {
                    self.line_number += 1;
                    self.column = 1;
                }
                _ if c.is_whitespace() => self.column += 1,
                _ if c.is_ascii_alphabetic() || c == '_' => {
                    let line_number = self.line_number;
                    let column = self.column;
                    let kind = self.identifier(c);
                    return Ok(Some(Token {
                        line_number,
                        column,
                        kind,
                    }));
                }
                '0' if self.source.next_if_eq(&'x').is_some()
                    || self.source.next_if_eq(&'X').is_some() =>
                {
                    let line_number = self.line_number;
                    let column = self.column;
                    let kind = TokenKind::Constant(
                        self.hex_constant()
                            .map_err(|e| fix_error(e, line_number, column))?,
                    );
                    self.column += 2;
                    return Ok(Some(Token {
                        line_number,
                        column,
                        kind,
                    }));
                }
                '0' if self.source.next_if_eq(&'b').is_some()
                    || self.source.next_if_eq(&'B').is_some() =>
                {
                    let line_number = self.line_number;
                    let column = self.column;
                    let kind = TokenKind::Constant(
                        self.binary_constant()
                            .map_err(|e| fix_error(e, line_number, column))?,
                    );
                    self.column += 2;
                    return Ok(Some(Token {
                        line_number,
                        column,
                        kind,
                    }));
                }
                _ if c.is_ascii_digit() => {
                    let line_number = self.line_number;
                    let column = self.column;
                    let kind = self
                        .decimal_constant(c)
                        .map_err(|e| fix_error(e, line_number, column))?;
                    return Ok(Some(Token {
                        line_number,
                        column,
                        kind,
                    }));
                }
                '/' if self.source.next_if_eq(&'/').is_some() => self.ignore_line(),
                '/' if self.source.next_if_eq(&'*').is_some() => {
                    self.column += 2;
                    self.ignore_multiline_comment()
                }
                '(' => return Ok(Some(self.token(TokenKind::OpenParenthesis))),
                ')' => return Ok(Some(self.token(TokenKind::CloseParenthesis))),
                '{' => return Ok(Some(self.token(TokenKind::OpenBrace))),
                '}' => return Ok(Some(self.token(TokenKind::CloseBrace))),
                ';' => return Ok(Some(self.token(TokenKind::Semicolon))),
                '~' => return Ok(Some(self.token(TokenKind::Tilde))),
                '-' if self.source.next_if_eq(&'-').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::TwoHyphens)));
                }
                '-' if self.source.next_if_eq(&'=').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::HyphenEqualSign)));
                }
                '-' => return Ok(Some(self.token(TokenKind::Hyphen))),
                '+' if self.source.next_if_eq(&'+').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::TwoPlusSigns)));
                }
                '+' if self.source.next_if_eq(&'=').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::PlusEqualSign)));
                }
                '+' => return Ok(Some(self.token(TokenKind::PlusSign))),
                '*' if self.source.next_if_eq(&'=').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::AsteriskEqualSign)));
                }
                '*' => return Ok(Some(self.token(TokenKind::Asterisk))),
                '/' if self.source.next_if_eq(&'=').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::SlashEqualSign)));
                }
                '/' => return Ok(Some(self.token(TokenKind::Slash))),
                '%' if self.source.next_if_eq(&'=').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::PercentEqualSign)));
                }
                '%' => return Ok(Some(self.token(TokenKind::Percent))),
                '^' if self.source.next_if_eq(&'=').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::CircumflexEqualSign)));
                }
                '^' => return Ok(Some(self.token(TokenKind::Circumflex))),
                '<' if self.source.next_if_eq(&'<').is_some() => {
                    self.column += 1;
                    return Ok(Some(if self.source.next_if_eq(&'=').is_some() {
                        self.column += 1;
                        self.token(TokenKind::TwoLessThanOpsEqualSign)
                    } else {
                        self.token(TokenKind::TwoLessThanOps)
                    }));
                }
                '>' if self.source.next_if_eq(&'>').is_some() => {
                    self.column += 1;
                    return Ok(Some(if self.source.next_if_eq(&'=').is_some() {
                        self.column += 1;
                        self.token(TokenKind::TwoGreaterThanOpsEqualSign)
                    } else {
                        self.token(TokenKind::TwoGreaterThanOps)
                    }));
                }
                '&' if self.source.next_if_eq(&'&').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::TwoAmpersands)));
                }
                '&' if self.source.next_if_eq(&'=').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::AmpersandEqualSign)));
                }
                '&' => return Ok(Some(self.token(TokenKind::Ampersand))),
                '|' if self.source.next_if_eq(&'|').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::TwoBars)));
                }
                '|' if self.source.next_if_eq(&'=').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::BarEqualSign)));
                }
                '|' => return Ok(Some(self.token(TokenKind::Bar))),
                '=' if self.source.next_if_eq(&'=').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::TwoEqualSigns)));
                }
                '=' => return Ok(Some(self.token(TokenKind::EqualSign))),
                '!' if self.source.next_if_eq(&'=').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::NotEqualSign)));
                }
                '!' => return Ok(Some(self.token(TokenKind::ExclamationPoint))),
                '<' if self.source.next_if_eq(&'=').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::LessThanOrEqualToOp)));
                }
                '<' => return Ok(Some(self.token(TokenKind::LessThanOp))),
                '>' if self.source.next_if_eq(&'=').is_some() => {
                    self.column += 1;
                    return Ok(Some(self.token(TokenKind::GreaterThanOrEqualToOp)));
                }
                '>' => return Ok(Some(self.token(TokenKind::GreaterThanOp))),
                '?' => return Ok(Some(self.token(TokenKind::QuestionMark))),
                ':' => return Ok(Some(self.token(TokenKind::Colon))),
                ',' => return Ok(Some(self.token(TokenKind::Comma))),
                '#' => return Ok(Some(self.token(TokenKind::NumberSign))),
                '\"' => {
                    let line_number = self.line_number;
                    let column = self.column;
                    let kind = self
                        .string()
                        .map_err(|e| fix_error(e, line_number, column))?;
                    return Ok(Some(Token {
                        line_number,
                        column,
                        kind,
                    }));
                }
                _ => return Err(self.err(ErrorKind::InvalidToken(c))),
            }
        }

        Ok(None)
    }

    fn ignore_line(&mut self) {
        for c in self.source.by_ref() {
            if c == '\n' {
                self.line_number += 1;
                self.column = 1;
                break;
            }
        }
    }

    fn ignore_multiline_comment(&mut self) {
        while let Some(c) = self.source.next() {
            match c {
                '*' if self.source.next_if_eq(&'/').is_some() => {
                    self.column += 2;
                    break;
                }
                '\n' => {
                    self.line_number += 1;
                    self.column = 1;
                }
                _ => self.column += 1,
            }
        }
    }

    fn identifier(&mut self, first: char) -> TokenKind {
        let mut identifier = vec![first];
        self.column += 1;

        while let Some(c) = self.source.next_if(|&c| c.is_alphanumeric() || c == '_') {
            identifier.push(c);
            self.column += 1;
        }

        let identifier = identifier.into_iter().collect::<String>();

        match identifier.as_str() {
            "int" => TokenKind::Keyword(Keyword::Int),
            "void" => TokenKind::Keyword(Keyword::Void),
            "return" => TokenKind::Keyword(Keyword::Return),
            "if" => TokenKind::Keyword(Keyword::If),
            "else" => TokenKind::Keyword(Keyword::Else),
            "do" => TokenKind::Keyword(Keyword::Do),
            "while" => TokenKind::Keyword(Keyword::While),
            "for" => TokenKind::Keyword(Keyword::For),
            "break" => TokenKind::Keyword(Keyword::Break),
            "continue" => TokenKind::Keyword(Keyword::Continue),
            "extern" => TokenKind::Keyword(Keyword::Extern),
            "const" => TokenKind::Keyword(Keyword::Const),
            "switch" => TokenKind::Keyword(Keyword::Switch),
            "case" => TokenKind::Keyword(Keyword::Case),
            "default" => TokenKind::Keyword(Keyword::Default),
            _ => TokenKind::Identifier(identifier.into()),
        }
    }

    fn hex_constant(&mut self) -> Result<i32, Error> {
        self.integer_constant(16, |c| c.is_ascii_hexdigit())
    }

    fn binary_constant(&mut self) -> Result<i32, Error> {
        self.integer_constant(2, |c| c == '0' || c == '1')
    }

    fn decimal_constant(&mut self, first: char) -> Result<TokenKind, Error> {
        let mut token = vec![first];
        self.column += 1;

        while let Some(c) = self.source.next_if(|c| c.is_ascii_digit()) {
            token.push(c);
            self.column += 1;
        }

        token
            .into_iter()
            .collect::<String>()
            .parse::<i32>()
            .map_err(|_| self.err(ErrorKind::ConstantTooLarge))
            .map(TokenKind::Constant)
    }

    fn integer_constant<F: Fn(char) -> bool>(
        &mut self,
        base: u32,
        included: F,
    ) -> Result<i32, Error> {
        let mut string = String::new();

        while let Some(c) = self.source.next_if(|&c| included(c)) {
            string.push(c);
            self.column += 1;
        }

        if string.is_empty() {
            return Err(self.err(ErrorKind::InvalidConstant));
        }

        let value = i32::from_str_radix(&string, base)
            .map_err(|_| self.err(ErrorKind::ConstantTooLarge))?;

        Ok(value)
    }

    fn string(&mut self) -> Result<TokenKind, Error> {
        let mut data = Vec::new();
        self.column += 1;

        while let Some(c) = self.source.next_if(|&c| c != '"') {
            self.column += 1;

            match c {
                '\\' => match self.source.next() {
                    Some('t') => data.push(b'\t'),
                    Some('n') => data.push(b'\n'),
                    Some('r') => data.push(b'\r'),
                    Some('\"') => data.push(b'\"'),
                    Some('\'') => data.push(b'\''),
                    Some('\\') => data.push(b'\\'),
                    Some('x') => {
                        let constant = self.hex_constant()?;
                        let value = u8::try_from(constant)
                            .map_err(|_| self.err(ErrorKind::ConstantTooLarge))?;
                        data.push(value);
                    }
                    Some(_) | None => return Err(self.err(ErrorKind::InvalidEscapeSequence)),
                },
                _ if c.is_ascii() => data.push(c as u8),
                _ => return Err(self.err(ErrorKind::InvalidCharacter(c))),
            }
        }

        if matches!(self.source.next(), Some('"')) {
            self.column += 1;
            Ok(TokenKind::String(data))
        } else {
            Err(self.err(ErrorKind::UnterminatedString))
        }
    }

    fn token(&mut self, kind: TokenKind) -> Token {
        let column = self.column;
        self.column += 1;
        Token {
            line_number: self.line_number,
            column,
            kind,
        }
    }

    fn err(&self, kind: ErrorKind) -> Error {
        Error {
            line_number: self.line_number,
            column: self.column,
            kind,
            severity: Severity::Error,
        }
    }
}

fn fix_error(mut error: Error, line_number: usize, column: usize) -> Error {
    error.line_number = line_number;
    error.column = column;
    error
}
