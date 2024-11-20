use crate::{
    token::{Keyword, Token},
    Error, ErrorKind, Severity,
};
use alloc::string::String;
use core::{iter::Peekable, str::Chars};

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    line_number: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source: source.chars().peekable(),
            line_number: 1,
        }
    }

    pub fn line_number(&self) -> usize {
        self.line_number
    }

    pub fn next(&mut self) -> Result<Option<Token>, Error> {
        while let Some(c) = self.source.next() {
            match c {
                '\n' => self.line_number += 1,
                _ if c.is_whitespace() => {}
                _ if c.is_ascii_alphabetic() || c == '_' => return Ok(Some(self.identifier(c))),
                _ if c.is_ascii_digit() => return Ok(Some(self.constant(c)?)),
                '(' => return Ok(Some(Token::OpenParenthesis)),
                ')' => return Ok(Some(Token::CloseParenthesis)),
                '{' => return Ok(Some(Token::OpenBrace)),
                '}' => return Ok(Some(Token::CloseBrace)),
                ';' => return Ok(Some(Token::Semicolon)),
                _ => return Err(self.err(ErrorKind::InvalidToken(c))),
            }
        }

        Ok(None)
    }
}

impl Lexer<'_> {
    fn identifier(&mut self, first: char) -> Token {
        let mut identifier = vec![first];

        while let Some(c) = self.source.next_if(|&c| c.is_alphanumeric() || c == '_') {
            identifier.push(c);
        }

        let identifier = identifier.into_iter().collect::<String>();

        match identifier.as_str() {
            "int" => Token::Keyword(Keyword::Int),
            "void" => Token::Keyword(Keyword::Void),
            "return" => Token::Keyword(Keyword::Return),
            _ => Token::Identifier(identifier),
        }
    }

    fn constant(&mut self, first: char) -> Result<Token, Error> {
        let mut token = vec![first];

        while let Some(c) = self.source.next_if(|c| c.is_ascii_digit()) {
            token.push(c);
        }

        token
            .into_iter()
            .collect::<String>()
            .parse::<i32>()
            .map_err(|_| self.err(ErrorKind::ConstantTooLarge))
            .map(Token::Constant)
    }

    fn err(&self, kind: ErrorKind) -> Error {
        Error {
            line_number: self.line_number,
            kind,
            severity: Severity::Error,
        }
    }
}
