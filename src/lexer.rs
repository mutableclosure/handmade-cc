use crate::{
    token::{Keyword, Token},
    Error, ErrorKind, Severity,
};
use alloc::{string::String, vec::Vec};
use core::{iter::Peekable, str::Chars};

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    next: Option<Token>,
    line_number: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source: source.chars().peekable(),
            next: None,
            line_number: 1,
        }
    }

    pub fn line_number(&self) -> usize {
        self.line_number
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
                '\n' => self.line_number += 1,
                _ if c.is_whitespace() => {}
                _ if c.is_ascii_alphabetic() || c == '_' => return Ok(Some(self.identifier(c))),
                '0' if self.source.next_if_eq(&'x').is_some()
                    || self.source.next_if_eq(&'X').is_some() =>
                {
                    return Ok(Some(Token::Constant(self.hex_constant()?)))
                }
                '0' if self.source.next_if_eq(&'b').is_some()
                    || self.source.next_if_eq(&'B').is_some() =>
                {
                    return Ok(Some(Token::Constant(self.binary_constant()?)))
                }
                _ if c.is_ascii_digit() => return Ok(Some(self.decimal_constant(c)?)),
                '/' if self.source.next_if_eq(&'/').is_some() => self.ignore_line(),
                '/' if self.source.next_if_eq(&'*').is_some() => self.ignore_multiline_comment(),
                '(' => return Ok(Some(Token::OpenParenthesis)),
                ')' => return Ok(Some(Token::CloseParenthesis)),
                '{' => return Ok(Some(Token::OpenBrace)),
                '}' => return Ok(Some(Token::CloseBrace)),
                ';' => return Ok(Some(Token::Semicolon)),
                '~' => return Ok(Some(Token::Tilde)),
                '-' if self.source.next_if_eq(&'-').is_some() => {
                    return Ok(Some(Token::TwoHyphens))
                }
                '-' if self.source.next_if_eq(&'=').is_some() => {
                    return Ok(Some(Token::HyphenEqualSign))
                }
                '-' => return Ok(Some(Token::Hyphen)),
                '+' if self.source.next_if_eq(&'+').is_some() => {
                    return Ok(Some(Token::TwoPlusSigns))
                }
                '+' if self.source.next_if_eq(&'=').is_some() => {
                    return Ok(Some(Token::PlusEqualSign))
                }
                '+' => return Ok(Some(Token::PlusSign)),
                '*' if self.source.next_if_eq(&'=').is_some() => {
                    return Ok(Some(Token::AsteriskEqualSign))
                }
                '*' => return Ok(Some(Token::Asterisk)),
                '/' if self.source.next_if_eq(&'=').is_some() => {
                    return Ok(Some(Token::SlashEqualSign))
                }
                '/' => return Ok(Some(Token::Slash)),
                '%' if self.source.next_if_eq(&'=').is_some() => {
                    return Ok(Some(Token::PercentEqualSign))
                }
                '%' => return Ok(Some(Token::Percent)),
                '^' if self.source.next_if_eq(&'=').is_some() => {
                    return Ok(Some(Token::CircumflexEqualSign))
                }
                '^' => return Ok(Some(Token::Circumflex)),
                '<' if self.source.next_if_eq(&'<').is_some() => {
                    return Ok(Some(if self.source.next_if_eq(&'=').is_some() {
                        Token::TwoLessThanOpsEqualSign
                    } else {
                        Token::TwoLessThanOps
                    }));
                }
                '>' if self.source.next_if_eq(&'>').is_some() => {
                    return Ok(Some(if self.source.next_if_eq(&'=').is_some() {
                        Token::TwoGreaterThanOpsEqualSign
                    } else {
                        Token::TwoGreaterThanOps
                    }));
                }
                '&' if self.source.next_if_eq(&'&').is_some() => {
                    return Ok(Some(Token::TwoAmpersands))
                }
                '&' if self.source.next_if_eq(&'=').is_some() => {
                    return Ok(Some(Token::AmpersandEqualSign))
                }
                '&' => return Ok(Some(Token::Ampersand)),
                '|' if self.source.next_if_eq(&'|').is_some() => return Ok(Some(Token::TwoBars)),
                '|' if self.source.next_if_eq(&'=').is_some() => {
                    return Ok(Some(Token::BarEqualSign))
                }
                '|' => return Ok(Some(Token::Bar)),
                '=' if self.source.next_if_eq(&'=').is_some() => {
                    return Ok(Some(Token::TwoEqualSigns))
                }
                '=' => return Ok(Some(Token::EqualSign)),
                '!' if self.source.next_if_eq(&'=').is_some() => {
                    return Ok(Some(Token::NotEqualSign))
                }
                '!' => return Ok(Some(Token::ExclamationPoint)),
                '<' if self.source.next_if_eq(&'=').is_some() => {
                    return Ok(Some(Token::LessThanOrEqualToOp))
                }
                '<' => return Ok(Some(Token::LessThanOp)),
                '>' if self.source.next_if_eq(&'=').is_some() => {
                    return Ok(Some(Token::GreaterThanOrEqualToOp))
                }
                '>' => return Ok(Some(Token::GreaterThanOp)),
                '?' => return Ok(Some(Token::QuestionMark)),
                ':' => return Ok(Some(Token::Colon)),
                ',' => return Ok(Some(Token::Comma)),
                '#' => return Ok(Some(Token::NumberSign)),
                '\"' => return Ok(Some(self.string()?)),
                _ => return Err(self.err(ErrorKind::InvalidToken(c))),
            }
        }

        Ok(None)
    }

    fn ignore_line(&mut self) {
        for c in self.source.by_ref() {
            if c == '\n' {
                self.line_number += 1;
                break;
            }
        }
    }

    fn ignore_multiline_comment(&mut self) {
        while let Some(c) = self.source.next() {
            match c {
                '*' if self.source.next_if_eq(&'/').is_some() => break,
                '\n' => self.line_number += 1,
                _ => {}
            }
        }
    }

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
            "if" => Token::Keyword(Keyword::If),
            "else" => Token::Keyword(Keyword::Else),
            "do" => Token::Keyword(Keyword::Do),
            "while" => Token::Keyword(Keyword::While),
            "for" => Token::Keyword(Keyword::For),
            "break" => Token::Keyword(Keyword::Break),
            "continue" => Token::Keyword(Keyword::Continue),
            "extern" => Token::Keyword(Keyword::Extern),
            "const" => Token::Keyword(Keyword::Const),
            "switch" => Token::Keyword(Keyword::Switch),
            "case" => Token::Keyword(Keyword::Case),
            "default" => Token::Keyword(Keyword::Default),
            _ => Token::Identifier(identifier.into()),
        }
    }

    fn hex_constant(&mut self) -> Result<i32, Error> {
        self.integer_constant(16, |c| c.is_ascii_hexdigit())
    }

    fn binary_constant(&mut self) -> Result<i32, Error> {
        self.integer_constant(2, |c| c == '0' || c == '1')
    }

    fn decimal_constant(&mut self, first: char) -> Result<Token, Error> {
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

    fn integer_constant<F: Fn(char) -> bool>(
        &mut self,
        base: u32,
        included: F,
    ) -> Result<i32, Error> {
        let mut string = String::new();

        while let Some(c) = self.source.next_if(|&c| included(c)) {
            string.push(c);
        }

        if string.is_empty() {
            return Err(self.err(ErrorKind::InvalidConstant));
        }

        let value = i32::from_str_radix(&string, base)
            .map_err(|_| self.err(ErrorKind::ConstantTooLarge))?;

        Ok(value)
    }

    fn string(&mut self) -> Result<Token, Error> {
        let mut data = Vec::new();

        while let Some(c) = self.source.next_if(|&c| c != '"') {
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
            Ok(Token::String(data))
        } else {
            Err(self.err(ErrorKind::UnterminatedString))
        }
    }

    fn err(&self, kind: ErrorKind) -> Error {
        Error {
            line_number: self.line_number,
            kind,
            severity: Severity::Error,
        }
    }
}
