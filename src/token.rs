use alloc::string::String;
use core::fmt::{self, Display, Formatter};

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Token {
    Identifier(String),
    Constant(i32),
    Keyword(Keyword),
    OpenParenthesis,
    CloseParenthesis,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Tilde,
    Hyphen,
    Plus,
    Asterisk,
    Slash,
    Percent,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Keyword {
    Int,
    Void,
    Return,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::Identifier(identifier) => write!(f, "{identifier}"),
            Token::Constant(value) => write!(f, "{value}"),
            Token::Keyword(keyword) => write!(f, "{keyword}"),
            Token::OpenParenthesis => write!(f, "("),
            Token::CloseParenthesis => write!(f, ")"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::Semicolon => write!(f, ";"),
            Token::Tilde => write!(f, "~"),
            Token::Hyphen => write!(f, "-"),
            Token::Plus => write!(f, "+"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Keyword::Int => write!(f, "int"),
            Keyword::Void => write!(f, "void"),
            Keyword::Return => write!(f, "return"),
        }
    }
}
