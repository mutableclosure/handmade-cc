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
    PlusSign,
    Asterisk,
    Slash,
    Percent,
    Ampersand,
    Bar,
    Circumflex,
    TwoLessThanOps,
    TwoGreaterThanOps,
    ExclamationPoint,
    TwoAmpersands,
    TwoBars,
    TwoEqualSigns,
    NotEqualSign,
    LessThanOp,
    GreaterThanOp,
    LessThanOrEqualToOp,
    GreaterThanOrEqualToOp,
    EqualSign,
    PlusEqualSign,
    HyphenEqualSign,
    AsteriskEqualSign,
    SlashEqualSign,
    PercentEqualSign,
    AmpersandEqualSign,
    BarEqualSign,
    CircumflexEqualSign,
    TwoLessThanOpsEqualSign,
    TwoGreaterThanOpsEqualSign,
    TwoPlusSigns,
    TwoHyphens,
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
            Token::PlusSign => write!(f, "+"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Ampersand => write!(f, "&"),
            Token::Bar => write!(f, "|"),
            Token::Circumflex => write!(f, "^"),
            Token::TwoLessThanOps => write!(f, "<<"),
            Token::TwoGreaterThanOps => write!(f, ">>"),
            Token::ExclamationPoint => write!(f, ""),
            Token::TwoAmpersands => write!(f, "&&"),
            Token::TwoBars => write!(f, "||"),
            Token::TwoEqualSigns => write!(f, "=="),
            Token::NotEqualSign => write!(f, "!="),
            Token::LessThanOp => write!(f, "<"),
            Token::GreaterThanOp => write!(f, ">"),
            Token::LessThanOrEqualToOp => write!(f, "<="),
            Token::GreaterThanOrEqualToOp => write!(f, ">="),
            Token::EqualSign => write!(f, "="),
            Token::PlusEqualSign => write!(f, "+="),
            Token::HyphenEqualSign => write!(f, "-="),
            Token::AsteriskEqualSign => write!(f, "*="),
            Token::SlashEqualSign => write!(f, "/="),
            Token::PercentEqualSign => write!(f, "%="),
            Token::AmpersandEqualSign => write!(f, "&="),
            Token::BarEqualSign => write!(f, "|="),
            Token::CircumflexEqualSign => write!(f, "^="),
            Token::TwoLessThanOpsEqualSign => write!(f, "<<="),
            Token::TwoGreaterThanOpsEqualSign => write!(f, ">>="),
            Token::TwoPlusSigns => write!(f, "++"),
            Token::TwoHyphens => write!(f, "--"),
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
