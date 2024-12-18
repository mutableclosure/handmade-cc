use alloc::{rc::Rc, string::String, vec::Vec};
use core::fmt::{self, Display, Formatter};

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Debug)]
pub struct Token {
    pub line_number: usize,
    pub column: usize,
    pub kind: TokenKind,
}

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Debug)]
pub enum TokenKind {
    Identifier(Rc<String>),
    Constant(i32),
    String(Vec<u8>),
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
    QuestionMark,
    Colon,
    Comma,
    NumberSign,
}

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Debug)]
pub enum Keyword {
    Int,
    Void,
    Return,
    If,
    Else,
    Do,
    While,
    For,
    Break,
    Continue,
    Extern,
    Const,
    Switch,
    Case,
    Default,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Identifier(identifier) => write!(f, "{identifier}"),
            TokenKind::Constant(value) => write!(f, "{value}"),
            TokenKind::String(value) => {
                write!(f, "\"{}\"", data_to_string(value))
            }
            TokenKind::Keyword(keyword) => write!(f, "{keyword}"),
            TokenKind::OpenParenthesis => write!(f, "("),
            TokenKind::CloseParenthesis => write!(f, ")"),
            TokenKind::OpenBrace => write!(f, "{{"),
            TokenKind::CloseBrace => write!(f, "}}"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Tilde => write!(f, "~"),
            TokenKind::Hyphen => write!(f, "-"),
            TokenKind::PlusSign => write!(f, "+"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Ampersand => write!(f, "&"),
            TokenKind::Bar => write!(f, "|"),
            TokenKind::Circumflex => write!(f, "^"),
            TokenKind::TwoLessThanOps => write!(f, "<<"),
            TokenKind::TwoGreaterThanOps => write!(f, ">>"),
            TokenKind::ExclamationPoint => write!(f, ""),
            TokenKind::TwoAmpersands => write!(f, "&&"),
            TokenKind::TwoBars => write!(f, "||"),
            TokenKind::TwoEqualSigns => write!(f, "=="),
            TokenKind::NotEqualSign => write!(f, "!="),
            TokenKind::LessThanOp => write!(f, "<"),
            TokenKind::GreaterThanOp => write!(f, ">"),
            TokenKind::LessThanOrEqualToOp => write!(f, "<="),
            TokenKind::GreaterThanOrEqualToOp => write!(f, ">="),
            TokenKind::EqualSign => write!(f, "="),
            TokenKind::PlusEqualSign => write!(f, "+="),
            TokenKind::HyphenEqualSign => write!(f, "-="),
            TokenKind::AsteriskEqualSign => write!(f, "*="),
            TokenKind::SlashEqualSign => write!(f, "/="),
            TokenKind::PercentEqualSign => write!(f, "%="),
            TokenKind::AmpersandEqualSign => write!(f, "&="),
            TokenKind::BarEqualSign => write!(f, "|="),
            TokenKind::CircumflexEqualSign => write!(f, "^="),
            TokenKind::TwoLessThanOpsEqualSign => write!(f, "<<="),
            TokenKind::TwoGreaterThanOpsEqualSign => write!(f, ">>="),
            TokenKind::TwoPlusSigns => write!(f, "++"),
            TokenKind::TwoHyphens => write!(f, "--"),
            TokenKind::QuestionMark => write!(f, "?"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::NumberSign => write!(f, "#"),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Keyword::Int => write!(f, "int"),
            Keyword::Void => write!(f, "void"),
            Keyword::Return => write!(f, "return"),
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::Do => write!(f, "do"),
            Keyword::While => write!(f, "while"),
            Keyword::For => write!(f, "for"),
            Keyword::Break => write!(f, "break"),
            Keyword::Continue => write!(f, "continue"),
            Keyword::Extern => write!(f, "extern"),
            Keyword::Const => write!(f, "const"),
            Keyword::Switch => write!(f, "switch"),
            Keyword::Case => write!(f, "case"),
            Keyword::Default => write!(f, "default"),
        }
    }
}

fn data_to_string(data: &[u8]) -> String {
    data.iter()
        .map(|&c| c as char)
        .collect::<String>()
        .replace('\n', "\\n")
        .replace('\t', "  ")
}
