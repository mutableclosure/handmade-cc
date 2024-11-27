use crate::token::Token;
use alloc::string::String;
use core::fmt::{self, Display, Formatter, Result};

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Kind {
    InvalidToken(char),
    ConstantTooLarge,
    UnknownType(Token),
    ExpectedIdentifier(Option<Token>),
    ExpectedToken(Token, Option<Token>),
    ExpectedExpression(Option<Token>),
    Redefined(String),
    Undeclared(String),
    InvalidLvalue,
    UndefinedMain,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum Severity {
    Warning,
    Error,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Error {
    pub(crate) line_number: usize,
    pub(crate) kind: Kind,
    pub(crate) severity: Severity,
}

impl Error {
    pub fn line_number(&self) -> usize {
        self.line_number
    }

    pub fn kind(&self) -> &Kind {
        &self.kind
    }

    pub fn severity(&self) -> Severity {
        self.severity
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}: {}: {}", self.line_number, self.severity, self.kind)
    }
}

impl Display for Kind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Kind::InvalidToken(found) => write!(f, "Invalid token: '{found}'"),
            Kind::ConstantTooLarge => write!(f, "Integer literal too large"),
            Kind::UnknownType(found) => write!(f, "Unknown type: '{found}'"),
            Kind::ExpectedIdentifier(found) => {
                write!(f, "Expected identifier")?;
                write_found(f, found)
            }
            Kind::ExpectedToken(token, found) => {
                write!(f, "Expected '{token}'")?;
                write_found(f, found)
            }
            Kind::ExpectedExpression(found) => {
                write!(f, "Expected expression")?;
                write_found(f, found)
            }
            Kind::Redefined(identifier) => write!(f, "Redefinition of '{identifier}'"),
            Kind::Undeclared(identifier) => write!(f, "'{identifier}' undeclared"),
            Kind::InvalidLvalue => write!(f, "'Invalid lvalue as left operand of assignment"),
            Kind::UndefinedMain => write!(f, "Undefined reference to 'main'"),
        }
    }
}

impl Display for Severity {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Severity::Warning => write!(f, "warning"),
            Severity::Error => write!(f, "error"),
        }
    }
}

fn write_found(f: &mut Formatter<'_>, found: &Option<Token>) -> fmt::Result {
    found
        .as_ref()
        .map(|found| write!(f, ", found: '{found}'"))
        .unwrap_or(Ok(()))
}
