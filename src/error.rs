use crate::token::Token;
use alloc::{rc::Rc, string::String};
use core::fmt::{self, Display, Formatter, Result};

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Kind {
    InvalidToken(char),
    ConstantTooLarge,
    UnknownType(Token),
    ExpectedType,
    ExpectedIdentifier(Option<Token>),
    ExpectedToken(Token, Option<Token>),
    ExpectedExpression(Option<Token>),
    Redefined(Rc<String>),
    Undeclared(Rc<String>),
    ConflictingTypes(Rc<String>),
    InvalidLvalue,
    BreakOutsideLoopOrSwitch,
    ContinueOutsideLoop,
    UndefinedFunction(Rc<String>),
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
            Kind::ExpectedType => write!(f, "Expected type"),
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
            Kind::ConflictingTypes(identifier) => write!(f, "Conflicting types for '{identifier}'"),
            Kind::InvalidLvalue => write!(f, "'Invalid lvalue"),
            Kind::BreakOutsideLoopOrSwitch => {
                write!(f, "'break' statement not within a loop or switch")
            }
            Kind::ContinueOutsideLoop => write!(f, "'continue' statement not within a loop"),
            Kind::UndefinedFunction(name) => write!(f, "Undefined reference to '{name}'"),
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
