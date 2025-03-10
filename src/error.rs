// Copyright (c) 2025 Ludovico Rossi
// SPDX-License-Identifier: MPL-2.0

use crate::token::TokenKind;
use alloc::{rc::Rc, string::String};
use core::fmt::{self, Display, Formatter, Result};

/// Kind of error encountered during compilation.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Kind {
    /// Invalid token, with the given offending character.
    InvalidToken(char),
    /// Invalid constant value.
    InvalidConstant,
    /// Constant value exceeded the capacity of a 32-bit `int`.
    ConstantTooLarge,
    /// Unknown type encountered, with the given token.
    UnknownType(TokenKind),
    /// Expected a type but none was found.
    ExpectedType,
    /// Expected an identifier but the given token was found instead (or `None` on EOF).
    ExpectedIdentifier(Option<TokenKind>),
    /// Expected a specific token but another given token was found instead (or `None` on EOF).
    ExpectedToken(TokenKind, Option<TokenKind>),
    /// Expected an expression but the given token was found instead (or `None` on EOF).
    ExpectedExpression(Option<TokenKind>),
    /// Encountered a redefinition of an identifier.
    Redefinition(Rc<String>),
    /// Attempted to use an undeclared identifier.
    Undeclared(Rc<String>),
    /// Conflicting type definitions for the same identifier.
    ConflictingTypes(Rc<String>),
    /// Invalid left-hand value in an assignment.
    InvalidLvalue,
    /// `break` outside of a loop or `switch` statement.
    BreakOutsideLoopOrSwitch,
    /// `continue` outside of a loop.
    ContinueOutsideLoop,
    /// Attempted to call an undefined function.
    UndefinedFunction(Rc<String>),
    /// Expression expected to be constant but wasn't.
    NonConstantExpression,
    /// Expression expected to be an integer but wasn't.
    NonIntegerExpression,
    /// Attempted to assign to a `const` variable.
    CannotAssignToConst(Rc<String>),
    /// Duplicate `case` value in a `switch` statement.
    DuplicateCase,
    /// Invalid preprocessor directive.
    InvalidDirective(Rc<String>),
    /// String literal was not properly terminated.
    UnterminatedString,
    /// Invalid character.
    InvalidCharacter(char),
    /// Invalid escape sequence in a string literal.
    InvalidEscapeSequence,
    /// Expected a string literal but the given token was found instead (or `None` on EOF).
    ExpectedString(Option<TokenKind>),
    /// Attempted to use type that may be valid in C but it's currently unsupported.
    UnsupportedType,
    /// Attempted to use a feature that may be valid in C but it's currently unsupported.
    UnsupportedFeature(TokenKind),
    /// Attempted to use array syntax but arrays are currently unsupported.
    ArraysUnsupported,
    /// Error emitted by the [`wat`](https://crates.io/crates/wat) crate when compiling the WAT source to binary.
    ///
    /// Note: Likely indicates a bug in this compiler!
    AssemblerError(String),
}

/// Severity level of an error.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum Severity {
    /// An error that halts compilation.
    Error,
}

/// Compilation error or warning, including its location and context.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Error {
    /// The line number in the C source where the error occurred.
    pub(crate) line_number: usize,
    /// The column number in the C source where the error occurred.
    pub(crate) column: usize,
    /// The kind of error encountered.
    pub(crate) kind: Kind,
    /// The severity of the error.
    pub(crate) severity: Severity,
}

impl Error {
    /// Returns the line number in the C source where the error occurred.
    pub fn line_number(&self) -> usize {
        self.line_number
    }

    /// Returns the column number in the C source where the error occurred.
    pub fn column(&self) -> usize {
        self.column
    }

    /// Returns the error kind.
    pub fn kind(&self) -> &Kind {
        &self.kind
    }

    /// Returns the error severity level.
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
            Kind::InvalidToken(found) => write!(f, "invalid token: '{found}'"),
            Kind::InvalidConstant => write!(f, "invalid integer constant"),
            Kind::ConstantTooLarge => write!(f, "integer literal too large"),
            Kind::UnknownType(found) => write!(f, "unknown type: '{found}'"),
            Kind::ExpectedType => write!(f, "expected type"),
            Kind::ExpectedIdentifier(found) => {
                write!(f, "expected identifier")?;
                write_found(f, found)
            }
            Kind::ExpectedToken(token, found) => {
                write!(f, "expected '{token}'")?;
                write_found(f, found)
            }
            Kind::ExpectedExpression(found) => {
                write!(f, "expected expression")?;
                write_found(f, found)
            }
            Kind::Redefinition(identifier) => write!(f, "redefinition of '{identifier}'"),
            Kind::Undeclared(identifier) => write!(f, "'{identifier}' undeclared"),
            Kind::ConflictingTypes(identifier) => write!(f, "conflicting types for '{identifier}'"),
            Kind::InvalidLvalue => write!(f, "'invalid lvalue"),
            Kind::BreakOutsideLoopOrSwitch => {
                write!(f, "'break' statement not within a loop or switch")
            }
            Kind::ContinueOutsideLoop => write!(f, "'continue' statement not within a loop"),
            Kind::UndefinedFunction(name) => write!(f, "undefined reference to '{name}'"),
            Kind::NonConstantExpression => write!(f, "expression is not constant"),
            Kind::NonIntegerExpression => write!(f, "expression has non-integer type"),
            Kind::CannotAssignToConst(name) => {
                write!(f, "cannot assign to const-qualified variable '{name}'")
            }
            Kind::DuplicateCase => write!(f, "duplicate case in switch statement"),
            Kind::InvalidDirective(name) => write!(f, "invalid directive: '{name}'"),
            Kind::UnterminatedString => write!(f, "unterminated string"),
            Kind::InvalidCharacter(c) => write!(f, "invalid character: '{c}'"),
            Kind::InvalidEscapeSequence => write!(f, "invalid escape sequence"),
            Kind::ExpectedString(found) => {
                write!(f, "expected string")?;
                write_found(f, found)
            }
            Kind::UnsupportedType => write!(f, "only 'int' and 'void' types are supported"),
            Kind::UnsupportedFeature(token) => write!(f, "'{token}' is unsupported"),
            Kind::ArraysUnsupported => write!(f, "arrays are unsupported"),
            Kind::AssemblerError(error) => write!(f, "{error}"),
        }
    }
}

impl Display for Severity {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Severity::Error => write!(f, "error"),
        }
    }
}

fn write_found(f: &mut Formatter<'_>, found: &Option<TokenKind>) -> fmt::Result {
    found
        .as_ref()
        .map(|found| write!(f, ", found: '{found}'"))
        .unwrap_or(Ok(()))
}
