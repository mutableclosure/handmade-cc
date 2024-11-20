use core::fmt::{Display, Formatter, Result};

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum Kind {}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum Severity {
    Warning,
    Error,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct Error {
    pub(crate) line_number: usize,
    pub(crate) kind: Kind,
    pub(crate) severity: Severity,
}

impl Error {
    pub fn line_number(&self) -> usize {
        self.line_number
    }

    pub fn kind(&self) -> Kind {
        self.kind
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
        write!(f, "")
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
