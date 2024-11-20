#![no_std]

extern crate alloc;

pub use compiler::Compiler;
pub use error::{Error, Kind as ErrorKind, Severity};

mod compiler;
mod error;
