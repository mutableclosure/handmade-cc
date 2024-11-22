#![no_std]

#[macro_use]
extern crate alloc;

#[allow(unused_imports)]
#[macro_use]
extern crate log;

pub use compiler::Compiler;
pub use error::{Error, Kind as ErrorKind, Severity};

mod ast;
mod compiler;
mod emitter;
mod error;
mod ir;
mod lexer;
mod parser;
mod token;
mod wat;
