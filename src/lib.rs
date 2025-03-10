// Copyright (c) 2025 Ludovico Rossi
// SPDX-License-Identifier: MPL-2.0

#![no_std]
#![forbid(unsafe_code)]
#![warn(missing_docs)]
#![doc = include_str!("../README.md")]

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
mod environment;
mod error;
mod evaluator;
mod ir;
mod lexer;
mod parser;
mod token;
mod verifier;
mod wat;
