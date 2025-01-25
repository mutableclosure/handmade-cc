// Copyright (c) 2025 Ludovico Rossi
// SPDX-License-Identifier: MPL-2.0

use crate::{emitter::Emitter, parser::Parser, wat::Wat, Error, ErrorKind, Severity};
use alloc::{string::String, vec::Vec};

/// Public interface to the C-to-WebAssembly compiler.
///
/// Same instance can be reused to compile multiple files.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub struct Compiler;

impl Compiler {
    /// Compiles the given C source to [WAT (WebAssembly Text) format](https://webassembly.github.io/mutable-global/core/text/index.html).
    ///
    /// Call [Self::assemble] to produce a binary from the resulting WAT.
    pub fn compile(&mut self, source: impl AsRef<str>) -> Result<String, Error> {
        let mut parser = Parser::new(source.as_ref());
        let ast = parser.parse()?;
        let module = Emitter.emit(ast);
        let wat = Wat::default().generate(module);
        Ok(wat)
    }

    /// Compiles the given WAT source to [WebAssembly Binary format](https://webassembly.github.io/mutable-global/core/binary/index.html).
    ///
    /// Call [Self::compile] to obtain a WAT source from the C source.
    #[cfg(feature = "binary-output")]
    pub fn assemble(&mut self, source: impl AsRef<str>) -> Result<Vec<u8>, Error> {
        use alloc::string::ToString;

        wat::parse_str(source).map_err(|e| Error {
            line_number: 0,
            column: 0,
            kind: ErrorKind::AssemblerError(e.to_string()),
            severity: Severity::Error,
        })
    }
}
