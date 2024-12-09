use crate::{emitter::Emitter, parser::Parser, wat::Wat, Error, ErrorKind, Severity};
use alloc::{string::String, vec::Vec};

#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub struct Compiler;

impl Compiler {
    pub fn compile(&mut self, source: impl AsRef<str>) -> Result<String, Error> {
        let mut parser = Parser::new(source.as_ref());
        let ast = parser.parse()?;
        let module = Emitter.emit(ast);
        let wat = Wat::default().generate(module);
        Ok(wat)
    }

    #[cfg(feature = "binary-output")]
    pub fn assemble(&mut self, source: impl AsRef<str>) -> Result<Vec<u8>, Error> {
        use alloc::string::ToString;

        wat::parse_str(source).map_err(|e| Error {
            line_number: 0,
            kind: ErrorKind::AssemblerError(e.to_string()),
            severity: Severity::Error,
        })
    }
}
