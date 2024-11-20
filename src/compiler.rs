use crate::{emitter::Emitter, parser::Parser, Error};
use alloc::string::String;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub struct Compiler;

impl Compiler {
    pub fn compile(&mut self, source: impl AsRef<str>) -> Result<String, Error> {
        let mut parser = Parser::new(source.as_ref());
        let ast = parser.parse()?;
        let wat = Emitter.emit(&ast)?;
        Ok(wat)
    }
}
