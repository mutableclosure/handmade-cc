use crate::{emitter::Emitter, parser::Parser, wat::Wat, Error};
use alloc::string::String;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub struct Compiler;

impl Compiler {
    pub fn compile(&mut self, source: impl AsRef<str>) -> Result<String, Error> {
        let mut parser = Parser::new(source.as_ref());
        let ast = parser.parse()?;
        let module = Emitter.emit(ast)?;
        let wat = Wat.generate(&module)?;
        Ok(wat)
    }
}
