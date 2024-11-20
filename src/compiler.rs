use crate::Error;
use alloc::string::String;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub struct Compiler;

impl Compiler {
    pub fn compile(&mut self, source: impl AsRef<str>) -> Result<String, Error> {
        todo!()
    }
}
