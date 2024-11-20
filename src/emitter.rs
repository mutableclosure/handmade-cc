use crate::{ast::Program, Error};
use alloc::string::{String, ToString};

#[derive(Clone, Debug)]
pub struct Emitter;

impl Emitter {
    pub fn emit(&self, program: &Program) -> Result<String, Error> {
        log::info!("{:?}", program);
        Ok("".to_string())
    }
}
