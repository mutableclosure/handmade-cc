use crate::{ir::Module, Error};
use alloc::string::{String, ToString};

#[derive(Clone, Debug)]
pub struct Wat;

impl Wat {
    pub fn generate(&self, module: &Module) -> Result<String, Error> {
        log::info!("{:?}", module);
        Ok("".to_string())
    }
}
