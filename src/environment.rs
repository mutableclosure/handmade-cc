use crate::{
    symbol::{Symbol, Type},
    ErrorKind,
};
use alloc::{
    collections::BTreeMap,
    string::{String, ToString},
};

#[derive(Clone, Default, Debug)]
pub struct Environment {
    symbols: BTreeMap<String, Symbol>,
    level: usize,
}

impl Environment {
    pub fn declare_variable(&mut self, identifier: &str) -> Result<String, ErrorKind> {
        let name = self.make_name(identifier);

        if self.symbols.contains_key(&name) {
            Err(ErrorKind::Redefined(name))
        } else {
            let symbol = Symbol {
                r#type: Type::Variable,
            };
            self.symbols.insert(name.clone(), symbol);
            Ok(name)
        }
    }

    pub fn resolve_variable(&mut self, identifier: &str) -> Result<String, ErrorKind> {
        let name = self.make_name(identifier);

        if let Some(symbol) = self.symbols.get(&name) {
            assert!(symbol.r#type == Type::Variable);
            Ok(name)
        } else {
            Err(ErrorKind::Undeclared(name))
        }
    }
}

impl Environment {
    fn make_name(&self, identifier: &str) -> String {
        let mut name = String::with_capacity(identifier.len() + 4);
        name.push_str(identifier);
        name.push('.');
        name.push_str(&self.level.to_string());
        name
    }
}
