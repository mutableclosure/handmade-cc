use crate::{
    symbol::{Symbol, Type},
    ErrorKind,
};
use alloc::{
    collections::BTreeMap,
    string::{String, ToString},
    vec::Vec,
};

const LOOP_LABEL: &str = "loop";

#[derive(Clone, Default, Debug)]
pub struct Environment {
    symbols: Vec<BTreeMap<String, Symbol>>,
    loop_labels: Vec<String>,
}

impl Environment {
    pub fn nest(&mut self) {
        self.symbols.push(BTreeMap::default());
    }

    pub fn unnest(&mut self) {
        self.symbols.pop();
    }

    pub fn enter_loop(&mut self) -> &str {
        let label = make_name(LOOP_LABEL, self.loop_labels.len());
        self.loop_labels.push(label);
        self.loop_labels.last().unwrap()
    }

    pub fn loop_label(&self) -> Option<&str> {
        self.loop_labels.last().map(|s| s.as_str())
    }

    pub fn exit_loop(&mut self) {
        self.loop_labels.pop();
    }

    pub fn declare_variable(&mut self, identifier: &str) -> Result<String, ErrorKind> {
        let name = make_name(identifier, self.symbols.len());
        let symbols = self.symbols.last_mut().unwrap();

        if symbols.contains_key(&name) {
            Err(ErrorKind::Redefined(name))
        } else {
            let symbol = Symbol {
                r#type: Type::Variable,
            };
            symbols.insert(name.clone(), symbol);
            Ok(name)
        }
    }

    pub fn resolve_variable(&self, identifier: &str) -> Result<String, ErrorKind> {
        for (level, symbols) in self.symbols.iter().enumerate().rev() {
            let name = make_name(identifier, level + 1);
            if let Some(symbol) = symbols.get(&name) {
                assert!(symbol.r#type == Type::Variable);
                return Ok(name);
            }
        }

        Err(ErrorKind::Undeclared(identifier.to_string()))
    }
}

fn make_name(identifier: &str, level: usize) -> String {
    let mut name = String::with_capacity(identifier.len() + 4);
    name.push_str(identifier);
    name.push('.');
    name.push_str(&level.to_string());
    name
}
