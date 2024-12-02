use crate::{
    ast::{FunctionParameter, Type},
    ErrorKind,
};
use alloc::{
    borrow::Cow,
    collections::{btree_map::Entry, BTreeMap},
    string::{String, ToString},
    vec::Vec,
};

const LOOP_LABEL: &str = "loop";

#[derive(Clone, Debug)]
pub enum Symbol {
    Variable,
    Function(Function),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub parameters: BTreeMap<String, Type>,
    pub return_type: Type,
    pub declaration_type: FunctionDeclarationType,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum FunctionDeclarationType {
    ExternDeclaration,
    ForwardDeclaration,
    Definition,
}

#[derive(Clone, Debug)]
pub struct Environment {
    symbols: Vec<BTreeMap<String, Symbol>>,
    function: Option<String>,
    loop_labels: Vec<String>,
}

impl Default for Environment {
    fn default() -> Self {
        let mut environment = Self {
            symbols: Default::default(),
            function: Default::default(),
            loop_labels: Default::default(),
        };
        environment.nest();
        environment
    }
}

impl Environment {
    pub fn nest(&mut self) {
        self.symbols.push(BTreeMap::default());
    }

    pub fn unnest(&mut self) {
        self.symbols.pop();
    }

    pub fn enter_function(&mut self, name: String) {
        assert!(self.resolve_function(&name).is_some());
        self.function = Some(name);
    }

    pub fn exit_function(&mut self) {
        self.function = None;
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

    pub fn declare_function(
        &mut self,
        name: String,
        parameters: Vec<FunctionParameter>,
        return_type: Type,
        declaration_type: FunctionDeclarationType,
    ) -> Result<(), ErrorKind> {
        let symbols = self.symbols.last_mut().unwrap();
        match symbols.entry(name) {
            Entry::Vacant(entry) => {
                let parameters =
                    BTreeMap::from_iter(parameters.into_iter().map(|p| (p.name, p.r#type)));
                let function = Function {
                    parameters,
                    return_type,
                    declaration_type,
                };
                entry.insert(Symbol::Function(function));
                Ok(())
            }
            Entry::Occupied(mut entry) => {
                if let Symbol::Function(function) = entry.get_mut() {
                    match function.declaration_type {
                        FunctionDeclarationType::ForwardDeclaration => {
                            if return_type != function.return_type
                                || !parameters
                                    .iter()
                                    .map(|p| &p.r#type)
                                    .eq(function.parameters.values())
                            {
                                return Err(ErrorKind::ConflictingTypes(entry.key().to_string()));
                            }

                            if declaration_type == FunctionDeclarationType::Definition {
                                function.declaration_type = declaration_type;
                                function.parameters = BTreeMap::from_iter(
                                    parameters.into_iter().map(|p| (p.name, p.r#type)),
                                );
                            }

                            return Ok(());
                        }
                        FunctionDeclarationType::ExternDeclaration
                        | FunctionDeclarationType::Definition => {}
                    }
                }

                Err(ErrorKind::Redefined(entry.key().to_string()))
            }
        }
    }

    pub fn declare_variable(&mut self, identifier: &str) -> Result<String, ErrorKind> {
        if let Some(f) = self
            .function
            .as_ref()
            .and_then(|f| self.resolve_function(f))
        {
            if f.parameters.contains_key(identifier) {
                return Err(ErrorKind::Redefined(identifier.to_string()));
            }
        }

        let name = make_name(identifier, self.symbols.len());
        let symbols = self.symbols.last_mut().unwrap();

        if symbols.contains_key(&name) {
            Err(ErrorKind::Redefined(name))
        } else {
            symbols.insert(name.clone(), Symbol::Variable);
            Ok(name)
        }
    }

    pub fn resolve_lvalue<'a>(&'a self, identifier: &'a str) -> Result<Cow<'a, str>, ErrorKind> {
        let (name, symbol) = self.resolve_symbol(identifier)?;

        if matches!(symbol, Symbol::Variable) {
            Ok(name)
        } else {
            Err(ErrorKind::InvalidLvalue)
        }
    }

    pub fn is_function_defined(&self, identifier: &str) -> bool {
        self.resolve_function(identifier)
            .map(|f| match f.declaration_type {
                FunctionDeclarationType::ExternDeclaration
                | FunctionDeclarationType::Definition => true,
                FunctionDeclarationType::ForwardDeclaration => false,
            })
            .unwrap_or_default()
    }

    pub fn resolve_symbol<'a>(
        &'a self,
        identifier: &'a str,
    ) -> Result<(Cow<'a, str>, &Symbol), ErrorKind> {
        for (level, symbols) in self.symbols.iter().enumerate().rev() {
            if let Some(Symbol::Function(f)) = self.function.as_ref().and_then(|f| symbols.get(f)) {
                if let Some(r#type) = f.parameters.get(identifier) {
                    assert!(matches!(r#type, Type::Int));
                    return Ok((identifier.into(), &Symbol::Variable));
                }
            }

            if let Some(symbol) = symbols.get(identifier) {
                return Ok((identifier.into(), symbol));
            }

            let name = make_name(identifier, level + 1);
            if let Some(symbol) = symbols.get(&name) {
                return Ok((name.into(), symbol));
            }
        }

        Err(ErrorKind::Undeclared(identifier.to_string()))
    }
}

impl Environment {
    fn resolve_function<'a>(&'a self, name: &'a str) -> Option<&'a Function> {
        if let Ok((_, Symbol::Function(function))) = self.resolve_symbol(name) {
            Some(function)
        } else {
            None
        }
    }
}

fn make_name(identifier: &str, level: usize) -> String {
    let mut name = String::with_capacity(identifier.len() + 4);
    name.push_str(identifier);
    name.push('.');
    name.push_str(&level.to_string());
    name
}
