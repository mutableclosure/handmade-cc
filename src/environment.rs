use crate::{
    ast::{FunctionParameter, Lvalue, Type},
    ErrorKind,
};
use alloc::{
    collections::{btree_map::Entry, BTreeMap},
    rc::Rc,
    string::{String, ToString},
    vec::Vec,
};

const LOOP_LABEL: &str = "loop";

#[derive(Clone, Debug)]
pub enum Symbol {
    Global(GlobalDeclarationType),
    Variable,
    Function(Function),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum GlobalDeclarationType {
    Tentative,
    NonTentative,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub parameters: BTreeMap<Rc<String>, Type>,
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
    symbols: Vec<BTreeMap<Rc<String>, Symbol>>,
    function: Option<Rc<String>>,
    loop_labels: Vec<Rc<String>>,
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

    pub fn enter_function(&mut self, name: Rc<String>) {
        assert!(self.resolve_function(name.clone()).is_some());
        self.function = Some(name);
    }

    pub fn exit_function(&mut self) {
        self.function = None;
    }

    pub fn enter_loop(&mut self) -> Rc<String> {
        let label = make_name(LOOP_LABEL, self.loop_labels.len());
        self.loop_labels.push(label);
        self.loop_labels.last().unwrap().clone()
    }

    pub fn loop_label(&self) -> Option<Rc<String>> {
        self.loop_labels.last().cloned()
    }

    pub fn exit_loop(&mut self) {
        self.loop_labels.pop();
    }

    pub fn declare_function(
        &mut self,
        name: Rc<String>,
        parameters: Vec<FunctionParameter>,
        return_type: Type,
        declaration_type: FunctionDeclarationType,
    ) -> Result<(), ErrorKind> {
        let symbols = self.symbols.last_mut().unwrap();
        match symbols.entry(name) {
            Entry::Vacant(entry) => {
                entry.insert(Symbol::Function(Function {
                    parameters: to_parameters(parameters),
                    return_type,
                    declaration_type,
                }));
                Ok(())
            }
            Entry::Occupied(mut entry) => {
                if let Symbol::Function(function) = entry.get_mut() {
                    match function.declaration_type {
                        FunctionDeclarationType::ForwardDeclaration => {
                            if return_type != function.return_type
                                || !parameters_match(&parameters, &function.parameters)
                            {
                                return Err(ErrorKind::ConflictingTypes(entry.key().clone()));
                            }

                            if declaration_type == FunctionDeclarationType::Definition {
                                function.declaration_type = declaration_type;
                                function.parameters = to_parameters(parameters);
                            }

                            return Ok(());
                        }
                        FunctionDeclarationType::ExternDeclaration
                        | FunctionDeclarationType::Definition => {}
                    }
                }

                Err(ErrorKind::Redefined(entry.key().clone()))
            }
        }
    }

    pub fn declare_global(
        &mut self,
        identifier: Rc<String>,
        declaration_type: GlobalDeclarationType,
    ) -> Result<Rc<String>, ErrorKind> {
        let symbols = self.symbols.last_mut().unwrap();

        match symbols.get(&identifier) {
            Some(Symbol::Global(GlobalDeclarationType::Tentative)) | None => {
                symbols.insert(identifier.clone(), Symbol::Global(declaration_type));
                Ok(identifier)
            }
            Some(Symbol::Global(GlobalDeclarationType::NonTentative))
                if matches!(declaration_type, GlobalDeclarationType::Tentative) =>
            {
                Ok(identifier)
            }
            Some(Symbol::Global(_)) | Some(Symbol::Variable) | Some(Symbol::Function(_)) => {
                Err(ErrorKind::Redefined(identifier))
            }
        }
    }

    pub fn declare_variable(&mut self, identifier: Rc<String>) -> Result<Rc<String>, ErrorKind> {
        if let Some(f) = self
            .function
            .as_ref()
            .and_then(|f| self.resolve_function(f.clone()))
        {
            if f.parameters.contains_key(&identifier) {
                return Err(ErrorKind::Redefined(identifier));
            }
        }

        let name = make_name(&identifier, self.symbols.len());
        let symbols = self.symbols.last_mut().unwrap();

        if symbols.contains_key(&name) {
            Err(ErrorKind::Redefined(name))
        } else {
            symbols.insert(name.clone(), Symbol::Variable);
            Ok(name)
        }
    }

    pub fn resolve_lvalue(&self, identifier: Rc<String>) -> Result<Lvalue, ErrorKind> {
        let (name, symbol) = self.resolve_symbol(identifier)?;

        match symbol {
            Symbol::Global(_) => Ok(Lvalue::Global(name)),
            Symbol::Variable => Ok(Lvalue::Variable(name)),
            Symbol::Function(_) => Err(ErrorKind::InvalidLvalue),
        }
    }

    pub fn is_function_defined(&self, identifier: Rc<String>) -> bool {
        self.resolve_function(identifier)
            .map(|f| match f.declaration_type {
                FunctionDeclarationType::ExternDeclaration
                | FunctionDeclarationType::Definition => true,
                FunctionDeclarationType::ForwardDeclaration => false,
            })
            .unwrap_or_default()
    }

    pub fn resolve_symbol(
        &self,
        identifier: Rc<String>,
    ) -> Result<(Rc<String>, &Symbol), ErrorKind> {
        for (level, symbols) in self.symbols.iter().enumerate().rev() {
            if let Some(Symbol::Function(f)) = self.function.as_ref().and_then(|f| symbols.get(f)) {
                if let Some(r#type) = f.parameters.get(&identifier) {
                    assert!(matches!(r#type, Type::Int));
                    return Ok((identifier, &Symbol::Variable));
                }
            }

            if let Some(symbol) = symbols.get(&identifier) {
                return Ok((identifier, symbol));
            }

            let name = make_name(&identifier, level + 1);
            if let Some(symbol) = symbols.get(&name) {
                return Ok((name, symbol));
            }
        }

        Err(ErrorKind::Undeclared(identifier.clone()))
    }
}

impl Environment {
    fn resolve_function(&self, name: Rc<String>) -> Option<&Function> {
        if let Ok((_, Symbol::Function(function))) = self.resolve_symbol(name) {
            Some(function)
        } else {
            None
        }
    }
}

fn make_name(identifier: &str, level: usize) -> Rc<String> {
    let mut name = String::with_capacity(identifier.len() + 4);
    name.push_str(identifier);
    name.push('.');
    name.push_str(&level.to_string());
    name.into()
}

fn to_parameters(parameters: Vec<FunctionParameter>) -> BTreeMap<Rc<String>, Type> {
    BTreeMap::from_iter(parameters.into_iter().map(|p| (p.name, p.r#type)))
}

fn parameters_match(found: &[FunctionParameter], expected: &BTreeMap<Rc<String>, Type>) -> bool {
    found.iter().map(|p| &p.r#type).eq(expected.values())
}
