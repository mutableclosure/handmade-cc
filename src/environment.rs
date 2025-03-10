// Copyright (c) 2025 Ludovico Rossi
// SPDX-License-Identifier: MPL-2.0

use crate::{
    ast::{ConstQualifier, FunctionParameter, Lvalue, Type},
    ErrorKind,
};
use alloc::{
    collections::{btree_map::Entry, BTreeMap},
    rc::Rc,
    string::{String, ToString},
    vec::Vec,
};

const LOOP_LABEL: &str = "loop";
const SWITCH_LABEL: &str = "switch";
const CASE_LABEL: &str = "case";

#[derive(Clone, Debug)]
pub enum Symbol {
    Global(ConstQualifier, GlobalDeclarationType),
    Variable(ConstQualifier),
    Function(Function),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum GlobalDeclarationType {
    Tentative,
    NonTentative(Option<i32>),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub parameters: BTreeMap<Rc<String>, (Type, ConstQualifier)>,
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
    labels: Vec<Label>,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum LabelContext {
    Loop,
    Switch,
    Case,
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Label {
    name: Rc<String>,
    context: LabelContext,
}

impl Default for Environment {
    fn default() -> Self {
        let mut environment = Self {
            symbols: Default::default(),
            function: Default::default(),
            labels: Default::default(),
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

    pub fn function(&self) -> Option<&Function> {
        self.function.clone().and_then(|f| self.resolve_function(f))
    }

    pub fn exit_function(&mut self) {
        self.function = None;
    }

    pub fn enter_loop(&mut self) -> Rc<String> {
        self.push_label(LabelContext::Loop)
    }

    pub fn loop_label(&self) -> Option<Rc<String>> {
        self.labels
            .iter()
            .rev()
            .find_map(|l| (l.context == LabelContext::Loop).then(|| l.name.clone()))
    }

    pub fn loop_or_switch_label(&self) -> Option<Rc<String>> {
        self.labels.iter().rev().find_map(|l| {
            (l.context == LabelContext::Loop || l.context == LabelContext::Switch)
                .then(|| l.name.clone())
        })
    }

    pub fn exit_loop(&mut self) {
        assert_eq!(
            self.labels.last().map(|l| l.context),
            Some(LabelContext::Loop)
        );
        self.labels.pop();
    }

    pub fn enter_switch(&mut self) -> Rc<String> {
        self.push_label(LabelContext::Switch)
    }

    pub fn case(&mut self) -> Rc<String> {
        self.push_label(LabelContext::Case)
    }

    pub fn exit_switch(&mut self) {
        while let Some(label) = self.labels.pop() {
            match label.context {
                LabelContext::Loop => unreachable!(),
                LabelContext::Switch => break,
                LabelContext::Case => {}
            }
        }
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

                Err(ErrorKind::Redefinition(entry.key().clone()))
            }
        }
    }

    pub fn declare_global(
        &mut self,
        identifier: Rc<String>,
        r#const: ConstQualifier,
        declaration_type: GlobalDeclarationType,
    ) -> Result<Rc<String>, ErrorKind> {
        let symbols = self.symbols.last_mut().unwrap();

        match symbols.get(&identifier) {
            None => {
                symbols.insert(
                    identifier.clone(),
                    Symbol::Global(r#const, declaration_type),
                );
                Ok(identifier)
            }
            Some(Symbol::Global(existing_const, GlobalDeclarationType::Tentative))
                if r#const == *existing_const =>
            {
                symbols.insert(
                    identifier.clone(),
                    Symbol::Global(r#const, declaration_type),
                );
                Ok(identifier)
            }
            Some(Symbol::Global(existing_const, GlobalDeclarationType::NonTentative(_)))
                if declaration_type == GlobalDeclarationType::Tentative
                    && r#const == *existing_const =>
            {
                Ok(identifier)
            }
            Some(Symbol::Global(_, _)) | Some(Symbol::Variable(_)) | Some(Symbol::Function(_)) => {
                Err(ErrorKind::Redefinition(identifier))
            }
        }
    }

    pub fn declare_variable(
        &mut self,
        identifier: Rc<String>,
        r#const: ConstQualifier,
    ) -> Result<Rc<String>, ErrorKind> {
        if let Some(f) = self
            .function
            .as_ref()
            .and_then(|f| self.resolve_function(f.clone()))
        {
            if f.parameters.contains_key(&identifier) {
                return Err(ErrorKind::Redefinition(identifier));
            }
        }

        let name = make_name(&identifier, self.symbols.len());
        let symbols = self.symbols.last_mut().unwrap();

        if symbols.contains_key(&name) {
            Err(ErrorKind::Redefinition(name))
        } else {
            symbols.insert(name.clone(), Symbol::Variable(r#const));
            Ok(name)
        }
    }

    pub fn resolve_lvalue(&self, identifier: Rc<String>) -> Result<Lvalue, ErrorKind> {
        let (name, symbol) = self.resolve_symbol(identifier)?;

        match symbol {
            Symbol::Global(ConstQualifier::NonConst, _) => Ok(Lvalue::Global(name)),
            Symbol::Variable(ConstQualifier::NonConst) => Ok(Lvalue::Variable(name)),
            Symbol::Global(ConstQualifier::Const, _)
            | Symbol::Variable(ConstQualifier::Const)
            | Symbol::Function(_) => Err(ErrorKind::InvalidLvalue),
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
                if let Some((r#type, r#const)) = f.parameters.get(&identifier) {
                    assert!(matches!(r#type, Type::Int));
                    return Ok((
                        identifier,
                        match r#const {
                            ConstQualifier::NonConst => &Symbol::Variable(ConstQualifier::NonConst),
                            ConstQualifier::Const => &Symbol::Variable(ConstQualifier::Const),
                        },
                    ));
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
    fn push_label(&mut self, context: LabelContext) -> Rc<String> {
        let name_prefix = match context {
            LabelContext::Loop => LOOP_LABEL,
            LabelContext::Switch => SWITCH_LABEL,
            LabelContext::Case => CASE_LABEL,
        };
        let name = make_name(name_prefix, self.labels.len());
        let label = Label { name, context };
        self.labels.push(label);
        self.labels.last().map(|l| l.name.clone()).unwrap()
    }

    fn resolve_function(&self, name: Rc<String>) -> Option<&Function> {
        for symbols in self.symbols.iter().rev() {
            if let Some(Symbol::Function(function)) = symbols.get(&name) {
                return Some(function);
            }
        }

        None
    }
}

fn make_name(identifier: &str, level: usize) -> Rc<String> {
    let mut name = String::with_capacity(identifier.len() + 4);
    name.push_str(identifier);
    name.push('.');
    name.push_str(&level.to_string());
    name.into()
}

fn to_parameters(
    parameters: Vec<FunctionParameter>,
) -> BTreeMap<Rc<String>, (Type, ConstQualifier)> {
    BTreeMap::from_iter(
        parameters
            .into_iter()
            .map(|p| (p.name, (p.r#type, p.r#const))),
    )
}

fn parameters_match(
    found: &[FunctionParameter],
    expected: &BTreeMap<Rc<String>, (Type, ConstQualifier)>,
) -> bool {
    found
        .iter()
        .map(|p| &p.r#type)
        .eq(expected.values().map(|(t, _)| t))
}
