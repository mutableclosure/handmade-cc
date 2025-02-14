// Copyright (c) 2025 Ludovico Rossi
// SPDX-License-Identifier: MPL-2.0

use alloc::{rc::Rc, string::String, vec::Vec};

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Module {
    pub globals: Vec<Global>,
    pub external_functions: Vec<ExternalFunction>,
    pub functions: Vec<Function>,
    pub data: Vec<Datum>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Global {
    pub name: Rc<String>,
    pub r#type: Type,
    pub is_constant: bool,
    pub value: Option<i32>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Function {
    pub name: Rc<String>,
    pub return_type: Type,
    pub parameters: Vec<Variable>,
    pub local_variables: Vec<Variable>,
    pub instructions: Vec<Instruction>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct ExternalFunction {
    pub name: Rc<String>,
    pub return_type: Type,
    pub parameters: Vec<Variable>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Variable {
    pub name: Rc<String>,
    pub r#type: Type,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Instruction {
    PushConstant(i32),
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    ShiftLeft,
    ShiftRight,
    Eq,
    Eqz,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    If,
    IfWithResult,
    Else,
    Select,
    Return,
    GlobalGet(Rc<String>),
    GlobalSet(Rc<String>),
    LocalGet(Rc<String>),
    LocalSet(Rc<String>),
    LocalTee(Rc<String>),
    Drop,
    Loop(Rc<String>),
    Block(Rc<String>),
    Branch(Rc<String>),
    BranchIf(Rc<String>),
    Call(Rc<String>),
    End,
    Nop,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
    Int32,
    Void,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Datum {
    pub address: i32,
    pub bytes: Vec<u8>,
}
