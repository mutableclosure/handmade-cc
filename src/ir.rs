use alloc::{string::String, vec::Vec};

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Module {
    pub functions: Vec<Function>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub local_variables: Vec<Variable>,
    pub instructions: Vec<Instruction>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Variable {
    pub name: String,
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
    IfWithResult,
    Else,
    End,
    Select,
    Return,
    LocalGet(String),
    LocalSet(String),
    LocalTee(String),
    Drop,
    Nop,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
    Int32,
    Void,
}
