use alloc::{string::String, vec::Vec};

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Module {
    pub functions: Vec<Function>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub instructions: Vec<Instruction>,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
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
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
    Int32,
    Void,
}
