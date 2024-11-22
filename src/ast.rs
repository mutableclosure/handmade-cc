use alloc::string::String;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Program {
    pub main: FunctionDefinition,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub return_type: Type,
    pub body: Statement,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Expression {
    Constant(i32),
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
    Int,
    Void,
}
