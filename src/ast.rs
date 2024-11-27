use alloc::{boxed::Box, string::String};

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

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Expression {
    Constant(i32),
    BitwiseComplement(Box<Expression>),
    Negation(Box<Expression>),
    Not(Box<Expression>),
    BinaryOp(BinaryOp, Box<Expression>, Box<Expression>),
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    Xor,
    LeftShift,
    RightShift,
    And,
    Or,
    EqualTo,
    NotEqualTo,
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
    Int,
    Void,
}
