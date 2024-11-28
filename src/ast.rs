use alloc::{boxed::Box, string::String, vec::Vec};

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Program {
    pub main: FunctionDefinition,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub return_type: Type,
    pub body: Block,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Compound(Block),
    Break(String),
    Continue(String),
    While(String, Expression, Box<Statement>),
    DoWhile(String, Box<Statement>, Expression),
    For(
        String,
        Option<ForInit>,
        Option<Expression>,
        Option<Expression>,
        Box<Statement>,
    ),
    Null,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum ForInit {
    Declaration(Declaration),
    Expression(Expression),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Declaration {
    pub name: String,
    pub r#type: Type,
    pub init: Option<Expression>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Expression {
    Constant(i32),
    Variable(String),
    BitwiseComplement(Box<Expression>),
    Negation(Box<Expression>),
    Not(Box<Expression>),
    PrefixIncrement(Box<Expression>),
    PrefixDecrement(Box<Expression>),
    PostfixIncrement(Box<Expression>),
    PostfixDecrement(Box<Expression>),
    BinaryOp(BinaryOp, Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
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
    Assignment,
    AddAssignment,
    SubtractAssignment,
    MultiplyAssignment,
    DivideAssignment,
    RemainderAssignment,
    BitwiseAndAssignment,
    BitwiseOrAssignment,
    XorAssignment,
    LeftShiftAssignment,
    RightShiftAssignment,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
    Int,
    Void,
}
