use alloc::{boxed::Box, rc::Rc, string::String, vec::Vec};

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Program {
    pub functions: Vec<FunctionDeclaration>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Compound(Block),
    Break(Rc<String>),
    Continue(Rc<String>),
    While(Rc<String>, Expression, Box<Statement>),
    DoWhile(Rc<String>, Box<Statement>, Expression),
    For(
        Rc<String>,
        Option<ForInit>,
        Option<Expression>,
        Option<Expression>,
        Box<Statement>,
    ),
    Null,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum ForInit {
    Declaration(VariableDeclaration),
    Expression(Expression),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum BlockItem {
    Statement(Statement),
    VariableDeclaration(VariableDeclaration),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct FunctionDeclaration {
    pub name: Rc<String>,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Type,
    pub body: FunctionBody,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum FunctionBody {
    Extern,
    Block(Block),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct FunctionParameter {
    pub name: Rc<String>,
    pub r#type: Type,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct VariableDeclaration {
    pub name: Rc<String>,
    pub r#type: Type,
    pub init: Option<Expression>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Expression {
    Constant(i32),
    Variable(Rc<String>),
    BitwiseComplement(Box<Expression>),
    Negation(Box<Expression>),
    Not(Box<Expression>),
    PrefixIncrement(Box<Expression>),
    PrefixDecrement(Box<Expression>),
    PostfixIncrement(Box<Expression>),
    PostfixDecrement(Box<Expression>),
    BinaryOp(BinaryOp, Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    FunctionCall(Rc<String>, Vec<Expression>),
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
