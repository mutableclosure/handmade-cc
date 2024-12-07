use alloc::{boxed::Box, rc::Rc, string::String, vec::Vec};

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Program {
    pub globals: Vec<GlobalDeclaration>,
    pub functions: Vec<FunctionDeclaration>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct GlobalDeclaration {
    pub name: Rc<String>,
    pub r#type: Type,
    pub r#const: ConstQualifier,
    pub init: Option<i32>,
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
    pub r#const: ConstQualifier,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct VariableDeclaration {
    pub name: Rc<String>,
    pub r#type: Type,
    pub r#const: ConstQualifier,
    pub init: Option<Expression>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Statement {
    Return(Option<Expression>),
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
    Switch(Rc<String>, Expression, Vec<Case>, Option<i32>),
    Null,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum ForInit {
    Declaration(VariableDeclaration),
    Expression(Expression),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Case {
    pub label: Rc<String>,
    pub value: i32,
    pub statements: Vec<Statement>,
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
pub struct Expression {
    pub kind: ExpressionKind,
    pub r#type: Type,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum ExpressionKind {
    Constant(i32),
    Global(Rc<String>, ConstQualifier),
    Variable(Rc<String>, ConstQualifier),
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

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum Lvalue {
    Global(Rc<String>),
    Variable(Rc<String>),
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
pub enum ConstQualifier {
    NonConst,
    Const,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
    Int,
    Void,
}
