use crate::{
    ast::{
        BinaryOp, Block, BlockItem, Expression, ForInit, FunctionBody, FunctionDeclaration,
        FunctionParameter, Program, Statement, Type, VariableDeclaration,
    },
    environment::{Environment, FunctionDeclarationType, Symbol},
    lexer::Lexer,
    token::{Keyword, Token},
    verifier::Verifier,
    Error, ErrorKind, Severity,
};
use alloc::{
    boxed::Box,
    string::{String, ToString},
    vec::Vec,
};

#[derive(Clone, Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    environment: Environment,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
enum Op {
    Binary(BinaryOp),
    Conditional,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
enum DeclarationScope {
    Extern,
    Static,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            environment: Environment::default(),
        }
    }

    pub fn parse(&mut self) -> Result<Program, Error> {
        let mut functions = Vec::new();

        while let Some(token) = self.lexer.next()? {
            if let Some(function) = match token {
                Token::Keyword(Keyword::Int) => {
                    self.declaration(Type::Int, DeclarationScope::Static)
                }
                Token::Keyword(Keyword::Void) => {
                    self.declaration(Type::Void, DeclarationScope::Static)
                }
                Token::Keyword(Keyword::Extern) => self.extern_declaration(),
                _ => Err(self.err(ErrorKind::UnknownType(token))),
            }? {
                functions.push(function);
            }
        }

        Verifier::new(&self.environment).verify_functions(&functions)?;

        Ok(Program { functions })
    }
}

impl Parser<'_> {
    fn extern_declaration(&mut self) -> Result<Option<FunctionDeclaration>, Error> {
        let token = self.lexer.next()?;

        match token {
            Some(Token::Keyword(Keyword::Int)) => {
                self.declaration(Type::Int, DeclarationScope::Extern)
            }
            Some(Token::Keyword(Keyword::Void)) => {
                self.declaration(Type::Void, DeclarationScope::Extern)
            }
            _ => Err(self.err(
                token
                    .map(ErrorKind::UnknownType)
                    .unwrap_or_else(|| ErrorKind::ExpectedType),
            )),
        }
    }

    fn declaration(
        &mut self,
        r#type: Type,
        scope: DeclarationScope,
    ) -> Result<Option<FunctionDeclaration>, Error> {
        let name = self.expect_identifier()?;
        let parameters = self.function_parameters()?;
        let declaration_type = match scope {
            DeclarationScope::Extern => {
                self.expect_token(Token::Semicolon)?;
                FunctionDeclarationType::ExternDeclaration
            }
            DeclarationScope::Static => {
                if self.peek_token(Token::Semicolon)? {
                    self.lexer.next()?;
                    FunctionDeclarationType::ForwardDeclaration
                } else {
                    FunctionDeclarationType::Definition
                }
            }
        };

        self.environment
            .declare_function(name.clone(), parameters.clone(), r#type, declaration_type)
            .map_err(|kind| self.err(kind))?;

        match declaration_type {
            FunctionDeclarationType::ExternDeclaration => Ok(Some(FunctionDeclaration {
                name,
                parameters,
                return_type: r#type,
                body: FunctionBody::Extern,
            })),
            FunctionDeclarationType::ForwardDeclaration => Ok(None),
            FunctionDeclarationType::Definition => {
                self.environment.enter_function(name.clone());
                let body = FunctionBody::Block(self.block()?);
                self.environment.exit_function();

                Ok(Some(FunctionDeclaration {
                    name,
                    parameters,
                    return_type: r#type,
                    body,
                }))
            }
        }
    }

    fn function_parameters(&mut self) -> Result<Vec<FunctionParameter>, Error> {
        let mut parameters = Vec::new();
        self.expect_token(Token::OpenParenthesis)?;

        if self.peek_token(Token::Keyword(Keyword::Void))? {
            self.lexer.next()?;
        } else {
            loop {
                self.expect_token(Token::Keyword(Keyword::Int))?;
                let name = self.expect_identifier()?;
                let r#type = Type::Int;
                parameters.push(FunctionParameter { name, r#type });

                if !self.peek_token(Token::Comma)? {
                    break;
                }

                self.lexer.next()?;
            }
        }

        self.expect_token(Token::CloseParenthesis)?;

        Ok(parameters)
    }

    fn statement(&mut self) -> Result<Statement, Error> {
        match self.lexer.peek()? {
            Some(Token::Keyword(Keyword::Return)) => {
                self.lexer.next()?;
                let expression = self.expression(0)?;
                self.expect_token(Token::Semicolon)?;
                Ok(Statement::Return(expression))
            }
            Some(Token::Semicolon) => {
                self.lexer.next()?;
                Ok(Statement::Null)
            }
            Some(Token::Keyword(Keyword::If)) => {
                self.lexer.next()?;
                let condition = self.condition()?;
                let then = self.statement()?.into();

                let r#else = if self.peek_token(Token::Keyword(Keyword::Else))? {
                    self.lexer.next()?;
                    Some(self.statement()?)
                } else {
                    None
                }
                .map(|s| s.into());

                Ok(Statement::If(condition, then, r#else))
            }
            Some(Token::Keyword(Keyword::Break)) => {
                self.lexer.next()?;
                let label = self
                    .environment
                    .loop_label()
                    .map(|l| l.to_string())
                    .ok_or_else(|| self.err(ErrorKind::BreakOutsideLoopOrSwitch))?;
                self.expect_token(Token::Semicolon)?;
                Ok(Statement::Break(label))
            }
            Some(Token::Keyword(Keyword::Continue)) => {
                self.lexer.next()?;
                let label = self
                    .environment
                    .loop_label()
                    .map(|l| l.to_string())
                    .ok_or_else(|| self.err(ErrorKind::ContinueOutsideLoop))?;
                self.expect_token(Token::Semicolon)?;
                Ok(Statement::Continue(label))
            }
            Some(Token::Keyword(Keyword::While)) => {
                self.lexer.next()?;
                let condition = self.condition()?;
                let (label, body) = self.loop_body()?;
                Ok(Statement::While(label, condition, body))
            }
            Some(Token::Keyword(Keyword::Do)) => {
                self.lexer.next()?;
                let (label, body) = self.loop_body()?;
                self.expect_token(Token::Keyword(Keyword::While))?;
                let condition = self.condition()?;
                self.expect_token(Token::Semicolon)?;
                Ok(Statement::DoWhile(label, body, condition))
            }
            Some(Token::Keyword(Keyword::For)) => {
                self.lexer.next()?;
                self.environment.nest();
                self.expect_token(Token::OpenParenthesis)?;
                let init = self.for_init()?;
                let condition = self.maybe_expression(Token::Semicolon)?;
                let post = self.maybe_expression(Token::CloseParenthesis)?;
                let (label, body) = self.loop_body()?;
                self.environment.unnest();
                Ok(Statement::For(label, init, condition, post, body))
            }
            Some(Token::OpenBrace) => Ok(Statement::Compound(self.block()?)),
            _ => {
                let expression = self.expression(0)?;
                self.expect_token(Token::Semicolon)?;
                Ok(Statement::Expression(expression))
            }
        }
    }

    fn condition(&mut self) -> Result<Expression, Error> {
        self.expect_token(Token::OpenParenthesis)?;
        let condition = self.expression(0)?;
        self.expect_token(Token::CloseParenthesis)?;
        Ok(condition)
    }

    fn for_init(&mut self) -> Result<Option<ForInit>, Error> {
        if self.peek_token(Token::Keyword(Keyword::Int))? {
            Ok(Some(ForInit::Declaration(self.variable_declaration()?)))
        } else if self.peek_token(Token::Semicolon)? {
            self.lexer.next()?;
            Ok(None)
        } else {
            let expression = self.expression(0)?;
            self.expect_token(Token::Semicolon)?;
            Ok(Some(ForInit::Expression(expression)))
        }
    }

    fn maybe_expression(&mut self, terminator: Token) -> Result<Option<Expression>, Error> {
        Ok(if self.peek_token(terminator.clone())? {
            self.lexer.next()?;
            None
        } else {
            let expression = self.expression(0)?;
            self.expect_token(terminator)?;
            Some(expression)
        })
    }

    fn loop_body(&mut self) -> Result<(String, Box<Statement>), Error> {
        let label = self.environment.enter_loop().to_string();
        let body = self.statement()?.into();
        self.environment.exit_loop();
        Ok((label, body))
    }

    fn block(&mut self) -> Result<Block, Error> {
        self.expect_token(Token::OpenBrace)?;
        self.environment.nest();

        let mut items = Vec::new();

        while self.peek_any_token_but(Token::CloseBrace)? {
            items.push(self.block_item()?);
        }

        self.expect_token(Token::CloseBrace)?;
        self.environment.unnest();

        Ok(Block { items })
    }

    fn block_item(&mut self) -> Result<BlockItem, Error> {
        if self.peek_token(Token::Keyword(Keyword::Int))? {
            Ok(BlockItem::VariableDeclaration(self.variable_declaration()?))
        } else {
            Ok(BlockItem::Statement(self.statement()?))
        }
    }

    fn variable_declaration(&mut self) -> Result<VariableDeclaration, Error> {
        self.expect_token(Token::Keyword(Keyword::Int))?;

        let r#type = Type::Int;
        let identifier = self.expect_identifier()?;
        let name = self
            .environment
            .declare_variable(&identifier)
            .map_err(|kind| self.err(kind))?;

        let init = if self.peek_token(Token::EqualSign)? {
            self.expect_token(Token::EqualSign)?;
            Some(self.expression(0)?)
        } else {
            None
        };

        self.expect_token(Token::Semicolon)?;

        Ok(VariableDeclaration { name, r#type, init })
    }

    fn expression(&mut self, min_precedence: u32) -> Result<Expression, Error> {
        let mut left = self.factor()?;
        let mut token = self.lexer.peek()?;

        while let Some(op) = token.and_then(op) {
            let precedence = precedence(op);

            if precedence < min_precedence {
                break;
            }

            self.lexer.next()?;

            match op {
                Op::Binary(op) => {
                    let precedence = if is_assignment(op) {
                        self.expect_lvalue(&left)?;
                        precedence
                    } else {
                        precedence.saturating_add(1)
                    };

                    let right = self.expression(precedence)?;
                    left = Expression::BinaryOp(op, left.into(), right.into());
                }
                Op::Conditional => {
                    let middle = self.expression(0)?;
                    self.expect_token(Token::Colon)?;
                    let right = self.expression(precedence)?;
                    left = Expression::Conditional(left.into(), middle.into(), right.into());
                }
            }

            token = self.lexer.peek()?;
        }

        Ok(left)
    }

    fn factor(&mut self) -> Result<Expression, Error> {
        match self.lexer.next()? {
            Some(Token::Constant(value)) => Ok(Expression::Constant(value)),
            Some(Token::Identifier(identifier)) if self.peek_token(Token::TwoPlusSigns)? => {
                self.lexer.next()?;
                self.environment
                    .resolve_lvalue(&identifier)
                    .map(|v| v.to_string())
                    .map(Expression::Variable)
                    .map_err(|kind| self.err(kind))
                    .map(Box::new)
                    .map(Expression::PostfixIncrement)
            }
            Some(Token::Identifier(identifier)) if self.peek_token(Token::TwoHyphens)? => {
                self.lexer.next()?;
                self.environment
                    .resolve_lvalue(&identifier)
                    .map(|v| v.to_string())
                    .map(Expression::Variable)
                    .map_err(|kind| self.err(kind))
                    .map(Box::new)
                    .map(Expression::PostfixDecrement)
            }
            Some(Token::Identifier(identifier)) => self.symbol(&identifier),
            Some(Token::TwoPlusSigns) => {
                let variable = self.factor()?;
                self.expect_lvalue(&variable)?;
                Ok(Expression::PrefixIncrement(variable.into()))
            }
            Some(Token::TwoHyphens) => {
                let variable = self.factor()?;
                self.expect_lvalue(&variable)?;
                Ok(Expression::PrefixDecrement(variable.into()))
            }
            Some(Token::Tilde) => self
                .factor()
                .map(Box::new)
                .map(Expression::BitwiseComplement),
            Some(Token::Hyphen) => self.factor().map(Box::new).map(Expression::Negation),
            Some(Token::ExclamationPoint) => self.factor().map(Box::new).map(Expression::Not),
            Some(Token::OpenParenthesis) => {
                let expression = self.expression(0)?;
                let expression = self
                    .expect_token(Token::CloseParenthesis)
                    .map(|()| expression)?;
                if matches!(expression, Expression::Variable(_)) {
                    if self.peek_token(Token::TwoPlusSigns)? {
                        self.lexer.next()?;
                        return Ok(Expression::PostfixIncrement(expression.into()));
                    } else if self.peek_token(Token::TwoHyphens)? {
                        self.lexer.next()?;
                        return Ok(Expression::PostfixDecrement(expression.into()));
                    }
                }
                Ok(expression)
            }
            token => Err(self.err(ErrorKind::ExpectedExpression(token))),
        }
    }

    fn symbol(&mut self, identifier: &str) -> Result<Expression, Error> {
        let (name, symbol) = self
            .environment
            .resolve_symbol(identifier)
            .map_err(|kind| self.err(kind))?;

        match symbol {
            Symbol::Variable => Ok(Expression::Variable(name.to_string())),
            Symbol::Function(function) => {
                let name = name.to_string();
                let parameters = function.parameters.values().copied().collect::<Vec<_>>();
                assert!(matches!(function.return_type, Type::Int));
                self.expect_token(Token::OpenParenthesis)?;

                let mut arguments = Vec::new();

                for (index, param_type) in parameters.iter().enumerate() {
                    let argument = self.expression(0)?;
                    assert!(matches!(param_type, Type::Int));
                    arguments.push(argument);

                    if index < parameters.len() - 1 {
                        self.expect_token(Token::Comma)?;
                    }
                }

                self.expect_token(Token::CloseParenthesis)?;

                Ok(Expression::FunctionCall(name, arguments))
            }
        }
    }

    fn expect_identifier(&mut self) -> Result<String, Error> {
        self.lexer.next()?.map_or_else(
            || Err(self.err(ErrorKind::ExpectedIdentifier(None))),
            |token| match token {
                Token::Identifier(identifier) => Ok(identifier.clone()),
                _ => Err(self.err(ErrorKind::ExpectedIdentifier(Some(token)))),
            },
        )
    }

    fn expect_token(&mut self, expected_token: Token) -> Result<(), Error> {
        self.lexer.next()?.map_or_else(
            || Err(self.err(ErrorKind::ExpectedIdentifier(None))),
            |token| {
                if token == expected_token {
                    Ok(())
                } else {
                    Err(self.err(ErrorKind::ExpectedToken(expected_token, Some(token))))
                }
            },
        )
    }

    fn peek_token(&mut self, expected_token: Token) -> Result<bool, Error> {
        Ok(self.lexer.peek()?.is_some_and(|t| *t == expected_token))
    }

    fn peek_any_token_but(&mut self, unexpected_token: Token) -> Result<bool, Error> {
        Ok(self.lexer.peek()?.is_some_and(|t| *t != unexpected_token))
    }

    fn expect_lvalue(&self, expression: &Expression) -> Result<(), Error> {
        if !matches!(expression, Expression::Variable(_)) {
            Err(self.err(ErrorKind::InvalidLvalue))
        } else {
            Ok(())
        }
    }

    fn err(&self, kind: ErrorKind) -> Error {
        Error {
            line_number: self.lexer.line_number(),
            kind,
            severity: Severity::Error,
        }
    }
}

fn op(token: &Token) -> Option<Op> {
    binary_op(token)
        .map(Op::Binary)
        .or_else(|| matches!(token, Token::QuestionMark).then(|| Op::Conditional))
}

fn binary_op(token: &Token) -> Option<BinaryOp> {
    match token {
        Token::PlusSign => Some(BinaryOp::Add),
        Token::Hyphen => Some(BinaryOp::Subtract),
        Token::Asterisk => Some(BinaryOp::Multiply),
        Token::Slash => Some(BinaryOp::Divide),
        Token::Percent => Some(BinaryOp::Remainder),
        Token::Ampersand => Some(BinaryOp::BitwiseAnd),
        Token::Bar => Some(BinaryOp::BitwiseOr),
        Token::Circumflex => Some(BinaryOp::Xor),
        Token::TwoLessThanOps => Some(BinaryOp::LeftShift),
        Token::TwoGreaterThanOps => Some(BinaryOp::RightShift),
        Token::TwoAmpersands => Some(BinaryOp::And),
        Token::TwoBars => Some(BinaryOp::Or),
        Token::TwoEqualSigns => Some(BinaryOp::EqualTo),
        Token::NotEqualSign => Some(BinaryOp::NotEqualTo),
        Token::LessThanOp => Some(BinaryOp::LessThan),
        Token::LessThanOrEqualToOp => Some(BinaryOp::LessThanOrEqualTo),
        Token::GreaterThanOp => Some(BinaryOp::GreaterThan),
        Token::GreaterThanOrEqualToOp => Some(BinaryOp::GreaterThanOrEqualTo),
        Token::EqualSign => Some(BinaryOp::Assignment),
        Token::PlusEqualSign => Some(BinaryOp::AddAssignment),
        Token::HyphenEqualSign => Some(BinaryOp::SubtractAssignment),
        Token::AsteriskEqualSign => Some(BinaryOp::MultiplyAssignment),
        Token::SlashEqualSign => Some(BinaryOp::DivideAssignment),
        Token::PercentEqualSign => Some(BinaryOp::RemainderAssignment),
        Token::AmpersandEqualSign => Some(BinaryOp::BitwiseAndAssignment),
        Token::BarEqualSign => Some(BinaryOp::BitwiseOrAssignment),
        Token::CircumflexEqualSign => Some(BinaryOp::XorAssignment),
        Token::TwoLessThanOpsEqualSign => Some(BinaryOp::LeftShiftAssignment),
        Token::TwoGreaterThanOpsEqualSign => Some(BinaryOp::RightShiftAssignment),
        _ => None,
    }
}

fn precedence(op: Op) -> u32 {
    match op {
        Op::Binary(op) => match op {
            BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Remainder => 50,
            BinaryOp::Add | BinaryOp::Subtract => 45,
            BinaryOp::LeftShift | BinaryOp::RightShift => 40,
            BinaryOp::LessThan
            | BinaryOp::LessThanOrEqualTo
            | BinaryOp::GreaterThan
            | BinaryOp::GreaterThanOrEqualTo => 35,
            BinaryOp::EqualTo | BinaryOp::NotEqualTo => 30,
            BinaryOp::BitwiseAnd => 25,
            BinaryOp::Xor => 20,
            BinaryOp::BitwiseOr => 15,
            BinaryOp::And => 10,
            BinaryOp::Or => 5,
            BinaryOp::Assignment
            | BinaryOp::AddAssignment
            | BinaryOp::SubtractAssignment
            | BinaryOp::MultiplyAssignment
            | BinaryOp::DivideAssignment
            | BinaryOp::RemainderAssignment
            | BinaryOp::LeftShiftAssignment
            | BinaryOp::RightShiftAssignment
            | BinaryOp::BitwiseAndAssignment
            | BinaryOp::BitwiseOrAssignment
            | BinaryOp::XorAssignment => 1,
        },
        Op::Conditional => 3,
    }
}

fn is_assignment(op: BinaryOp) -> bool {
    match op {
        BinaryOp::Assignment
        | BinaryOp::AddAssignment
        | BinaryOp::SubtractAssignment
        | BinaryOp::MultiplyAssignment
        | BinaryOp::DivideAssignment
        | BinaryOp::RemainderAssignment
        | BinaryOp::BitwiseAndAssignment
        | BinaryOp::BitwiseOrAssignment
        | BinaryOp::XorAssignment
        | BinaryOp::LeftShiftAssignment
        | BinaryOp::RightShiftAssignment => true,
        BinaryOp::Add
        | BinaryOp::Subtract
        | BinaryOp::Multiply
        | BinaryOp::Divide
        | BinaryOp::Remainder
        | BinaryOp::BitwiseAnd
        | BinaryOp::BitwiseOr
        | BinaryOp::Xor
        | BinaryOp::LeftShift
        | BinaryOp::RightShift
        | BinaryOp::And
        | BinaryOp::Or
        | BinaryOp::EqualTo
        | BinaryOp::NotEqualTo
        | BinaryOp::LessThan
        | BinaryOp::LessThanOrEqualTo
        | BinaryOp::GreaterThan
        | BinaryOp::GreaterThanOrEqualTo => false,
    }
}
