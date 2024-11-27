use crate::{
    ast::{
        BinaryOp, BlockItem, Declaration, Expression, FunctionDefinition, Program, Statement, Type,
    },
    environment::Environment,
    lexer::Lexer,
    token::{Keyword, Token},
    Error, ErrorKind, Severity,
};
use alloc::{boxed::Box, string::String, vec::Vec};

const MAIN: &str = "main";

#[derive(Clone, Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    environment: Environment,
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
            let function = match token {
                Token::Keyword(Keyword::Int) => self.function_definition(Type::Int),
                Token::Keyword(Keyword::Void) => self.function_definition(Type::Void),
                _ => Err(self.err(ErrorKind::UnknownType(token))),
            }?;
            functions.push(function);
        }

        let Some(main) = functions.into_iter().find(|function| function.name == MAIN) else {
            return Err(self.err(ErrorKind::UndefinedMain));
        };

        Ok(Program { main })
    }
}

impl Parser<'_> {
    fn function_definition(&mut self, return_type: Type) -> Result<FunctionDefinition, Error> {
        let name = self.expect_identifier()?;
        self.expect_token(Token::OpenParenthesis)?;
        self.expect_token(Token::Keyword(Keyword::Void))?;
        self.expect_token(Token::CloseParenthesis)?;
        self.expect_token(Token::OpenBrace)?;

        let mut body = Vec::new();

        while self.peek_any_token_but(Token::CloseBrace)? {
            body.push(self.block_item()?);
        }

        self.expect_token(Token::CloseBrace)?;

        Ok(FunctionDefinition {
            name,
            return_type,
            body,
        })
    }

    fn block_item(&mut self) -> Result<BlockItem, Error> {
        if self.peek_token(Token::Keyword(Keyword::Int))? {
            Ok(BlockItem::Declaration(self.declaration()?))
        } else {
            Ok(BlockItem::Statement(self.statement()?))
        }
    }

    fn statement(&mut self) -> Result<Statement, Error> {
        match self.lexer.peek()? {
            Some(Token::Keyword(Keyword::Return)) => {
                self.expect_token(Token::Keyword(Keyword::Return))?;
                let expression = self.expression(0)?;
                self.expect_token(Token::Semicolon)?;
                Ok(Statement::Return(expression))
            }
            Some(Token::Semicolon) => {
                self.expect_token(Token::Semicolon)?;
                Ok(Statement::Null)
            }
            _ => Ok(Statement::Expression(self.expression(0)?)),
        }
    }

    fn declaration(&mut self) -> Result<Declaration, Error> {
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

        Ok(Declaration { name, r#type, init })
    }

    fn expression(&mut self, min_precedence: u32) -> Result<Expression, Error> {
        let mut left = self.factor()?;
        let mut token = self.lexer.peek()?;

        while let Some(op) = token.and_then(binary_op) {
            let precedence = precedence(op);

            if precedence < min_precedence {
                break;
            }

            self.lexer.next()?;

            let precedence = if is_assignment(op) {
                self.expect_lvalue(&left)?;
                precedence
            } else {
                precedence.saturating_add(1)
            };

            let right = self.expression(precedence)?;
            left = Expression::BinaryOp(op, left.into(), right.into());
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
                    .resolve_variable(&identifier)
                    .map(Expression::Variable)
                    .map_err(|kind| self.err(kind))
                    .map(Box::new)
                    .map(Expression::PostfixIncrement)
            }
            Some(Token::Identifier(identifier)) if self.peek_token(Token::TwoHyphens)? => {
                self.lexer.next()?;
                self.environment
                    .resolve_variable(&identifier)
                    .map(Expression::Variable)
                    .map_err(|kind| self.err(kind))
                    .map(Box::new)
                    .map(Expression::PostfixDecrement)
            }
            Some(Token::Identifier(identifier)) => self
                .environment
                .resolve_variable(&identifier)
                .map(Expression::Variable)
                .map_err(|kind| self.err(kind)),
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

fn precedence(op: BinaryOp) -> u32 {
    match op {
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
