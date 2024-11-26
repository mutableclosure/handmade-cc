use crate::{
    ast::{BinaryOp, Expression, FunctionDefinition, Program, Statement, Type},
    lexer::Lexer,
    token::{Keyword, Token},
    Error, ErrorKind, Severity,
};
use alloc::{boxed::Box, string::String, vec::Vec};

const MAIN: &str = "main";

#[derive(Clone, Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
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
        let body = self.statement()?;
        self.expect_token(Token::CloseBrace)?;

        Ok(FunctionDefinition {
            name,
            return_type,
            body,
        })
    }

    fn statement(&mut self) -> Result<Statement, Error> {
        self.expect_token(Token::Keyword(Keyword::Return))?;
        let expression = self.expression(0)?;
        self.expect_token(Token::Semicolon)?;
        Ok(Statement::Return(expression))
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

            let right = self.expression(precedence.saturating_add(1))?;
            left = Expression::BinaryOp(op, left.into(), right.into());
            token = self.lexer.peek()?;
        }

        Ok(left)
    }

    fn factor(&mut self) -> Result<Expression, Error> {
        match self.lexer.next()? {
            Some(Token::Constant(value)) => Ok(Expression::Constant(value)),
            Some(Token::Tilde) => self
                .factor()
                .map(Box::new)
                .map(Expression::BitwiseComplement),
            Some(Token::Hyphen) => self.factor().map(Box::new).map(Expression::Negation),
            Some(Token::OpenParenthesis) => {
                let expression = self.expression(0)?;
                self.expect_token(Token::CloseParenthesis)
                    .map(|()| expression)
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
        Token::Plus => Some(BinaryOp::Add),
        Token::Hyphen => Some(BinaryOp::Subtract),
        Token::Asterisk => Some(BinaryOp::Multiply),
        Token::Slash => Some(BinaryOp::Divide),
        Token::Percent => Some(BinaryOp::Remainder),
        Token::Ampersand => Some(BinaryOp::And),
        Token::Bar => Some(BinaryOp::Or),
        Token::Circumflex => Some(BinaryOp::Xor),
        Token::LeftShift => Some(BinaryOp::LeftShift),
        Token::RightShift => Some(BinaryOp::RightShift),
        _ => None,
    }
}

fn precedence(op: BinaryOp) -> u32 {
    match op {
        BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Remainder => 50,
        BinaryOp::Add | BinaryOp::Subtract => 45,
        BinaryOp::LeftShift | BinaryOp::RightShift => 40,
        BinaryOp::And => 35,
        BinaryOp::Xor => 30,
        BinaryOp::Or => 25,
    }
}
