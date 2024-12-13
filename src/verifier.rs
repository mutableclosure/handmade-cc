use crate::{
    ast::{
        Block, BlockItem, Expression, ExpressionKind, ForInit, FunctionBody, FunctionDeclaration,
        Statement,
    },
    environment::Environment,
    Error, ErrorKind, Severity,
};
use alloc::string::ToString;

const MAIN_FUNCTION_NAME: &str = "main";

#[derive(Clone, Debug)]
pub struct Verifier<'a> {
    environment: &'a Environment,
}

impl<'a> Verifier<'a> {
    pub fn new(environment: &'a Environment) -> Self {
        Self { environment }
    }

    pub fn verify_functions(&self, functions: &[FunctionDeclaration]) -> Result<(), Error> {
        if !functions
            .iter()
            .any(|f| f.name.as_str() == MAIN_FUNCTION_NAME)
        {
            return Err(self.err(ErrorKind::UndefinedFunction(
                MAIN_FUNCTION_NAME.to_string().into(),
            )));
        }

        self.verify_calls(functions)
    }
}

impl Verifier<'_> {
    fn verify_calls(&self, functions: &[FunctionDeclaration]) -> Result<(), Error> {
        for function in functions {
            match &function.body {
                FunctionBody::Extern => {}
                FunctionBody::Block(block) => self.verify_calls_in_block(block)?,
            }
        }

        Ok(())
    }

    fn verify_calls_in_block(&self, block: &Block) -> Result<(), Error> {
        for item in &block.items {
            match item {
                BlockItem::Statement(statement) => self.verify_calls_in_statement(statement)?,
                BlockItem::VariableDeclaration(declaration) => {
                    if let Some(init) = declaration.init.as_ref() {
                        self.verify_calls_in_expression(init)?;
                    }
                }
            }
        }

        Ok(())
    }

    fn verify_calls_in_statement(&self, statement: &Statement) -> Result<(), Error> {
        match statement {
            Statement::Expression(expression) => self.verify_calls_in_expression(expression)?,
            Statement::If(expression, statement, statement1) => {
                self.verify_calls_in_expression(expression)?;
                self.verify_calls_in_statement(statement)?;
                if let Some(statement) = statement1 {
                    self.verify_calls_in_statement(statement)?;
                }
            }
            Statement::Compound(block) => self.verify_calls_in_block(block)?,
            Statement::While(_, expression, statement) => {
                self.verify_calls_in_expression(expression)?;
                self.verify_calls_in_statement(statement)?;
            }
            Statement::DoWhile(_, statement, expression) => {
                self.verify_calls_in_statement(statement)?;
                self.verify_calls_in_expression(expression)?;
            }
            Statement::For(_, for_init, expression, expression1, statement) => {
                if let Some(for_init) = for_init {
                    match for_init {
                        ForInit::Declaration(_) => {}
                        ForInit::Expression(expression) => {
                            self.verify_calls_in_expression(expression)?
                        }
                    }
                }
                if let Some(expression) = expression {
                    self.verify_calls_in_expression(expression)?;
                }
                if let Some(expression) = expression1 {
                    self.verify_calls_in_expression(expression)?;
                }
                self.verify_calls_in_statement(statement)?;
            }
            Statement::Switch(_, condition, cases, _) => {
                self.verify_calls_in_expression(condition)?;
                for case in cases {
                    for statement in &case.statements {
                        self.verify_calls_in_statement(statement)?;
                    }
                }
            }
            Statement::Return(expression) => {
                if let Some(expression) = expression {
                    self.verify_calls_in_expression(expression)?;
                }
            }
            Statement::Break(_) | Statement::Continue(_) | Statement::Null => {}
        }

        Ok(())
    }

    fn verify_calls_in_expression(&self, expression: &Expression) -> Result<(), Error> {
        match &expression.kind {
            ExpressionKind::BitwiseComplement(expression)
            | ExpressionKind::Negation(expression)
            | ExpressionKind::Not(expression)
            | ExpressionKind::PrefixIncrement(expression)
            | ExpressionKind::PrefixDecrement(expression)
            | ExpressionKind::PostfixIncrement(expression)
            | ExpressionKind::PostfixDecrement(expression) => {
                self.verify_calls_in_expression(expression)
            }
            ExpressionKind::BinaryOp(_, left, right) => {
                self.verify_calls_in_expression(left)?;
                self.verify_calls_in_expression(right)
            }
            ExpressionKind::Conditional(left, middle, right) => {
                self.verify_calls_in_expression(left)?;
                self.verify_calls_in_expression(middle)?;
                self.verify_calls_in_expression(right)
            }
            ExpressionKind::FunctionCall(name, arguments) => {
                if !self.environment.is_function_defined(name.clone()) {
                    return Err(self.err(ErrorKind::UndefinedFunction(name.clone())));
                }

                for arg in arguments {
                    self.verify_calls_in_expression(arg)?;
                }

                Ok(())
            }
            ExpressionKind::Constant(_)
            | ExpressionKind::Global(_, _)
            | ExpressionKind::Variable(_, _) => Ok(()),
        }
    }

    fn err(&self, kind: ErrorKind) -> Error {
        Error {
            line_number: 0,
            kind,
            severity: Severity::Error,
        }
    }
}
