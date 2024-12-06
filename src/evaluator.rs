use crate::{
    ast::{Expression, ExpressionKind, Type},
    ErrorKind,
};

#[derive(Clone, Debug)]
pub struct Evaluator;

impl Evaluator {
    pub fn evaluate_i32(&self, expression: &Expression) -> Result<i32, ErrorKind> {
        match expression.r#type {
            Type::Int => {}
            Type::Void => return Err(ErrorKind::NonIntegerExpression),
        }

        match expression.kind {
            ExpressionKind::Constant(value) => Ok(value),
            _ => Err(ErrorKind::NonConstantExpression),
        }
    }
}
