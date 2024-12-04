use crate::{
    ast::{Expression, ExpressionKind},
    ErrorKind,
};

#[derive(Clone, Debug)]
pub struct Evaluator;

impl Evaluator {
    pub fn evaluate_init_value(&self, expression: &Expression) -> Result<i32, ErrorKind> {
        match expression.kind {
            ExpressionKind::Constant(value) => Ok(value),
            _ => Err(ErrorKind::NonConstantInit),
        }
    }
}
