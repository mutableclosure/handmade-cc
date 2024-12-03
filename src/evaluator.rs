use crate::{ast::Expression, ErrorKind};

#[derive(Clone, Debug)]
pub struct Evaluator;

impl Evaluator {
    pub fn evaluate_init_value(&self, expression: &Expression) -> Result<i32, ErrorKind> {
        match expression {
            Expression::Constant(value) => Ok(*value),
            _ => Err(ErrorKind::NonConstantInit),
        }
    }
}
