use crate::{
    ast::{BinaryOp, Expression, ExpressionKind, Type},
    ErrorKind,
};

#[derive(Clone, Debug)]
pub struct Evaluator;

impl Evaluator {
    pub fn evaluate_i32(&self, expression: &Expression) -> Result<i32, ErrorKind> {
        do_evaluate_i32(expression)
    }
}

pub fn do_evaluate_i32(expression: &Expression) -> Result<i32, ErrorKind> {
    match expression.r#type {
        Type::Int => {}
        Type::Void => return Err(ErrorKind::NonIntegerExpression),
    }

    match &expression.kind {
        ExpressionKind::Constant(value) => Ok(*value),
        ExpressionKind::BitwiseComplement(value) => Ok(!do_evaluate_i32(value.as_ref())?),
        ExpressionKind::Negation(value) => Ok(-do_evaluate_i32(value.as_ref())?),
        ExpressionKind::Not(value) => {
            let value = do_evaluate_i32(value.as_ref())?;
            Ok(if value == 0 { 1 } else { 0 })
        }
        ExpressionKind::BinaryOp(op, left, right) => {
            let left = do_evaluate_i32(left.as_ref())?;
            let right = do_evaluate_i32(right.as_ref())?;
            Ok(match op {
                BinaryOp::Add => left.wrapping_add(right),
                BinaryOp::Subtract => left.wrapping_sub(right),
                BinaryOp::Multiply => left.wrapping_mul(right),
                BinaryOp::Divide => {
                    if right == 0 {
                        return Err(ErrorKind::NonConstantExpression);
                    }
                    left.wrapping_div(right)
                }
                BinaryOp::Remainder => {
                    if right == 0 {
                        return Err(ErrorKind::NonConstantExpression);
                    }
                    left.wrapping_rem(right)
                }
                BinaryOp::BitwiseAnd => left & right,
                BinaryOp::BitwiseOr => left | right,
                BinaryOp::Xor => left ^ right,
                BinaryOp::LeftShift => left.wrapping_shl(right as u32),
                BinaryOp::RightShift => left.wrapping_shr(right as u32),
                BinaryOp::And => {
                    if left != 0 && right != 0 {
                        1
                    } else {
                        0
                    }
                }
                BinaryOp::Or => {
                    if left != 0 || right != 0 {
                        1
                    } else {
                        0
                    }
                }
                BinaryOp::EqualTo => {
                    if left == right {
                        1
                    } else {
                        0
                    }
                }
                BinaryOp::NotEqualTo => {
                    if left != right {
                        1
                    } else {
                        0
                    }
                }
                BinaryOp::LessThan => {
                    if left < right {
                        1
                    } else {
                        0
                    }
                }
                BinaryOp::LessThanOrEqualTo => {
                    if left <= right {
                        1
                    } else {
                        0
                    }
                }
                BinaryOp::GreaterThan => {
                    if left > right {
                        1
                    } else {
                        0
                    }
                }
                BinaryOp::GreaterThanOrEqualTo => {
                    if left >= right {
                        1
                    } else {
                        0
                    }
                }
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
                | BinaryOp::RightShiftAssignment => return Err(ErrorKind::NonConstantExpression),
            })
        }
        ExpressionKind::Conditional(condition, left, right) => {
            let condition = do_evaluate_i32(condition.as_ref())?;
            do_evaluate_i32(if condition != 0 {
                left.as_ref()
            } else {
                right.as_ref()
            })
        }
        ExpressionKind::Global(_)
        | ExpressionKind::Variable(_)
        | ExpressionKind::PrefixIncrement(_)
        | ExpressionKind::PrefixDecrement(_)
        | ExpressionKind::PostfixIncrement(_)
        | ExpressionKind::PostfixDecrement(_)
        | ExpressionKind::FunctionCall(_, _) => Err(ErrorKind::NonConstantExpression),
    }
}
