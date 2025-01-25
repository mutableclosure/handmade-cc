// Copyright (c) 2025 Ludovico Rossi
// SPDX-License-Identifier: MPL-2.0

use crate::{
    ast::{BinaryOp, ConstQualifier, Expression, ExpressionKind, Type},
    environment::{Environment, GlobalDeclarationType, Symbol},
    ErrorKind,
};

#[derive(Clone, Debug)]
pub struct Evaluator;

impl Evaluator {
    #[allow(clippy::only_used_in_recursion)]
    pub fn evaluate_i32(
        &self,
        expression: &Expression,
        environment: &Environment,
    ) -> Result<i32, ErrorKind> {
        match expression.r#type {
            Type::Int => {}
            Type::Void => return Err(ErrorKind::NonIntegerExpression),
        }

        match &expression.kind {
            ExpressionKind::Constant(value) => Ok(*value),
            ExpressionKind::BitwiseComplement(value) => {
                Ok(!self.evaluate_i32(value.as_ref(), environment)?)
            }
            ExpressionKind::Negation(value) => Ok(-self.evaluate_i32(value.as_ref(), environment)?),
            ExpressionKind::Not(value) => {
                let value = self.evaluate_i32(value.as_ref(), environment)?;
                Ok(if value == 0 { 1 } else { 0 })
            }
            ExpressionKind::BinaryOp(op, left, right) => {
                let left = self.evaluate_i32(left.as_ref(), environment)?;
                let right = self.evaluate_i32(right.as_ref(), environment)?;
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
                    | BinaryOp::RightShiftAssignment => {
                        return Err(ErrorKind::NonConstantExpression)
                    }
                })
            }
            ExpressionKind::Conditional(condition, left, right) => {
                let condition = self.evaluate_i32(condition.as_ref(), environment)?;
                self.evaluate_i32(
                    if condition != 0 {
                        left.as_ref()
                    } else {
                        right.as_ref()
                    },
                    environment,
                )
            }
            ExpressionKind::Global(name, ConstQualifier::Const) => {
                let (_, symbol) = environment.resolve_symbol(name.clone())?;
                match symbol {
                    Symbol::Global(
                        ConstQualifier::Const,
                        GlobalDeclarationType::NonTentative(value),
                    ) => Ok(value.unwrap_or_default()),
                    Symbol::Global(ConstQualifier::NonConst, _)
                    | Symbol::Global(ConstQualifier::Const, GlobalDeclarationType::Tentative)
                    | Symbol::Variable(_)
                    | Symbol::Function(_) => Err(ErrorKind::NonConstantExpression),
                }
            }
            ExpressionKind::Global(_, ConstQualifier::NonConst)
            | ExpressionKind::Variable(_, _)
            | ExpressionKind::PrefixIncrement(_)
            | ExpressionKind::PrefixDecrement(_)
            | ExpressionKind::PostfixIncrement(_)
            | ExpressionKind::PostfixDecrement(_)
            | ExpressionKind::FunctionCall(_, _) => Err(ErrorKind::NonConstantExpression),
        }
    }
}
