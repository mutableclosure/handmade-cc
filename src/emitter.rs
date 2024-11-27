use crate::{
    ast::{BinaryOp, Expression, FunctionDefinition, Program, Statement, Type as AstType},
    ir::{Function, Instruction, Module, Type as IrType},
    Error,
};
use alloc::vec::Vec;

#[derive(Clone, Debug)]
pub struct Emitter;

impl Emitter {
    pub fn emit(&self, program: Program) -> Result<Module, Error> {
        Ok(Module {
            functions: vec![emit_func_def(program.main)],
        })
    }
}

fn emit_func_def(func_def: FunctionDefinition) -> Function {
    Function {
        name: func_def.name,
        return_type: emit_type(func_def.return_type),
        instructions: {
            let mut instructions = Vec::new();
            emit_statement(func_def.body, &mut instructions);
            instructions
        },
    }
}

fn emit_statement(statement: Statement, instructions: &mut Vec<Instruction>) {
    match statement {
        Statement::Return(expression) => {
            emit_expression(expression, instructions);
            instructions.push(Instruction::Return);
        }
    }
}

fn emit_expression(expression: Expression, instructions: &mut Vec<Instruction>) {
    match expression {
        Expression::Constant(value) => instructions.push(Instruction::PushConstant(value)),
        Expression::BitwiseComplement(expression) => {
            emit_expression(*expression, instructions);
            instructions.push(Instruction::PushConstant(-1));
            instructions.push(Instruction::Xor);
        }
        Expression::Negation(expression) => {
            instructions.push(Instruction::PushConstant(0));
            emit_expression(*expression, instructions);
            instructions.push(Instruction::Sub);
        }
        Expression::Not(expression) => {
            emit_expression(*expression, instructions);
            instructions.push(Instruction::Eqz);
        }
        Expression::BinaryOp(BinaryOp::Add, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::Add);
        }
        Expression::BinaryOp(BinaryOp::Subtract, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::Sub);
        }
        Expression::BinaryOp(BinaryOp::Multiply, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::Mul);
        }
        Expression::BinaryOp(BinaryOp::Divide, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::Div);
        }
        Expression::BinaryOp(BinaryOp::Remainder, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::Rem);
        }
        Expression::BinaryOp(BinaryOp::BitwiseAnd, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::And);
        }
        Expression::BinaryOp(BinaryOp::BitwiseOr, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::Or);
        }
        Expression::BinaryOp(BinaryOp::Xor, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::Xor);
        }
        Expression::BinaryOp(BinaryOp::LeftShift, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::ShiftLeft);
        }
        Expression::BinaryOp(BinaryOp::RightShift, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::ShiftRight);
        }
        Expression::BinaryOp(BinaryOp::And, left, right) => {
            emit_expression(*left, instructions);
            instructions.push(Instruction::Eqz);
            instructions.push(Instruction::IfWithResult);
            instructions.push(Instruction::PushConstant(0));
            instructions.push(Instruction::Else);
            instructions.push(Instruction::PushConstant(0));
            instructions.push(Instruction::PushConstant(1));
            emit_expression(*right, instructions);
            instructions.push(Instruction::Eqz);
            instructions.push(Instruction::Select);
            instructions.push(Instruction::End);
        }
        Expression::BinaryOp(BinaryOp::Or, left, right) => {
            emit_expression(*left, instructions);
            instructions.push(Instruction::Eqz);
            instructions.push(Instruction::IfWithResult);
            instructions.push(Instruction::PushConstant(0));
            instructions.push(Instruction::PushConstant(1));
            emit_expression(*right, instructions);
            instructions.push(Instruction::Eqz);
            instructions.push(Instruction::Select);
            instructions.push(Instruction::Else);
            instructions.push(Instruction::PushConstant(1));
            instructions.push(Instruction::End);
        }
        Expression::BinaryOp(BinaryOp::EqualTo, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::Eq);
        }
        Expression::BinaryOp(BinaryOp::NotEqualTo, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::Ne);
        }
        Expression::BinaryOp(BinaryOp::LessThan, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::Lt);
        }
        Expression::BinaryOp(BinaryOp::LessThanOrEqualTo, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::Le);
        }
        Expression::BinaryOp(BinaryOp::GreaterThan, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::Gt);
        }
        Expression::BinaryOp(BinaryOp::GreaterThanOrEqualTo, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(Instruction::Ge);
        }
    }
}

fn emit_type(r#type: AstType) -> IrType {
    match r#type {
        AstType::Int => IrType::Int32,
        AstType::Void => IrType::Void,
    }
}
