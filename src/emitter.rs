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
        Expression::BinaryOp(op, left, right) => {
            emit_expression(*left, instructions);
            emit_expression(*right, instructions);
            instructions.push(op.into());
        }
    }
}

fn emit_type(r#type: AstType) -> IrType {
    match r#type {
        AstType::Int => IrType::Int32,
        AstType::Void => IrType::Void,
    }
}

impl From<BinaryOp> for Instruction {
    fn from(value: BinaryOp) -> Self {
        match value {
            BinaryOp::Add => Instruction::Add,
            BinaryOp::Subtract => Instruction::Sub,
            BinaryOp::Multiply => Instruction::Mul,
            BinaryOp::Divide => Instruction::Div,
            BinaryOp::Remainder => Instruction::Rem,
            BinaryOp::And => Instruction::And,
            BinaryOp::Or => Instruction::Or,
            BinaryOp::Xor => Instruction::Xor,
            BinaryOp::LeftShift => Instruction::ShiftLeft,
            BinaryOp::RightShift => Instruction::ShiftRight,
        }
    }
}
