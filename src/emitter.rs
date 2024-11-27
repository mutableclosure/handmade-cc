use crate::{
    ast::{
        BinaryOp, BlockItem, Expression, FunctionDefinition, Program, Statement, Type as AstType,
    },
    ir::{Function, Instruction, Module, Type as IrType, Variable},
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
    let return_type = emit_type(func_def.return_type);

    Function {
        name: func_def.name,
        local_variables: emit_variables(&func_def.body),
        instructions: {
            let mut instructions = Vec::new();
            emit_block(func_def.body, &mut instructions);
            emit_return_if_needed(return_type, &mut instructions);
            instructions
        },
        return_type,
    }
}

fn emit_variables(block: &[BlockItem]) -> Vec<Variable> {
    let mut variables = Vec::new();

    for item in block {
        match item {
            BlockItem::Statement(_) => {}
            BlockItem::Declaration(declaration) => {
                let variable = Variable {
                    name: declaration.name.clone(),
                    r#type: emit_type(declaration.r#type),
                };
                variables.push(variable);
            }
        }
    }

    variables
}

fn emit_block(block: Vec<BlockItem>, instructions: &mut Vec<Instruction>) {
    block
        .into_iter()
        .for_each(|item| emit_block_item(item, instructions));
}

fn emit_block_item(block_item: BlockItem, instructions: &mut Vec<Instruction>) {
    match block_item {
        BlockItem::Statement(statement) => emit_statement(statement, instructions),
        BlockItem::Declaration(declaration) => {
            if let Some(init) = declaration.init {
                emit_expression(init, instructions);
                instructions.push(Instruction::LocalSet(declaration.name));
            }
        }
    }
}

fn emit_statement(statement: Statement, instructions: &mut Vec<Instruction>) {
    match statement {
        Statement::Return(expression) => {
            emit_expression(expression, instructions);
            instructions.push(Instruction::Return);
        }
        Statement::Expression(expression) => {
            emit_expression(expression, instructions);
            instructions.push(Instruction::Drop);
        }
        Statement::Null => instructions.push(Instruction::Nop),
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
        Expression::BinaryOp(BinaryOp::Assignment, left, right) => {
            emit_expression(*right, instructions);

            let name = match *left {
                Expression::Variable(name) => name,
                _ => unreachable!(),
            };

            instructions.push(Instruction::LocalTee(name));
        }
        Expression::Variable(name) => instructions.push(Instruction::LocalGet(name)),
    }
}

fn emit_return_if_needed(return_type: IrType, instructions: &mut Vec<Instruction>) {
    if !instructions
        .last()
        .is_some_and(|i| matches!(i, Instruction::Return))
    {
        match return_type {
            IrType::Int32 => {
                instructions.push(Instruction::PushConstant(0));
                instructions.push(Instruction::Return);
            }
            IrType::Void => {}
        }
    }
}

fn emit_type(r#type: AstType) -> IrType {
    match r#type {
        AstType::Int => IrType::Int32,
        AstType::Void => IrType::Void,
    }
}
