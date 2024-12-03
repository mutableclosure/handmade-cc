use crate::{
    ast::{
        BinaryOp, Block, BlockItem, Expression, ForInit, FunctionBody, FunctionParameter, Lvalue,
        Program, Statement, Type as AstType, VariableDeclaration,
    },
    ir::{ExternalFunction, Function, Global, Instruction, Module, Type as IrType, Variable},
};
use alloc::{collections::btree_set::BTreeSet, rc::Rc, string::String, vec::Vec};

const BREAK_LABEL: &str = "break";
const CONTINUE_LABEL: &str = "continue";
const POST_LABEL: &str = "post";

#[derive(Clone, Debug)]
pub struct Emitter;

impl Emitter {
    pub fn emit(&self, program: Program) -> Module {
        let globals = program
            .globals
            .into_iter()
            .map(|g| Global {
                name: g.name,
                r#type: g.r#type.into(),
                value: g.init,
            })
            .collect();

        let mut functions = Vec::new();
        let mut external_functions = Vec::new();

        for declaration in program.functions.into_iter() {
            match declaration.body {
                FunctionBody::Extern => {
                    let function = ExternalFunction {
                        name: declaration.name,
                        return_type: declaration.return_type.into(),
                        parameters: declaration
                            .parameters
                            .into_iter()
                            .map(Variable::from)
                            .collect(),
                    };
                    external_functions.push(function);
                }
                FunctionBody::Block(body) => {
                    let return_type = declaration.return_type.into();
                    let function = Function {
                        name: declaration.name,
                        local_variables: emit_variables(&body),
                        parameters: declaration
                            .parameters
                            .into_iter()
                            .map(Variable::from)
                            .collect(),
                        instructions: {
                            let mut instructions = Vec::new();
                            emit_block(body, &mut instructions);
                            emit_return_if_needed(return_type, &mut instructions);
                            instructions
                        },
                        return_type,
                    };
                    functions.push(function);
                }
            }
        }

        Module {
            globals,
            functions,
            external_functions,
        }
    }
}

fn emit_variables(block: &Block) -> Vec<Variable> {
    let mut variables = Vec::new();
    let mut names = BTreeSet::new();
    emit_variables_in_block(block, &mut variables, &mut names);
    variables
}

fn emit_variables_in_block(
    block: &Block,
    variables: &mut Vec<Variable>,
    names: &mut BTreeSet<Rc<String>>,
) {
    for item in &block.items {
        match item {
            BlockItem::Statement(statement) => {
                emit_variables_in_statement(statement, variables, names)
            }
            BlockItem::VariableDeclaration(declaration) => {
                emit_variables_in_declaration(declaration, variables, names)
            }
        }
    }
}

fn emit_variables_in_statement(
    statement: &Statement,
    variables: &mut Vec<Variable>,
    names: &mut BTreeSet<Rc<String>>,
) {
    match statement {
        Statement::If(_, then, r#else) => {
            emit_variables_in_statement(then.as_ref(), variables, names);
            if let Some(r#else) = r#else.as_deref() {
                emit_variables_in_statement(r#else, variables, names)
            }
        }
        Statement::Compound(block) => emit_variables_in_block(block, variables, names),
        Statement::While(_, _, statement) | Statement::DoWhile(_, statement, _) => {
            emit_variables_in_statement(statement, variables, names)
        }
        Statement::For(_, for_init, _, _, statement) => {
            if let Some(ForInit::Declaration(declaration)) = for_init {
                emit_variables_in_declaration(declaration, variables, names);
            }
            emit_variables_in_statement(statement, variables, names);
        }
        Statement::Return(_)
        | Statement::Expression(_)
        | Statement::Break(_)
        | Statement::Continue(_)
        | Statement::Null => {}
    }
}

fn emit_variables_in_declaration(
    declaration: &VariableDeclaration,
    variables: &mut Vec<Variable>,
    names: &mut BTreeSet<Rc<String>>,
) {
    if !names.contains(&declaration.name) {
        let variable = Variable {
            name: declaration.name.clone(),
            r#type: declaration.r#type.into(),
        };
        variables.push(variable);
        names.insert(declaration.name.clone());
    }
}

fn emit_block(block: Block, instructions: &mut Vec<Instruction>) {
    block
        .items
        .into_iter()
        .for_each(|item| emit_block_item(item, instructions));
}

fn emit_block_item(block_item: BlockItem, instructions: &mut Vec<Instruction>) {
    match block_item {
        BlockItem::Statement(statement) => emit_statement(statement, instructions),
        BlockItem::VariableDeclaration(declaration) => {
            emit_variable_declaration(declaration, instructions)
        }
    }
}

fn emit_variable_declaration(
    declaration: VariableDeclaration,
    instructions: &mut Vec<Instruction>,
) {
    if let Some(init) = declaration.init {
        emit_expression(init, instructions);
        instructions.push(Instruction::LocalSet(declaration.name));
    }
}

fn emit_statement(statement: Statement, instructions: &mut Vec<Instruction>) {
    match statement {
        Statement::Return(expression) => {
            emit_expression(expression, instructions);
            instructions.push(Instruction::Return);
        }
        Statement::Expression(expression) => emit_expression_as_statement(expression, instructions),
        Statement::If(condition, then, r#else) => {
            emit_expression(condition, instructions);
            instructions.push(Instruction::If);
            emit_statement(*then, instructions);

            if let Some(r#else) = r#else {
                instructions.push(Instruction::Else);
                emit_statement(*r#else, instructions);
            }

            instructions.push(Instruction::End);
        }
        Statement::Compound(block) => {
            for item in block.items.into_iter() {
                emit_block_item(item, instructions);
            }
        }
        Statement::Break(label) => {
            let break_label = emit_label(&label, BREAK_LABEL);
            instructions.push(Instruction::Branch(break_label));
        }
        Statement::Continue(label) => {
            let continue_label = emit_label(&label, CONTINUE_LABEL);
            instructions.push(Instruction::Branch(continue_label));
        }
        Statement::While(label, condition, body) => {
            let (continue_label, break_label) =
                emit_loop_start(&label, CONTINUE_LABEL, instructions);
            emit_expression(condition, instructions);
            instructions.push(Instruction::Eqz);
            instructions.push(Instruction::BranchIf(break_label));
            emit_statement(*body, instructions);
            instructions.push(Instruction::Branch(continue_label));
            emit_loop_end(instructions);
        }
        Statement::DoWhile(label, body, condition) => {
            let (post_label, _) = emit_loop_start(&label, POST_LABEL, instructions);
            let continue_label = emit_label(&label, CONTINUE_LABEL);
            instructions.push(Instruction::Block(continue_label));
            emit_statement(*body, instructions);
            instructions.push(Instruction::End);
            emit_expression(condition, instructions);
            instructions.push(Instruction::BranchIf(post_label));
            emit_loop_end(instructions);
        }
        Statement::For(label, for_init, condition, post, body) => {
            if let Some(for_init) = for_init {
                emit_for_init(for_init, instructions);
            }
            let (post_label, break_label) = emit_loop_start(&label, POST_LABEL, instructions);
            let continue_label = emit_label(&label, CONTINUE_LABEL);
            instructions.push(Instruction::Block(continue_label));

            if let Some(condition) = condition {
                emit_expression(condition, instructions);
                instructions.push(Instruction::Eqz);
                instructions.push(Instruction::BranchIf(break_label));
            }

            emit_statement(*body, instructions);

            instructions.push(Instruction::End);

            if let Some(post) = post {
                emit_expression_as_statement(post, instructions);
            }

            instructions.push(Instruction::Branch(post_label));
            emit_loop_end(instructions);
        }
        Statement::Null => instructions.push(Instruction::Nop),
    }
}

fn emit_loop_start(
    label: &str,
    continue_label: &str,
    instructions: &mut Vec<Instruction>,
) -> (Rc<String>, Rc<String>) {
    let continue_label = emit_label(label, continue_label);
    let break_label = emit_label(label, BREAK_LABEL);
    instructions.push(Instruction::Loop(continue_label.clone()));
    instructions.push(Instruction::Block(break_label.clone()));
    (continue_label, break_label)
}

fn emit_loop_end(instructions: &mut Vec<Instruction>) {
    instructions.push(Instruction::End);
    instructions.push(Instruction::End);
}

fn emit_for_init(for_init: ForInit, instructions: &mut Vec<Instruction>) {
    match for_init {
        ForInit::Declaration(declaration) => emit_variable_declaration(declaration, instructions),
        ForInit::Expression(expression) => emit_expression_as_statement(expression, instructions),
    }
}

fn emit_expression_as_statement(expression: Expression, instructions: &mut Vec<Instruction>) {
    emit_expression(expression, instructions);
    instructions.push(Instruction::Drop);
}

fn emit_expression(expression: Expression, instructions: &mut Vec<Instruction>) {
    match expression {
        Expression::Constant(value) => instructions.push(Instruction::PushConstant(value)),
        Expression::Global(name) => instructions.push(Instruction::GlobalGet(name)),
        Expression::Variable(name) => instructions.push(Instruction::LocalGet(name)),
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
        Expression::PrefixIncrement(expression) => {
            emit_prefix_op(*expression, Instruction::Add, instructions);
        }
        Expression::PrefixDecrement(expression) => {
            emit_prefix_op(*expression, Instruction::Sub, instructions);
        }
        Expression::PostfixIncrement(expression) => {
            emit_postfix_op(*expression, Instruction::Add, instructions);
        }
        Expression::PostfixDecrement(expression) => {
            emit_postfix_op(*expression, Instruction::Sub, instructions);
        }
        Expression::BinaryOp(BinaryOp::Add, left, right) => {
            emit_op(*left, *right, Instruction::Add, instructions);
        }
        Expression::BinaryOp(BinaryOp::Subtract, left, right) => {
            emit_op(*left, *right, Instruction::Sub, instructions);
        }
        Expression::BinaryOp(BinaryOp::Multiply, left, right) => {
            emit_op(*left, *right, Instruction::Mul, instructions);
        }
        Expression::BinaryOp(BinaryOp::Divide, left, right) => {
            emit_op(*left, *right, Instruction::Div, instructions);
        }
        Expression::BinaryOp(BinaryOp::Remainder, left, right) => {
            emit_op(*left, *right, Instruction::Rem, instructions);
        }
        Expression::BinaryOp(BinaryOp::BitwiseAnd, left, right) => {
            emit_op(*left, *right, Instruction::And, instructions);
        }
        Expression::BinaryOp(BinaryOp::BitwiseOr, left, right) => {
            emit_op(*left, *right, Instruction::Or, instructions);
        }
        Expression::BinaryOp(BinaryOp::Xor, left, right) => {
            emit_op(*left, *right, Instruction::Xor, instructions);
        }
        Expression::BinaryOp(BinaryOp::LeftShift, left, right) => {
            emit_op(*left, *right, Instruction::ShiftLeft, instructions);
        }
        Expression::BinaryOp(BinaryOp::RightShift, left, right) => {
            emit_op(*left, *right, Instruction::ShiftRight, instructions);
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
            emit_op(*left, *right, Instruction::Eq, instructions);
        }
        Expression::BinaryOp(BinaryOp::NotEqualTo, left, right) => {
            emit_op(*left, *right, Instruction::Ne, instructions);
        }
        Expression::BinaryOp(BinaryOp::LessThan, left, right) => {
            emit_op(*left, *right, Instruction::Lt, instructions);
        }
        Expression::BinaryOp(BinaryOp::LessThanOrEqualTo, left, right) => {
            emit_op(*left, *right, Instruction::Le, instructions);
        }
        Expression::BinaryOp(BinaryOp::GreaterThan, left, right) => {
            emit_op(*left, *right, Instruction::Gt, instructions);
        }
        Expression::BinaryOp(BinaryOp::GreaterThanOrEqualTo, left, right) => {
            emit_op(*left, *right, Instruction::Ge, instructions);
        }
        Expression::BinaryOp(BinaryOp::Assignment, left, right) => {
            emit_expression(*right, instructions);
            emit_lvalue_tee((*left).into(), instructions);
        }
        Expression::BinaryOp(BinaryOp::AddAssignment, left, right) => {
            emit_assignment_op(*left, *right, Instruction::Add, instructions);
        }
        Expression::BinaryOp(BinaryOp::SubtractAssignment, left, right) => {
            emit_assignment_op(*left, *right, Instruction::Sub, instructions);
        }
        Expression::BinaryOp(BinaryOp::MultiplyAssignment, left, right) => {
            emit_assignment_op(*left, *right, Instruction::Mul, instructions);
        }
        Expression::BinaryOp(BinaryOp::DivideAssignment, left, right) => {
            emit_assignment_op(*left, *right, Instruction::Div, instructions);
        }
        Expression::BinaryOp(BinaryOp::RemainderAssignment, left, right) => {
            emit_assignment_op(*left, *right, Instruction::Rem, instructions);
        }
        Expression::BinaryOp(BinaryOp::BitwiseAndAssignment, left, right) => {
            emit_assignment_op(*left, *right, Instruction::And, instructions);
        }
        Expression::BinaryOp(BinaryOp::BitwiseOrAssignment, left, right) => {
            emit_assignment_op(*left, *right, Instruction::Or, instructions);
        }
        Expression::BinaryOp(BinaryOp::XorAssignment, left, right) => {
            emit_assignment_op(*left, *right, Instruction::Xor, instructions);
        }
        Expression::BinaryOp(BinaryOp::LeftShiftAssignment, left, right) => {
            emit_assignment_op(*left, *right, Instruction::ShiftLeft, instructions);
        }
        Expression::BinaryOp(BinaryOp::RightShiftAssignment, left, right) => {
            emit_assignment_op(*left, *right, Instruction::ShiftRight, instructions);
        }
        Expression::Conditional(condition, then, r#else) => {
            emit_expression(*condition, instructions);
            instructions.push(Instruction::IfWithResult);
            emit_expression(*then, instructions);
            instructions.push(Instruction::Else);
            emit_expression(*r#else, instructions);
            instructions.push(Instruction::End);
        }
        Expression::FunctionCall(name, arguments) => {
            arguments
                .into_iter()
                .for_each(|a| emit_expression(a, instructions));
            instructions.push(Instruction::Call(name));
        }
    }
}

fn emit_prefix_op(expression: Expression, op: Instruction, instructions: &mut Vec<Instruction>) {
    let lvalue: Lvalue = expression.into();
    emit_lvalue_get(lvalue.clone(), instructions);
    instructions.push(Instruction::PushConstant(1));
    instructions.push(op);
    emit_lvalue_tee(lvalue, instructions);
}

fn emit_postfix_op(expression: Expression, op: Instruction, instructions: &mut Vec<Instruction>) {
    let lvalue: Lvalue = expression.into();
    emit_lvalue_get(lvalue.clone(), instructions);
    emit_lvalue_get(lvalue.clone(), instructions);
    instructions.push(Instruction::PushConstant(1));
    instructions.push(op);
    emit_lvalue_set(lvalue.clone(), instructions);
}

fn emit_op(
    left: Expression,
    right: Expression,
    op: Instruction,
    instructions: &mut Vec<Instruction>,
) {
    emit_expression(left, instructions);
    emit_expression(right, instructions);
    instructions.push(op);
}

fn emit_assignment_op(
    left: Expression,
    right: Expression,
    op: Instruction,
    instructions: &mut Vec<Instruction>,
) {
    let lvalue: Lvalue = left.into();
    emit_lvalue_get(lvalue.clone(), instructions);
    emit_expression(right, instructions);
    instructions.push(op);
    emit_lvalue_tee(lvalue, instructions);
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

fn emit_lvalue_get(lvalue: Lvalue, instructions: &mut Vec<Instruction>) {
    match lvalue {
        Lvalue::Global(name) => instructions.push(Instruction::GlobalGet(name)),
        Lvalue::Variable(name) => instructions.push(Instruction::LocalGet(name)),
    }
}

fn emit_lvalue_set(lvalue: Lvalue, instructions: &mut Vec<Instruction>) {
    match lvalue {
        Lvalue::Global(name) => instructions.push(Instruction::GlobalSet(name.clone())),
        Lvalue::Variable(name) => instructions.push(Instruction::LocalSet(name)),
    }
}

fn emit_lvalue_tee(lvalue: Lvalue, instructions: &mut Vec<Instruction>) {
    match lvalue {
        Lvalue::Global(name) => {
            instructions.push(Instruction::GlobalSet(name.clone()));
            instructions.push(Instruction::GlobalGet(name));
        }
        Lvalue::Variable(name) => instructions.push(Instruction::LocalTee(name)),
    }
}

fn emit_label(identifier: &str, r#type: &str) -> Rc<String> {
    let mut label = String::with_capacity(identifier.len() + r#type.len() + 1);
    label.push_str(identifier);
    label.push('.');
    label.push_str(r#type);
    label.into()
}

impl From<Expression> for Lvalue {
    fn from(value: Expression) -> Self {
        match value {
            Expression::Global(name) => Lvalue::Global(name),
            Expression::Variable(name) => Lvalue::Variable(name),
            _ => unreachable!(),
        }
    }
}

impl From<AstType> for IrType {
    fn from(value: AstType) -> Self {
        match value {
            AstType::Int => IrType::Int32,
            AstType::Void => IrType::Void,
        }
    }
}

impl From<FunctionParameter> for Variable {
    fn from(value: FunctionParameter) -> Self {
        Self {
            name: value.name,
            r#type: value.r#type.into(),
        }
    }
}
