use crate::{
    ir::{Function, Instruction, Module, Type},
    Error,
};
use alloc::string::{String, ToString};

#[derive(Clone, Debug)]
pub struct Wat;

impl Wat {
    pub fn generate(&self, module: Module) -> Result<String, Error> {
        let mut wat = String::new();
        wat.push_str(
            r#"(module
  (import "env" "memory" (memory 1))
  (export "main" (func $main))
"#,
        );

        let last = module.functions.len().saturating_sub(1);

        module
            .functions
            .into_iter()
            .enumerate()
            .for_each(|(index, function)| {
                generate_function(function, &mut wat);
                if index < last {
                    wat.push('\n');
                }
            });

        wat.push(')');

        Ok(wat)
    }
}

fn generate_function(function: Function, wat: &mut String) {
    wat.push_str(r#"  (func $"#);
    wat.push_str(&function.name);

    if function.return_type != Type::Void {
        wat.push_str(" (result ");
        generate_type(function.return_type, wat);
        wat.push_str(")\n");
    }

    let last = function.instructions.len().saturating_sub(1);

    function
        .instructions
        .into_iter()
        .enumerate()
        .for_each(|(index, instruction)| {
            generate_instruction(instruction, wat);
            if index < last {
                wat.push('\n');
            }
        });

    wat.push(')');
}

fn generate_instruction(instruction: Instruction, wat: &mut String) {
    match instruction {
        Instruction::PushConstant(value) => {
            wat.push_str("    (i32.const ");
            wat.push_str(&value.to_string());
            wat.push(')');
        }
        Instruction::Xor => wat.push_str("    (i32.xor)"),
        Instruction::Sub => wat.push_str("    (i32.sub)"),
        Instruction::Return => wat.push_str("    (return)"),
    }
}

fn generate_type(r#type: Type, wat: &mut String) {
    match r#type {
        Type::Int32 => wat.push_str("i32"),
        Type::Void => unreachable!(),
    }
}
