use crate::{
    ir::{Function, Instruction, Module, Type},
    Error,
};
use alloc::string::{String, ToString};
use core::mem;

#[derive(Clone, Default, Debug)]
pub struct Wat {
    out: String,
    level: usize,
}

impl Wat {
    pub fn generate(&mut self, module: Module) -> Result<String, Error> {
        self.out = String::new();
        self.out.push_str(
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
                self.generate_function(function);
                if index < last {
                    self.out.push('\n');
                }
            });

        self.out.push(')');

        Ok(mem::take(&mut self.out))
    }
}

impl Wat {
    fn generate_function(&mut self, function: Function) {
        self.out.push_str(r#"  (func $"#);
        self.out.push_str(&function.name);

        if function.return_type != Type::Void {
            self.out.push_str(" (result ");
            self.generate_type(function.return_type);
            self.out.push_str(")\n");
        }

        for variable in &function.local_variables {
            self.out.push_str("    (local $");
            self.out.push_str(&variable.name);

            match variable.r#type {
                Type::Int32 => self.out.push_str(" i32)\n"),
                Type::Void => unreachable!(),
            }
        }

        let last = function.instructions.len().saturating_sub(1);
        self.level = 2;

        function
            .instructions
            .into_iter()
            .enumerate()
            .for_each(|(index, instruction)| {
                self.generate_instruction(instruction);
                if index < last {
                    self.out.push('\n');
                }
            });

        self.out.push(')');
    }

    fn generate_instruction(&mut self, instruction: Instruction) {
        if matches!(instruction, Instruction::Else | Instruction::End) {
            self.level -= 1;
        }

        (0..self.level * 2).for_each(|_| self.out.push(' '));

        match instruction {
            Instruction::PushConstant(value) => {
                self.out.push_str("(i32.const ");
                self.out.push_str(&value.to_string());
                self.out.push(')');
            }
            Instruction::Add => self.out.push_str("i32.add"),
            Instruction::Sub => self.out.push_str("i32.sub"),
            Instruction::Mul => self.out.push_str("i32.mul"),
            Instruction::Div => self.out.push_str("i32.div_s"),
            Instruction::Rem => self.out.push_str("i32.rem_s"),
            Instruction::And => self.out.push_str("i32.and"),
            Instruction::Or => self.out.push_str("i32.or"),
            Instruction::Xor => self.out.push_str("i32.xor"),
            Instruction::ShiftLeft => self.out.push_str("i32.shl"),
            Instruction::ShiftRight => self.out.push_str("i32.shr_s"),
            Instruction::Eq => self.out.push_str("i32.eq"),
            Instruction::Eqz => self.out.push_str("i32.eqz"),
            Instruction::Ne => self.out.push_str("i32.ne"),
            Instruction::Lt => self.out.push_str("i32.lt_s"),
            Instruction::Le => self.out.push_str("i32.le_s"),
            Instruction::Gt => self.out.push_str("i32.gt_s"),
            Instruction::Ge => self.out.push_str("i32.ge_s"),
            Instruction::IfWithResult => {
                self.out.push_str("if (result i32)");
                self.level += 1;
            }
            Instruction::Else => {
                self.out.push_str("else");
                self.level += 1;
            }
            Instruction::End => self.out.push_str("end"),
            Instruction::Select => self.out.push_str("select"),
            Instruction::Return => self.out.push_str("return"),
            Instruction::LocalGet(name) => {
                self.out.push_str("(local.get $");
                self.out.push_str(&name);
                self.out.push(')');
            }
            Instruction::LocalSet(name) => {
                self.out.push_str("(local.set $");
                self.out.push_str(&name);
                self.out.push(')');
            }
            Instruction::LocalTee(name) => {
                self.out.push_str("(local.tee $");
                self.out.push_str(&name);
                self.out.push(')');
            }
            Instruction::Drop => self.out.push_str("drop"),
            Instruction::Nop => self.out.push_str("nop"),
        }
    }

    fn generate_type(&mut self, r#type: Type) {
        match r#type {
            Type::Int32 => self.out.push_str("i32"),
            Type::Void => unreachable!(),
        }
    }
}
