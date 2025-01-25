// Copyright (c) 2025 Ludovico Rossi
// SPDX-License-Identifier: MPL-2.0

use crate::ir::{Datum, ExternalFunction, Function, Global, Instruction, Module, Type};
use alloc::string::{String, ToString};
use core::{fmt::Write, mem};

const DEFAULT_EXTERNAL_MODULE: &str = "env";
const PARAM_VARIABLE_TYPE: &str = "param";
const LOCAL_VARIABLE_TYPE: &str = "local";

#[derive(Clone, Default, Debug)]
pub struct Wat {
    out: String,
    level: usize,
}

impl Wat {
    pub fn generate(&mut self, module: Module) -> String {
        self.out = String::new();
        self.out.push_str(
            r#"(module
  (import "env" "memory" (memory 1))
  (export "main" (func $main))
"#,
        );

        module.external_functions.into_iter().for_each(|function| {
            self.generate_external_function(function);
            self.out.push('\n');
        });

        module.data.into_iter().for_each(|datum| {
            self.generate_datum(datum);
            self.out.push('\n');
        });

        module.globals.into_iter().for_each(|global| {
            self.generate_global(global);
            self.out.push('\n');
        });

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

        mem::take(&mut self.out)
    }
}

impl Wat {
    fn generate_datum(&mut self, datum: Datum) {
        self.out.push_str(r#"  (data  (i32.const "#);
        self.out.push_str(&datum.address.to_string());
        self.out.push_str(r#") ""#);

        for byte in datum.bytes {
            match byte {
                b' ' => self.out.push(' '),
                b'\t' => self.out.push_str("\\t"),
                b'\n' => self.out.push_str("\\n"),
                b'\r' => self.out.push_str("\\r"),
                b'\"' => self.out.push_str("\\\""),
                b'\'' => self.out.push_str("\\'"),
                b'\\' => self.out.push_str("\\\\"),
                _ if byte.is_ascii_graphic() => self.out.push(byte as char),
                _ => {
                    self.out.push('\\');
                    let mut value = String::with_capacity(2);
                    write!(&mut value, "{:02X}", byte).unwrap();
                    self.out.push_str(&value);
                }
            }
        }

        self.out.push_str(r#"")"#);
    }

    fn generate_global(&mut self, global: Global) {
        self.out.push_str(r#"  (global $"#);
        self.out.push_str(&global.name);

        match global.r#type {
            Type::Int32 => {
                if global.is_constant {
                    self.out.push_str(" i32")
                } else {
                    self.out.push_str(" (mut i32)")
                }
            }
            Type::Void => unreachable!(),
        }

        let value = global.value.unwrap_or_default();
        self.out.push_str(" (i32.const ");
        self.out.push_str(&value.to_string());
        self.out.push(')');

        self.out.push(')');
    }

    fn generate_external_function(&mut self, function: ExternalFunction) {
        self.out.push_str(r#"  (import ""#);
        self.out.push_str(DEFAULT_EXTERNAL_MODULE);
        self.out.push_str(r#"" ""#);
        self.out.push_str(&function.name);
        self.out.push_str(r#"" (func $"#);
        self.out.push_str(&function.name);
        self.out.push('\n');

        for parameter in &function.parameters {
            self.generate_variable(&parameter.name, parameter.r#type, PARAM_VARIABLE_TYPE);
        }

        self.generate_result(function.return_type);

        self.out.push_str("  ))");
    }

    fn generate_function(&mut self, function: Function) {
        self.out.push_str(r#"  (func $"#);
        self.out.push_str(&function.name);
        self.out.push('\n');

        for parameter in &function.parameters {
            self.generate_variable(&parameter.name, parameter.r#type, PARAM_VARIABLE_TYPE);
        }

        self.generate_result(function.return_type);

        for variable in &function.local_variables {
            self.generate_variable(&variable.name, variable.r#type, LOCAL_VARIABLE_TYPE);
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

    fn generate_variable(&mut self, name: &str, r#type: Type, kind: &str) {
        self.out.push_str("    (");
        self.out.push_str(kind);
        self.out.push_str(" $");
        self.out.push_str(name);

        match r#type {
            Type::Int32 => self.out.push_str(" i32)\n"),
            Type::Void => unreachable!(),
        }
    }

    fn generate_result(&mut self, return_type: Type) {
        if return_type != Type::Void {
            self.out.push_str("    (result ");
            self.generate_type(return_type);
            self.out.push_str(")\n");
        }
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
            Instruction::If => {
                self.out.push_str("if");
                self.level += 1;
            }
            Instruction::IfWithResult => {
                self.out.push_str("if (result i32)");
                self.level += 1;
            }
            Instruction::Else => {
                self.out.push_str("else");
                self.level += 1;
            }
            Instruction::Select => self.out.push_str("select"),
            Instruction::Return => self.out.push_str("return"),
            Instruction::GlobalGet(name) => {
                self.out.push_str("(global.get $");
                self.out.push_str(&name);
                self.out.push(')');
            }
            Instruction::GlobalSet(name) => {
                self.out.push_str("(global.set $");
                self.out.push_str(&name);
                self.out.push(')');
            }
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
            Instruction::Loop(label) => {
                self.out.push_str("loop $");
                self.out.push_str(&label);
                self.level += 1;
            }
            Instruction::Block(label) => {
                self.out.push_str("block $");
                self.out.push_str(&label);
                self.level += 1;
            }
            Instruction::Branch(label) => {
                self.out.push_str("(br $");
                self.out.push_str(&label);
                self.out.push(')');
            }
            Instruction::BranchIf(label) => {
                self.out.push_str("(br_if $");
                self.out.push_str(&label);
                self.out.push(')');
            }
            Instruction::Call(name) => {
                self.out.push_str("(call $");
                self.out.push_str(&name);
                self.out.push(')');
            }
            Instruction::End => self.out.push_str("end"),
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
