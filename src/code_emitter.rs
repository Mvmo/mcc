use std::{fs, path::PathBuf, process};

use crate::{asm_gen::{AsmFunctionDef, AsmInstruction, AsmProgram, Operand}, parser::Program};

pub fn emit(asm_program: AsmProgram, to: PathBuf) {
    let mut lines = Vec::new();
    write_program(&mut lines, asm_program);

    if fs::write(to, lines.join("\n")).is_err() {
        process::exit(4);
    }
}

fn write_program(lines: &mut Vec<String>, program: AsmProgram) {
    write_function_def(lines, program.function_definition);
}

fn write_function_def(lines: &mut Vec<String>, function_def: AsmFunctionDef) {
    lines.push(format!(".globl _{}", function_def.name));
    lines.push(format!("_{}:", function_def.name));
    for instruction in function_def.instructions {
        write_instruction(lines, instruction);
    }
}

fn write_instruction(lines: &mut Vec<String>, instruction: AsmInstruction) {
    match instruction {
        AsmInstruction::Mov(src, dest) => {
            lines.push(format!("movl {}, {}", translate_operand(src), translate_operand(dest)));
        },
        AsmInstruction::Ret => {
            lines.push(format!("ret"));
        }
    }
}

fn translate_operand(operand: Operand) -> String {
    match operand {
        Operand::Imm(int_value) => format!("${}", int_value),
        Operand::Register => format!("%eax"),
    }
}
