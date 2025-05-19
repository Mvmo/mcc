use std::{fs, ops::Neg, path::PathBuf, process};

use crate::asm_gen::{AsmFunctionDef, AsmInstruction, AsmOperand, AsmProgram, AsmUnaryOperator, Reg};

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
    lines.push(format!("
            .globl _{0}
        _{0}:
            pushq %rbp
            movq %rsp, %rbp
    ", function_def.name));

    for instruction in function_def.instructions {
        write_instruction(lines, instruction);
    }
}

fn write_instruction(lines: &mut Vec<String>, instruction: AsmInstruction) {
    match instruction {
        AsmInstruction::Mov(src, dest) => lines.push(format!("
            movl {}, {}
        ", translate_operand(src), translate_operand(dest))),
        AsmInstruction::Ret => lines.push(format!("
            movq %rbp, %rsp
            popq %rbp
            ret
        ")),
        AsmInstruction::AllocateStack(size) => lines.push(format!("
            subq ${}, %rsp
        ", size)),
        AsmInstruction::Unary(AsmUnaryOperator::Neg, operand) => lines.push(format!("
            negl {}
        ", translate_operand(operand))),
        AsmInstruction::Unary(AsmUnaryOperator::Not, operand) => lines.push(format!("
            notl {}
        ", translate_operand(operand))),
    }
}

fn translate_operand(operand: AsmOperand) -> String {
    match operand {
        AsmOperand::Register(Reg::AX) => format!("%eax"),
        AsmOperand::Register(Reg::R10) => format!("%r10d"),
        AsmOperand::Stack(offset) => format!("{}(%rbp)", offset),
        AsmOperand::Imm(int_value) => format!("${}", int_value),
        AsmOperand::Pseudo(_) => process::exit(7)
    }
}
