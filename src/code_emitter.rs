use std::{fs, path::PathBuf, process};

use crate::asm_gen::{AsmBinaryOperator, AsmFunctionDef, AsmInstruction, AsmOperand, AsmProgram, AsmUnaryOperator, Reg};

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
        AsmInstruction::Binary(AsmBinaryOperator::Add, operand_1, operand_2) => lines.push(format!("
            addl {}, {}
        ", translate_operand(operand_1), translate_operand(operand_2))),
        AsmInstruction::Binary(AsmBinaryOperator::Sub, operand_1, operand_2) => lines.push(format!("
            subl {}, {}
        ", translate_operand(operand_1), translate_operand(operand_2))),
        AsmInstruction::Binary(AsmBinaryOperator::Mult, operand_1, operand_2) => lines.push(format!("
            imull {}, {}
        ", translate_operand(operand_1), translate_operand(operand_2))),
        AsmInstruction::Cdq => lines.push(format!("
            cdq
        ")),
        AsmInstruction::Idiv(operand) => lines.push(format!("
            idivl {}
        ", translate_operand(operand))),
    }
}

fn translate_operand(operand: AsmOperand) -> String {
    match operand {
        AsmOperand::Register(Reg::AX) => format!("%eax"),
        AsmOperand::Register(Reg::DX) => format!("%edx"),
        AsmOperand::Register(Reg::R10) => format!("%r10d"),
        AsmOperand::Register(Reg::R11) => format!("%r11d"),
        AsmOperand::Stack(offset) => format!("{}(%rbp)", offset),
        AsmOperand::Imm(int_value) => format!("${}", int_value),
        AsmOperand::Pseudo(_) => process::exit(7)
    }
}
