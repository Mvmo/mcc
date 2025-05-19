use std::process;

use crate::tacco_ir::{TaccoBinaryOperator, TaccoFunctionDef, TaccoInstruction, TaccoProgram, TaccoUnaryOperator, TaccoVal};

#[derive(Debug, Clone)]
pub struct AsmProgram {
    pub function_definition: AsmFunctionDef,
}

#[derive(Debug, Clone)]
pub struct AsmFunctionDef {
    pub name: String,
    pub instructions: Vec<AsmInstruction>
}

#[derive(Debug, Clone)]
pub enum AsmInstruction {
    Mov(AsmOperand, AsmOperand),
    Unary(AsmUnaryOperator, AsmOperand),
    Binary(AsmBinaryOperator, AsmOperand, AsmOperand),
    Idiv(AsmOperand),
    Cdq,
    AllocateStack(i32),
    Ret
}

#[derive(Debug, Clone)]
pub enum Reg {
    AX,
    DX,
    R10,
    R11,
}

#[derive(Debug, Clone)]
pub enum AsmOperand {
    Imm(i32),
    Register(Reg),
    Pseudo(String),
    Stack(i32),
}

#[derive(Debug, Clone)]
pub enum AsmUnaryOperator {
    Neg,
    Not
}

#[derive(Debug, Clone)]
pub enum AsmBinaryOperator {
    Add,
    Sub,
    Mult
}

pub fn generate(program: TaccoProgram) -> AsmProgram {
    let function = translate_function_def(program.function_definition);
    return AsmProgram { function_definition: function }
}

fn translate_function_def(function_def: TaccoFunctionDef) -> AsmFunctionDef {
    let instructions: Vec<AsmInstruction> = function_def.body.iter()
        .flat_map(translate_instruction)
        .collect();

    return AsmFunctionDef { name: function_def.identifier, instructions }
}

fn translate_instruction(instruction: &TaccoInstruction) -> Vec<AsmInstruction> {
    return match instruction {
        TaccoInstruction::Return(value) => vec![
            AsmInstruction::Mov(
                translate_operand(value),
                AsmOperand::Register(Reg::AX)
            ),
            AsmInstruction::Ret
        ],
        TaccoInstruction::Unary { operator, src, dest } => vec![
            AsmInstruction::Mov(
                translate_operand(src),
                translate_operand(dest),
            ),
            AsmInstruction::Unary(
                translate_unary_operator(operator),
                translate_operand(dest),
            ),
        ],
        TaccoInstruction::Binary { operator: TaccoBinaryOperator::Divide, src_1, src_2, dest } => vec![
            AsmInstruction::Mov(
                translate_operand(src_1),
                AsmOperand::Register(Reg::AX),
            ),
            AsmInstruction::Cdq,
            AsmInstruction::Idiv(
                translate_operand(src_2),
            ),
            AsmInstruction::Mov(
                AsmOperand::Register(Reg::AX),
                translate_operand(dest)
            ),
        ],
        TaccoInstruction::Binary { operator: TaccoBinaryOperator::Remainder, src_1, src_2, dest } => vec![
            AsmInstruction::Mov(
                translate_operand(src_1),
                AsmOperand::Register(Reg::AX),
            ),
            AsmInstruction::Cdq,
            AsmInstruction::Idiv(
                translate_operand(src_2),
            ),
            AsmInstruction::Mov(
                AsmOperand::Register(Reg::DX),
                translate_operand(dest)
            ),
        ],
        TaccoInstruction::Binary { operator, src_1, src_2, dest } => vec![
            AsmInstruction::Mov(
                translate_operand(src_1),
                translate_operand(dest),
            ),
            AsmInstruction::Binary(
                translate_binary_operator(operator),
                translate_operand(src_2),
                translate_operand(dest),
            )
        ],
    }
}

fn translate_operand(value: &TaccoVal) -> AsmOperand {
    return match value {
        TaccoVal::Constant(int_value) => AsmOperand::Imm(*int_value),
        TaccoVal::Var(identifier) => AsmOperand::Pseudo(identifier.clone())
    }
}

fn translate_unary_operator(operator: &TaccoUnaryOperator) -> AsmUnaryOperator {
    return match operator {
        TaccoUnaryOperator::Complement => AsmUnaryOperator::Not,
        TaccoUnaryOperator::Negate => AsmUnaryOperator::Neg,
    }
}

fn translate_binary_operator(operator: &TaccoBinaryOperator) -> AsmBinaryOperator {
    return match operator {
        TaccoBinaryOperator::Add => AsmBinaryOperator::Add,
        TaccoBinaryOperator::Subtract => AsmBinaryOperator::Sub,
        TaccoBinaryOperator::Multiply => AsmBinaryOperator::Mult,
        _ => process::exit(7),
    }
}
