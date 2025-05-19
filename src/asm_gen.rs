use std::process;

use crate::{parser::BinaryOperator, tacco_ir::{TaccoBinaryOperator, TaccoFunctionDef, TaccoInstruction, TaccoProgram, TaccoUnaryOperator, TaccoVal}};

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
    Cmp(AsmOperand, AsmOperand),
    Idiv(AsmOperand),
    Cdq,
    Jmp(String),
    JmpCC(AsmCondCode, String),
    SetCC(AsmCondCode, AsmOperand),
    Label(String),
    AllocateStack(i32),
    Ret
}

#[derive(Debug, Clone)]
pub enum AsmCondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE
}

#[derive(Debug, Clone)]
pub enum Reg {
    AX,
    BX,
    CX,
    DX,
    R10,
    R11,
    CL,
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
    Mult,
    And,
    Or,
    Xor,
    Shl,
    Shr,
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
        TaccoInstruction::Binary { operator: TaccoBinaryOperator::Equal, src_1, src_2, dest } => vec![
            AsmInstruction::Cmp(
                translate_operand(src_2),
                translate_operand(src_1),
            ),
            AsmInstruction::Mov(AsmOperand::Imm(0), translate_operand(dest)),
            AsmInstruction::SetCC(AsmCondCode::E, translate_operand(dest)),
        ],
        TaccoInstruction::Binary { operator: TaccoBinaryOperator::NotEqual, src_1, src_2, dest } => vec![
            AsmInstruction::Cmp(
                translate_operand(src_2),
                translate_operand(src_1),
            ),
            AsmInstruction::Mov(AsmOperand::Imm(0), translate_operand(dest)),
            AsmInstruction::SetCC(AsmCondCode::NE, translate_operand(dest)),
        ],
        TaccoInstruction::Binary { operator: TaccoBinaryOperator::LessThan, src_1, src_2, dest } => vec![
            AsmInstruction::Cmp(
                translate_operand(src_2),
                translate_operand(src_1),
            ),
            AsmInstruction::Mov(AsmOperand::Imm(0), translate_operand(dest)),
            AsmInstruction::SetCC(AsmCondCode::L, translate_operand(dest)),
        ],
        TaccoInstruction::Binary { operator: TaccoBinaryOperator::LessThanOrEqual, src_1, src_2, dest } => vec![
            AsmInstruction::Cmp(
                translate_operand(src_2),
                translate_operand(src_1),
            ),
            AsmInstruction::Mov(AsmOperand::Imm(0), translate_operand(dest)),
            AsmInstruction::SetCC(AsmCondCode::LE, translate_operand(dest)),
        ],
        TaccoInstruction::Binary { operator: TaccoBinaryOperator::GreaterThan, src_1, src_2, dest } => vec![
            AsmInstruction::Cmp(
                translate_operand(src_2),
                translate_operand(src_1),
            ),
            AsmInstruction::Mov(AsmOperand::Imm(0), translate_operand(dest)),
            AsmInstruction::SetCC(AsmCondCode::G, translate_operand(dest)),
        ],
        TaccoInstruction::Binary { operator: TaccoBinaryOperator::GreaterThanOrEqual, src_1, src_2, dest } => vec![
            AsmInstruction::Cmp(
                translate_operand(src_2),
                translate_operand(src_1),
            ),
            AsmInstruction::Mov(AsmOperand::Imm(0), translate_operand(dest)),
            AsmInstruction::SetCC(AsmCondCode::GE, translate_operand(dest)),
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
        TaccoInstruction::Jump(target) => vec![
            AsmInstruction::Jmp(target.clone()),
        ],
        TaccoInstruction::Copy(condition, target) => vec![
            AsmInstruction::Mov(
                translate_operand(condition),
                translate_operand(target),
            )
        ],
        TaccoInstruction::Label(identifier) => vec![
            AsmInstruction::Label(identifier.clone()),
        ],
        TaccoInstruction::JumpIfZero(val, target) => vec![
            AsmInstruction::Cmp(AsmOperand::Imm(0), translate_operand(val)),
            AsmInstruction::JmpCC(AsmCondCode::E, target.clone()),
        ],
        TaccoInstruction::JumpIfNotZero(val, target) => vec![
            AsmInstruction::Cmp(AsmOperand::Imm(0), translate_operand(val)),
            AsmInstruction::JmpCC(AsmCondCode::NE, target.clone()),
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
        TaccoUnaryOperator::Not => AsmUnaryOperator::Not,
    }
}

fn translate_binary_operator(operator: &TaccoBinaryOperator) -> AsmBinaryOperator {
    return match operator {
        TaccoBinaryOperator::Add => AsmBinaryOperator::Add,
        TaccoBinaryOperator::Subtract => AsmBinaryOperator::Sub,
        TaccoBinaryOperator::Multiply => AsmBinaryOperator::Mult,
        TaccoBinaryOperator::BitwiseAnd => AsmBinaryOperator::And,
        TaccoBinaryOperator::BitwiseXor => AsmBinaryOperator::Xor,
        TaccoBinaryOperator::BitwiseOr => AsmBinaryOperator::Or,
        TaccoBinaryOperator::BitwiseLeftShift => AsmBinaryOperator::Shl,
        TaccoBinaryOperator::BitwiseRightShift => AsmBinaryOperator::Shr,
        _ => process::exit(7),
    }
}
