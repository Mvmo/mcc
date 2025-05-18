use crate::tacco_ir::{TaccoFunctionDef, TaccoInstruction, TaccoProgram, TaccoUnaryOperator, TaccoVal};

#[derive(Debug)]
pub struct AsmProgram {
    pub function_definition: AsmFunctionDef,
}

#[derive(Debug)]
pub struct AsmFunctionDef {
    pub name: String,
    pub instructions: Vec<AsmInstruction>
}

#[derive(Debug)]
pub enum AsmInstruction {
    Mov(AsmOperand, AsmOperand),
    Unary(AsmUnaryOperator, AsmOperand),
    Ret
}

#[derive(Debug)]
pub enum Reg {
    AX,
    R10,
}

#[derive(Debug)]
pub enum AsmOperand {
    Imm(i32),
    Register(Reg),
    Pseudo(String),
    Stack(i32),
}

#[derive(Debug)]
pub enum AsmUnaryOperator {
    Neg,
    Not
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
            )
        ]
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
