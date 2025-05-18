use crate::parser::{Expression, FunctionDef, Program, Statement};

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
    Mov(Operand, Operand),
    Ret
}

#[derive(Debug)]
pub enum Operand {
    Imm(i32),
    Register
}

pub fn generate(program: Program) -> AsmProgram {
    let function = translate_function_def(program.function_definition);
    return AsmProgram { function_definition: function }
}

fn translate_function_def(function_def: FunctionDef) -> AsmFunctionDef {
    return AsmFunctionDef { name: function_def.name, instructions: translate_statement(function_def.body) }
}

fn translate_statement(statement: Statement) -> Vec<AsmInstruction> {
    match statement {
        Statement::Return(expression) => {
            return vec![
                AsmInstruction::Mov(translate_expression(expression), Operand::Register),
                AsmInstruction::Ret,
            ]
        }
    }
}

fn translate_expression(expression: Expression) -> Operand {
    match expression {
        Expression::Const(value) => {
            Operand::Imm(value)
        }
    }
}
