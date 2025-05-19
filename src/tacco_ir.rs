use std::{process, sync::Mutex};

use crate::parser::{BinaryOperator, Expression, FunctionDef, Program, Statement, UnaryOperator};

#[derive(Debug, Clone)]
pub struct TaccoProgram {
    pub function_definition: TaccoFunctionDef,
}

#[derive(Debug, Clone)]
pub struct TaccoFunctionDef {
    pub identifier: String, // TODO rename to name
    pub body: Vec<TaccoInstruction>,
}

#[derive(Debug, Clone)]
pub enum TaccoInstruction {
    Return(TaccoVal),
    Unary {
        operator: TaccoUnaryOperator,
        src: TaccoVal,
        dest: TaccoVal,
    },
    Binary {
        operator: TaccoBinaryOperator,
        src_1: TaccoVal,
        src_2: TaccoVal,
        dest: TaccoVal,
    }
}

#[derive(Debug, Clone)]
pub enum TaccoVal {
    Constant(i32),
    Var(String),
}

#[derive(Debug, Clone)]
pub enum TaccoUnaryOperator {
    Complement,
    Negate,
}

#[derive(Debug, Clone)]
pub enum TaccoBinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,
}

pub fn transform(program: Program) -> TaccoProgram {
    return TaccoProgram { function_definition: transform_function_definition(program.function_definition) };
}

fn transform_function_definition(function_definition: FunctionDef) -> TaccoFunctionDef {
    let mut instructions = Vec::<TaccoInstruction>::new();
    emit_statement(function_definition.body, &mut instructions);
    return TaccoFunctionDef { identifier: function_definition.name, body: instructions }
}

fn emit_statement(statement: Statement, into: &mut Vec<TaccoInstruction>) {
    match statement {
        Statement::Return(expression) => {
            let value = emit_transform_expression(expression, into);
            into.push(TaccoInstruction::Return(value));
        }
    }
}

fn emit_transform_expression(expression: Expression, into: &mut Vec<TaccoInstruction>) -> TaccoVal {
    match expression {
        Expression::Const(int_value) => {
            return TaccoVal::Constant(int_value)
        },
        Expression::Unary { operator, inner_expression } => {
            let src = emit_transform_expression(inner_expression.as_ref().clone(), into);

            let dest_name = generate_temp_name();
            let dest = TaccoVal::Var(dest_name);

            let tacco_operator = transform_unary_operator(operator);

            into.push(TaccoInstruction::Unary{ operator: tacco_operator, src, dest: dest.clone() });

            return dest
        },
        Expression::Binary { operator, left, right } => {
            let src_1 = emit_transform_expression(left.as_ref().clone(), into);
            let src_2 = emit_transform_expression(right.as_ref().clone(), into);

            let dest_name = generate_temp_name();
            let dest = TaccoVal::Var(dest_name);

            let tacco_operator = transform_binary_operator(operator);

            into.push(TaccoInstruction::Binary{ operator: tacco_operator, src_1, src_2, dest: dest.clone() });

            return dest;
        }
    }
}

fn transform_binary_operator(binary_operator: BinaryOperator) -> TaccoBinaryOperator {
    return match binary_operator {
        BinaryOperator::Add => TaccoBinaryOperator::Add,
        BinaryOperator::Subtract => TaccoBinaryOperator::Subtract,
        BinaryOperator::Multiply => TaccoBinaryOperator::Multiply,
        BinaryOperator::Divide => TaccoBinaryOperator::Divide,
        BinaryOperator::Remainder => TaccoBinaryOperator::Remainder,
        BinaryOperator::BitwiseAnd => TaccoBinaryOperator::BitwiseAnd,
        BinaryOperator::BitwiseXor => TaccoBinaryOperator::BitwiseXor,
        BinaryOperator::BitwiseOr => TaccoBinaryOperator::BitwiseOr,
        BinaryOperator::BitwiseLeftShift => TaccoBinaryOperator::BitwiseLeftShift,
        BinaryOperator::BitwiseRightShift => TaccoBinaryOperator::BitwiseRightShift,
        _ => process::exit(5) // TODO remove
    }
}

fn transform_unary_operator(unary_operator: UnaryOperator) -> TaccoUnaryOperator {
    return match unary_operator {
        UnaryOperator::Complement => TaccoUnaryOperator::Complement,
        UnaryOperator::Negate => TaccoUnaryOperator::Negate,
        _ => process::exit(5) // TODO remove
    }
}

static TMP_COUNT: Mutex<i32> = Mutex::new(0);

fn generate_temp_name() -> String {
    let mut count = TMP_COUNT.lock()
        .expect("IR Tacco Transform | Couldn't lock the TMP_COUNTER");

    *count = *count + 1;
    return format!("temp.{}", *count)
}
