use std::{process, sync::Mutex};

use crate::{lexer::Token, parser::{Expression, FunctionDef, Program, Statement}};

pub struct TaccoProgram {
    pub function_definition: TaccoFunctionDef,
}

pub struct TaccoFunctionDef {
    pub identifier: String, // TODO rename to name
    pub body: Vec<TaccoInstruction>,
}

pub enum TaccoInstruction {
    Return(TaccoVal),
    Unary {
        operator: TaccoUnaryOperator,
        src: TaccoVal,
        dest: TaccoVal,
    },
}

#[derive(Clone)]
pub enum TaccoVal {
    Constant(i32),
    Var(String),
}

pub enum TaccoUnaryOperator {
    Complement,
    Negate
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
        }
        Expression::Unary { operator, inner_expression } => {
            let src = emit_transform_expression(inner_expression.as_ref().clone(), into);

            let dest_name = generate_temp_name();
            let dest = TaccoVal::Var(dest_name);

            let tacco_operator = transform_unary_operator(operator);
            into.push(TaccoInstruction::Unary{ operator: tacco_operator, src, dest: dest.clone() });

            return dest
        }
    }
}

fn transform_unary_operator(unary_operator_token: Token) -> TaccoUnaryOperator {
    return match unary_operator_token {
        Token::ComplementOp => TaccoUnaryOperator::Complement,
        Token::MinusOp => TaccoUnaryOperator::Negate,
        _ => process::exit(7)
    }
}

static TMP_COUNT: Mutex<i32> = Mutex::new(0);

fn generate_temp_name() -> String {
    let mut count = TMP_COUNT.lock()
        .expect("IR Tacco Transform | Couldn't lock the TMP_COUNTER");

    *count = *count + 1;
    return format!("temp.{}", *count)
}
