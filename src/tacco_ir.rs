use std::{process, sync::Mutex};

use crate::parser::{BinaryOperator, Expression, FunctionDef, Program, Statement, UnaryOperator};

static TRUE_VALUE: TaccoVal = TaccoVal::Constant(1);
static FALSE_VALUE: TaccoVal = TaccoVal::Constant(1);

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
    },
    Copy(TaccoVal, TaccoVal),
    Jump(String),
    JumpIfZero(TaccoVal, String),
    JumpIfNotZero(TaccoVal, String),
    Label(String)
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
    Not,
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
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
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
        Expression::Binary { operator: BinaryOperator::LogicalAnd, left, right } => {
            let left_val = emit_transform_expression(left.as_ref().clone(), into);

            let left_var_name = generate_temp_name();
            let left_var = TaccoVal::Var(left_var_name);

            let false_label = generate_label("false_label");
            let end_label = generate_label("false_label");

            into.push(TaccoInstruction::Copy(left_val, left_var.clone()));
            into.push(TaccoInstruction::JumpIfZero(left_var, false_label.clone()));

            let right_val = emit_transform_expression(right.as_ref().clone(), into);

            let right_var_name = generate_temp_name();
            let right_var = TaccoVal::Var(right_var_name);

            into.push(TaccoInstruction::Copy(right_val, right_var.clone()));
            into.push(TaccoInstruction::JumpIfZero(right_var, false_label.clone()));

            let result_var_name = generate_temp_name();
            let result_var = TaccoVal::Var(result_var_name);

            into.push(TaccoInstruction::Copy(TRUE_VALUE.clone(), result_var.clone()));
            into.push(TaccoInstruction::Jump(end_label.clone()));

            into.push(TaccoInstruction::Label(false_label));
            into.push(TaccoInstruction::Copy(FALSE_VALUE.clone(), result_var.clone()));

            into.push(TaccoInstruction::Label(end_label.clone()));

            return result_var
        },
        Expression::Binary { operator: BinaryOperator::LogicalOr, left, right } => {
            let left_val = emit_transform_expression(left.as_ref().clone(), into);

            let left_var_name = generate_temp_name();
            let left_var = TaccoVal::Var(left_var_name);

            into.push(TaccoInstruction::Copy(left_val, left_var.clone()));
            into.push(TaccoInstruction::JumpIfNotZero(left_var, "false_label".to_string()));

            let right_val = emit_transform_expression(right.as_ref().clone(), into);

            let right_var_name = generate_temp_name();
            let right_var = TaccoVal::Var(right_var_name);

            into.push(TaccoInstruction::Copy(right_val, right_var.clone()));
            into.push(TaccoInstruction::JumpIfNotZero(right_var, "false_label".to_string()));

            let result_var_name = generate_temp_name();
            let result_var = TaccoVal::Var(result_var_name);

            into.push(TaccoInstruction::Copy(TRUE_VALUE.clone(), result_var.clone()));
            into.push(TaccoInstruction::Jump("end".to_string()));

            into.push(TaccoInstruction::Label("false_label".to_string()));
            into.push(TaccoInstruction::Copy(FALSE_VALUE.clone(), result_var.clone()));

            into.push(TaccoInstruction::Label("end".to_string()));

            return result_var
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
        BinaryOperator::Equal => TaccoBinaryOperator::Equal,
        BinaryOperator::NotEqual => TaccoBinaryOperator::NotEqual,
        BinaryOperator::LessThan => TaccoBinaryOperator::LessThan,
        BinaryOperator::LessThanOrEqual => TaccoBinaryOperator::LessThanOrEqual,
        BinaryOperator::GreaterThan => TaccoBinaryOperator::GreaterThan,
        BinaryOperator::GreaterThanOrEqual => TaccoBinaryOperator::GreaterThanOrEqual,
        BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => process::exit(6)
    }
}

fn transform_unary_operator(unary_operator: UnaryOperator) -> TaccoUnaryOperator {
    return match unary_operator {
        UnaryOperator::Complement => TaccoUnaryOperator::Complement,
        UnaryOperator::Negate => TaccoUnaryOperator::Negate,
        UnaryOperator::Not => TaccoUnaryOperator::Not,
    }
}

static TMP_COUNT: Mutex<i32> = Mutex::new(0);

fn generate_temp_name() -> String {
    let mut count = TMP_COUNT.lock()
        .expect("IR Tacco Transform | Couldn't lock the TMP_COUNTER");

    *count = *count + 1;
    return format!("temp.{}", *count)
}

fn generate_label(label: &str) -> String {
    let mut count = TMP_COUNT.lock()
        .expect("IR Tacco Transform | Couldn't lock the TMP_COUNTER");

    *count = *count + 1;
    return format!("_mcc__{}__{}", label, *count)
}
