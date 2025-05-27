use std::{process, sync::Mutex};

use crate::parser::{AssignmentOperator, BinaryOperator, BlockItem, Declaration, Expression, ForInitializer, FunctionDeclaration, Program, Statement, UnaryOperator};

static TRUE_VALUE: TaccoVal = TaccoVal::Constant(1);
static FALSE_VALUE: TaccoVal = TaccoVal::Constant(0);

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
    // return TaccoProgram { function_definition: transform_function_definition(program.function_definition) };
    todo!()
}

fn transform_function_definition(function_definition: FunctionDeclaration) -> TaccoFunctionDef {
    let mut instructions = Vec::<TaccoInstruction>::new();
    // emit_block_items(&function_definition.body.block_items, &mut instructions);

    let last_instruction = instructions.last();
    if !matches!(last_instruction, Some(TaccoInstruction::Return(_))) {
        instructions.push(TaccoInstruction::Return(TaccoVal::Constant(0)));
    }

    return TaccoFunctionDef { identifier: function_definition.name, body: instructions }
}

fn emit_block_items(block_items: &Vec<BlockItem>, into: &mut Vec<TaccoInstruction>) {
    block_items.iter().for_each(|block_item| {
        match block_item {
            BlockItem::Statement(statement) => {
                emit_statement(statement.clone(), into);
            },
            BlockItem::Declaration(declaration) => emit_declaration(declaration, into),
        }
    });
}

fn emit_declaration(declaration: &Declaration, into: &mut Vec<TaccoInstruction>) {
    // if declaration.initializer.is_none() {
    //     return
    // }

    // let init_expr = declaration.initializer.as_ref().unwrap();
    // let value = emit_transform_expression(init_expr.clone(), into);

    // into.push(TaccoInstruction::Copy(value, TaccoVal::Var(declaration.name.clone())));
    todo!()
}

fn emit_statement(statement: Statement, into: &mut Vec<TaccoInstruction>) {
    match statement {
        Statement::Return(expression) => {
            let value = emit_transform_expression(expression, into);
            into.push(TaccoInstruction::Return(value));
        },
        Statement::If { condition, then, _else: None } => {
            let condition_result = emit_transform_expression(condition, into);

            let result_var_name = generate_temp_name();
            let result_var = TaccoVal::Var(result_var_name);

            let end_label = generate_label("if_end_label");

            into.push(TaccoInstruction::Copy(condition_result, result_var.clone()));
            into.push(TaccoInstruction::JumpIfZero(result_var, end_label.clone()));

            emit_statement(then.as_ref().clone(), into);

            into.push(TaccoInstruction::Label(end_label));
        },
        Statement::If { condition, then, _else: Some(else_statement) } => {
            let condition_result = emit_transform_expression(condition, into);

            let result_var_name = generate_temp_name();
            let result_var = TaccoVal::Var(result_var_name);

            let end_label = generate_label("if_end_label");
            let else_label = generate_label("else_label");

            into.push(TaccoInstruction::Copy(condition_result, result_var.clone()));
            into.push(TaccoInstruction::JumpIfZero(result_var, else_label.clone()));

            emit_statement(then.as_ref().clone(), into);

            into.push(TaccoInstruction::Jump(end_label.clone()));
            into.push(TaccoInstruction::Label(else_label));

            emit_statement(else_statement.as_ref().clone(), into);

            into.push(TaccoInstruction::Label(end_label));
        }
        Statement::Expression(expression) => {
            emit_transform_expression(expression, into);
        },
        Statement::Null => {},
        Statement::Label(label, statement) => {
            into.push(TaccoInstruction::Label(label));
            emit_statement(statement.as_ref().clone(), into);
        }
        Statement::Goto(label) => {
            into.push(TaccoInstruction::Jump(label));
        },
        Statement::Compound(block) => emit_block_items(&block.block_items, into),
        Statement::DoWhile { body, condition, label } => {
            into.push(TaccoInstruction::Label(label.clone()));

            emit_statement(body.as_ref().clone(), into);

            into.push(TaccoInstruction::Label(format!("{}_continue", label.clone())));

            let result = emit_transform_expression(condition, into);

            let result_var_name = generate_temp_name();
            let result_var = TaccoVal::Var(result_var_name);

            into.push(TaccoInstruction::Copy(result, result_var.clone()));
            into.push(TaccoInstruction::JumpIfNotZero(result_var, label.clone()));
            into.push(TaccoInstruction::Label(format!("{}_break", label)))
        },
        Statement::While { condition, body, label } => {
            println!("give out while");
            let continue_label = format!("{}_continue", label.clone());
            let break_label = format!("{}_break", label.clone());

            into.push(TaccoInstruction::Label(continue_label.clone()));

            let result = emit_transform_expression(condition, into);

            let result_var_name = generate_temp_name();
            let result_var = TaccoVal::Var(result_var_name);

            into.push(TaccoInstruction::Copy(result, result_var.clone()));
            into.push(TaccoInstruction::JumpIfZero(result_var, break_label.clone()));

            emit_statement(body.as_ref().clone(), into);

            into.push(TaccoInstruction::Jump(continue_label));
            into.push(TaccoInstruction::Label(break_label));
        },
        Statement::For { init, condition, post, body, label } => {
            let continue_label = format!("{}_continue", label.clone());
            let break_label = format!("{}_break", label.clone());

            emit_for_init(init, into);

            into.push(TaccoInstruction::Label(label.clone()));

            if let Some(condition_expr) = condition {
                let result = emit_transform_expression(condition_expr, into);

                let result_var_name = generate_temp_name();
                let result_var = TaccoVal::Var(result_var_name);

                into.push(TaccoInstruction::Copy(result, result_var.clone()));
                into.push(TaccoInstruction::JumpIfZero(result_var, break_label.clone()));
            }

            emit_statement(body.as_ref().clone(), into);

            into.push(TaccoInstruction::Label(continue_label));

            if let Some(post_expr) = post {
                emit_transform_expression(post_expr, into);
            }

            into.push(TaccoInstruction::Jump(label));
            into.push(TaccoInstruction::Label(break_label))
        },
        Statement::Break(label) => {
            into.push(TaccoInstruction::Jump(format!("{}_break", label)));
        },
        Statement::Continue(label) => {
            into.push(TaccoInstruction::Jump(format!("{}_continue", label)));
        },
        Statement::Switch { control_expression, body, label, cases, default } => {
            let control_value = emit_transform_expression(control_expression, into);
            let control_var = TaccoVal::Var(generate_temp_name());
            into.push(TaccoInstruction::Copy(control_value, control_var.clone()));

            let result_var = TaccoVal::Var(generate_temp_name());
            cases.expect("Switch Cases shouldn't be None at this point.")
                .iter()
                .for_each(|case| {
                    // we can safely assume that this is a const expression as it is checked in the semantic analysis
                    let TaccoVal::Constant(const_val) = emit_transform_expression(case.clone(), into) else { unreachable!() };
                    into.push(TaccoInstruction::Binary { operator: TaccoBinaryOperator::Equal, src_1: TaccoVal::Constant(const_val), src_2: control_var.clone(), dest: result_var.clone() });
                    into.push(TaccoInstruction::JumpIfNotZero(result_var.clone(), format!("{}_{}", label.clone(), const_val)));
                });

            if default {
                into.push(TaccoInstruction::Jump(format!("{}_default", label)));
            }

            let break_label = format!("{}_break", label);

            into.push(TaccoInstruction::Jump(break_label.clone()));

            emit_block_items(&body.block_items, into);

            into.push(TaccoInstruction::Label(break_label));
        },
        Statement::Case(expr, label) => {
            let Expression::Const(const_val) = expr else { unreachable!() };
            into.push(TaccoInstruction::Label(format!("{}_{}", label, const_val)));
        },
        Statement::Default(label) => {
            into.push(TaccoInstruction::Label(format!("{}_default", label)));
        }

    }
}

fn emit_for_init(for_init: ForInitializer, into: &mut Vec<TaccoInstruction>) {
    match for_init {
        ForInitializer::Declaration(decl) => todo!(), // emit_declaration(&decl, into),
        ForInitializer::Expression(Some(expr)) => { emit_transform_expression(expr, into); },
        ForInitializer::Expression(None) => {},
    };
}

fn emit_transform_expression(expression: Expression, into: &mut Vec<TaccoInstruction>) -> TaccoVal {
    match expression {
        _ => todo!(),
        Expression::Const(int_value) => {
            return TaccoVal::Constant(int_value)
        },
        Expression::Unary {
            operator: op @ UnaryOperator::Increment { postfix }
                    | op @ UnaryOperator::Decrement { postfix },
            inner_expression
        } => {
            let inner_value_expr = inner_expression.as_ref();
            let operator = match op {
                UnaryOperator::Increment { postfix: _ } => BinaryOperator::Add,
                UnaryOperator::Decrement { postfix: _ } => BinaryOperator::Subtract,
                _ => unreachable!(),
            };

            if !matches!(inner_value_expr, Expression::Var(_)) {
                println!("Tacco IR Generation | Can only use increment operator for variables");
                process::exit(84093);
            }

            let var_name_string = match inner_value_expr {
                Expression::Var(var_name) => var_name,
                _ => unreachable!(),
            };

            let var_value = TaccoVal::Var(var_name_string.clone());

            if postfix {
                let original_val_temp_name = generate_temp_name();
                let original_val_temp_value = TaccoVal::Var(original_val_temp_name.clone());

                into.push(TaccoInstruction::Copy(var_value.clone(), original_val_temp_value.clone()));

                let incremented_val_temp_value = emit_transform_expression(
                    Expression::Binary {
                        operator,
                        left: Box::new(inner_value_expr.clone()),
                        right: Box::new(Expression::Const(1)),
                    },
                    into,
                );

                into.push(TaccoInstruction::Copy(incremented_val_temp_value, var_value));

                return original_val_temp_value;
            } else {
                let incremented_val_temp_value = emit_transform_expression(
                    Expression::Binary {
                        operator,
                        left: Box::new(inner_value_expr.clone()),
                        right: Box::new(Expression::Const(1)),
                    },
                    into,
                );

                into.push(TaccoInstruction::Copy(incremented_val_temp_value.clone(), var_value));

                return incremented_val_temp_value;
            }
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

            let false_label = generate_label("and_false_label");
            let end_label = generate_label("and_end_label");

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

            let false_label = generate_label("or_false_label");
            let end_label = generate_label("or_end_label");

            into.push(TaccoInstruction::Copy(left_val, left_var.clone()));
            into.push(TaccoInstruction::JumpIfNotZero(left_var, false_label.clone()));

            let right_val = emit_transform_expression(right.as_ref().clone(), into);

            let right_var_name = generate_temp_name();
            let right_var = TaccoVal::Var(right_var_name);

            into.push(TaccoInstruction::Copy(right_val, right_var.clone()));
            into.push(TaccoInstruction::JumpIfNotZero(right_var, false_label.clone()));

            let result_var_name = generate_temp_name();
            let result_var = TaccoVal::Var(result_var_name);

            into.push(TaccoInstruction::Copy(FALSE_VALUE.clone(), result_var.clone()));
            into.push(TaccoInstruction::Jump(end_label.clone()));

            into.push(TaccoInstruction::Label(false_label));
            into.push(TaccoInstruction::Copy(TRUE_VALUE.clone(), result_var.clone()));

            into.push(TaccoInstruction::Label(end_label.clone()));

            return result_var
        },
        Expression::Binary { operator, left, right } => {
            let src_1 = emit_transform_expression(left.as_ref().clone(), into);
            let src_2 = emit_transform_expression(right.as_ref().clone(), into);

            let dest_name = generate_temp_name();
            let dest = TaccoVal::Var(dest_name);

            let tacco_operator = transform_binary_operator(operator);

            into.push(TaccoInstruction::Binary { operator: tacco_operator, src_1, src_2, dest: dest.clone() });

            return dest;
        },
        Expression::Var(identifier) => return TaccoVal::Var(identifier),
        Expression::Assignment(assignment_operator, left, right) => {
            if let Expression::Var(identifier) = left.as_ref() {
                let result = emit_transform_expression(right.as_ref().clone(), into);
                if let Some(operator) = assignment_operator {
                    into.push(TaccoInstruction::Binary {
                        operator: transform_assignment_operator(operator),
                        src_1: TaccoVal::Var(identifier.clone()),
                        src_2: result.clone(),
                        dest: TaccoVal::Var(identifier.clone()),
                    })
                } else {
                    into.push(TaccoInstruction::Copy(result, TaccoVal::Var(identifier.clone())));
                }

                return TaccoVal::Var(identifier.clone())
            }

            process::exit(10);
        },
        Expression::Conditional { condition, if_true, _else } => {
            let condition_result = emit_transform_expression(condition.as_ref().clone(), into);

            let result_var_name = generate_temp_name();
            let result_var = TaccoVal::Var(result_var_name);

            let final_result_var_name = generate_temp_name();
            let final_result_var = TaccoVal::Var(final_result_var_name);

            into.push(TaccoInstruction::Copy(condition_result, result_var.clone()));

            let end_label = generate_label("conditional_end_label");
            let second_label = generate_label("conditional_second_label");

            into.push(TaccoInstruction::JumpIfZero(result_var, second_label.clone()));

            let one_result = emit_transform_expression(if_true.as_ref().clone(), into);

            let one_var_name = generate_temp_name();
            let one_var = TaccoVal::Var(one_var_name);

            into.push(TaccoInstruction::Copy(one_result, one_var.clone()));
            into.push(TaccoInstruction::Copy(one_var, final_result_var.clone()));
            into.push(TaccoInstruction::Jump(end_label.clone()));

            into.push(TaccoInstruction::Label(second_label));

            let second_result = emit_transform_expression(_else.as_ref().clone(), into);

            let second_var_name = generate_temp_name();
            let second_var = TaccoVal::Var(second_var_name);

            into.push(TaccoInstruction::Copy(second_result, second_var.clone()));
            into.push(TaccoInstruction::Copy(second_var, final_result_var.clone()));

            into.push(TaccoInstruction::Label(end_label));

            return final_result_var
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

fn transform_assignment_operator(assignment_operator: AssignmentOperator) -> TaccoBinaryOperator {
    return match assignment_operator {
        AssignmentOperator::Plus => TaccoBinaryOperator::Add,
        AssignmentOperator::Minus => TaccoBinaryOperator::Subtract,
        AssignmentOperator::Multiply => TaccoBinaryOperator::Multiply,
        AssignmentOperator::Divide => TaccoBinaryOperator::Divide,
        AssignmentOperator::Remainder => TaccoBinaryOperator::Remainder,
        AssignmentOperator::LeftShift => TaccoBinaryOperator::BitwiseLeftShift,
        AssignmentOperator::RightShift => TaccoBinaryOperator::BitwiseRightShift,
        AssignmentOperator::BitwiseAnd => TaccoBinaryOperator::BitwiseAnd,
        AssignmentOperator::BitwiseOr => TaccoBinaryOperator::BitwiseOr,
        AssignmentOperator::BitwiseXor => TaccoBinaryOperator::BitwiseXor,
    }
}

fn transform_unary_operator(unary_operator: UnaryOperator) -> TaccoUnaryOperator {
    return match unary_operator {
        UnaryOperator::Complement => TaccoUnaryOperator::Complement,
        UnaryOperator::Negate => TaccoUnaryOperator::Negate,
        UnaryOperator::Not => TaccoUnaryOperator::Not,
        _ => process::exit(4809),
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
