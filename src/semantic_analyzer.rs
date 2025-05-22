use std::{collections::HashMap, process, sync::Mutex};

use crate::parser::{BlockItem, Declaration, Expression, FunctionDef, Program, Statement};

pub fn validate(program: Program) -> Program {
    let mut variable_map = HashMap::<String, String>::new();

    let body = program.function_definition.body.iter().map(|block_item| {
        match block_item {
            BlockItem::Statement(statement) => BlockItem::Statement(resolve_statement(statement.clone(), &mut variable_map)),
            BlockItem::Declaration(declaration) => BlockItem::Declaration(resolve_declaration(declaration.clone(), &mut variable_map)),
        }
    }).collect();

    return Program {
        function_definition: FunctionDef {
            name: program.function_definition.name,
            body,
        }
    }
}

fn resolve_declaration(declaration: Declaration, variable_map: &mut HashMap<String, String>) -> Declaration {
    if variable_map.contains_key(&declaration.name) {
        println!("Semantic Analyzer | Duplicate variable declaration");
        process::exit(7);
    }

    let unique_name = generate_unique_name(declaration.name.clone());
    variable_map.insert(declaration.name, unique_name.clone());

    let mut init = None;
    if let Some(init_expr) = declaration.initializer {
        init = Some(resolve_expression(init_expr, variable_map));
    }

    return Declaration{ name: unique_name, initializer: init };
}

fn resolve_statement(statement: Statement, variable_map: &mut HashMap<String, String>) -> Statement {
    return match statement {
        Statement::Return(expression) => Statement::Return(resolve_expression(expression, variable_map)),
        Statement::Expression(expression) => Statement::Expression(resolve_expression(expression, variable_map)),
        Statement::Null => Statement::Null,
    }
}

fn resolve_expression(expression: Expression, variable_map: &mut HashMap<String, String>) -> Expression {
    match expression {
        Expression::Assignment(assignment_operator, left, right) => {
            if let Expression::Var(_) = left.as_ref() {
                return Expression::Assignment(
                    assignment_operator,
                    Box::new(resolve_expression(left.as_ref().clone(), variable_map)),
                    Box::new(resolve_expression(right.as_ref().clone(), variable_map)),
                )
            }

            println!("Semantic Analyzer | Invalid lvalue");
            process::exit(8)
        },
        Expression::Var(identifier) => {
            if variable_map.contains_key(&identifier) {
                return Expression::Var(variable_map.get(&identifier).unwrap().to_string())
            }

            println!("Semantic Analyzer | Undeclared variable use");
            process::exit(8)
        },
        Expression::Binary { operator, left, right } => Expression::Binary {
            operator,
            left: Box::new(resolve_expression(left.as_ref().clone(), variable_map)),
            right: Box::new(resolve_expression(right.as_ref().clone(), variable_map)),
        },
        Expression::Unary { operator, inner_expression } => Expression::Unary {
            operator,
            inner_expression: Box::new(resolve_expression(inner_expression.as_ref().clone(), variable_map)),
        },
        Expression::Const(int_value) => Expression::Const(int_value),
    }
}

static UNIQUE_COUNT: Mutex<u32> = Mutex::new(0);

fn generate_unique_name(name: String) -> String {
    let mut count = UNIQUE_COUNT.lock()
        .expect("Semantic Analyzer | Couldn't lock the TMP_COUNTER");

    *count = *count + 1;
    return format!("u.{}_{}", name, *count)
}
