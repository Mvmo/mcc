use std::{collections::HashMap, process, sync::Mutex};

use crate::parser::{Block, BlockItem, Declaration, Expression, FunctionDef, Program, Statement};

pub fn validate(program: Program) -> Program {
    return label_resolve(var_resolve(program))
}

pub fn label_resolve(program: Program) -> Program {
    let mut undefined_used_labels = Vec::<String>::new();
    let mut label_map = HashMap::<String, String>::new();

    let body = program.function_definition.body.block_items.iter().map(|block_item| {
        match block_item {
            BlockItem::Statement(statement) => BlockItem::Statement(label_resolve_statement(statement, &mut label_map, &mut undefined_used_labels)),
            BlockItem::Declaration(declaration) => BlockItem::Declaration(declaration.clone()),
        }
    }).collect();

    if undefined_used_labels.len() != 0 {
        println!("Semantic Analysis | You've used labels, which are not declared");
        process::exit(40289);
    }

    return Program { function_definition: FunctionDef { name: program.function_definition.name, body: Block { block_items: body } } };
}

pub fn label_resolve_statement(statement: &Statement, label_map: &mut HashMap<String, String>, undefined_used_labels: &mut Vec<String>) -> Statement {
    match statement {
        Statement::Label(label, statement) => {
            if label_map.contains_key(label) && !undefined_used_labels.contains(label) {
                println!("Semantic Analyzer | Label must be unique.");
                process::exit(4989);
            }

            if let Some(position) = undefined_used_labels.iter().position(|str| str == label) {
                undefined_used_labels.remove(position);
                return Statement::Label(label_map.get(label).unwrap().clone(), Box::new(label_resolve_statement(statement, label_map, undefined_used_labels)));
            }

            let unique_name = generate_unique_label_name(label.clone());
            label_map.insert(label.clone(), unique_name.clone());

            return Statement::Label(unique_name, Box::new(label_resolve_statement(statement, label_map, undefined_used_labels)));
        },
        Statement::Goto(label) => {
            if !label_map.contains_key(label) {
                let label = label.clone();
                let unique_name = generate_unique_label_name(label.clone());
                label_map.insert(label.clone(), unique_name);

                undefined_used_labels.push(label);
            }

            return Statement::Goto(label_map.get(label).unwrap().clone())
        },
        Statement::If { condition, then, _else } => Statement::If {
            condition: condition.clone(),
            then: Box::new(label_resolve_statement(then, label_map, undefined_used_labels)),
            _else: _else.clone().map(|else_stmt| Box::new(label_resolve_statement(else_stmt.as_ref(), label_map, undefined_used_labels)))
        },
        statement => statement.clone()
    }
}

pub fn var_resolve(program: Program) -> Program {
    let mut variable_map = HashMap::<String, String>::new();

    let body = program.function_definition.body.block_items.iter().map(|block_item| {
        match block_item {
            BlockItem::Statement(statement) => BlockItem::Statement(var_resolve_statement(statement, &mut variable_map)),
            BlockItem::Declaration(declaration) => BlockItem::Declaration(var_resolve_declaration(declaration.clone(), &mut variable_map)),
        }
    }).collect();

    return Program {
        function_definition: FunctionDef {
            name: program.function_definition.name,
            body: Block{ block_items: body },
        }
    }
}

fn var_resolve_declaration(declaration: Declaration, variable_map: &mut HashMap<String, String>) -> Declaration {
    if variable_map.contains_key(&declaration.name) {
        println!("Semantic Analyzer | Duplicate variable declaration");
        process::exit(7);
    }

    let unique_name = generate_unique_name(declaration.name.clone());
    variable_map.insert(declaration.name, unique_name.clone());

    let mut init = None;
    if let Some(init_expr) = declaration.initializer {
        init = Some(var_resolve_expression(&init_expr, variable_map));
    }

    return Declaration{ name: unique_name, initializer: init };
}

fn var_resolve_statement(statement: &Statement, variable_map: &mut HashMap<String, String>) -> Statement {
    return match statement {
        Statement::Return(expression) => Statement::Return(var_resolve_expression(&expression, variable_map)),
        Statement::Expression(expression) => Statement::Expression(var_resolve_expression(&expression, variable_map)),
        Statement::Null => Statement::Null,
        Statement::If { condition, then, _else } => Statement::If {
            condition: var_resolve_expression(&condition, variable_map),
            then: Box::new(var_resolve_statement(then.as_ref(), variable_map)),
            _else: _else.as_ref().map(|statement| Box::new(var_resolve_statement(&statement, variable_map)))
        },
        Statement::Goto(_) => statement.clone(),
        Statement::Label(label, statement) => Statement::Label(
            label.clone(),
            Box::new(var_resolve_statement(statement, variable_map)),
        ),
        _ => todo!(),
    }
}

fn var_resolve_expression(expression: &Expression, variable_map: &mut HashMap<String, String>) -> Expression {
    match expression {
        Expression::Assignment(assignment_operator, left, right) => {
            if let Expression::Var(_) = left.as_ref() {
                return Expression::Assignment(
                    assignment_operator.clone(),
                    Box::new(var_resolve_expression(left, variable_map)),
                    Box::new(var_resolve_expression(right, variable_map)),
                )
            }

            println!("Semantic Analyzer | Invalid lvalue");
            process::exit(8)
        },
        Expression::Var(identifier) => {
            if variable_map.contains_key(identifier) {
                return Expression::Var(variable_map.get(identifier).unwrap().to_string())
            }

            println!("Semantic Analyzer | Undeclared variable use");
            process::exit(8)
        },
        Expression::Binary { operator, left, right } => Expression::Binary {
            operator: operator.clone(),
            left: Box::new(var_resolve_expression(left, variable_map)),
            right: Box::new(var_resolve_expression(right, variable_map)),
        },
        Expression::Unary { operator, inner_expression } => Expression::Unary {
            operator: operator.clone(),
            inner_expression: Box::new(var_resolve_expression(inner_expression, variable_map)),
        },
        Expression::Const(int_value) => Expression::Const(*int_value),
        Expression::Conditional { condition, if_true, _else } => Expression::Conditional {
            condition: Box::new(var_resolve_expression(condition, variable_map)),
            if_true: Box::new(var_resolve_expression(if_true, variable_map)),
            _else: Box::new(var_resolve_expression(_else, variable_map)),
        }
    }
}

static UNIQUE_COUNT: Mutex<u32> = Mutex::new(0);

fn generate_unique_name(name: String) -> String {
    let mut count = UNIQUE_COUNT.lock()
        .expect("Semantic Analyzer | Couldn't lock the TMP_COUNTER");

    *count = *count + 1;
    return format!("u.{}_{}", name, *count)
}

fn generate_unique_label_name(name: String) -> String {
    let mut count = UNIQUE_COUNT.lock()
        .expect("Semantic Analyzer | Couldn't lock the TMP_COUNTER");

    *count = *count + 1;
    return format!("__user_{}_{}", name, *count)
}
