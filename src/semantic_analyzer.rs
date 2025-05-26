use std::{collections::HashMap, process, sync::Mutex};

use crate::{parser::{Block, BlockItem, Declaration, Expression, ForInitializer, FunctionDef, Program, Statement}};

pub fn validate(program: Program) -> Program {
    return label_resolve(var_resolve(program))
}

pub fn label_resolve(program: Program) -> Program {
    let mut undefined_used_labels = Vec::<String>::new();
    let mut label_map = HashMap::<String, String>::new();

    let body = program.function_definition.body.block_items.iter().map(|block_item| {
        match block_item {
            BlockItem::Statement(statement) => BlockItem::Statement(label_resolve_statement(statement, &mut label_map, &mut undefined_used_labels)),
            BlockItem::Declaration(declaration) => BlockItem::Declaration(declaration.clone())
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
    let mut variable_map = HashMap::<String, (String, bool)>::new();

    return Program {
        function_definition: FunctionDef {
            name: program.function_definition.name,
            body: var_resolve_block(&program.function_definition.body, &mut variable_map),
        }
    }
}

fn var_resolve_declaration(declaration: &Declaration, variable_map: &mut HashMap<String, (String, bool)>) -> Declaration {
    if variable_map.contains_key(&declaration.name) && variable_map.get(&declaration.name.clone()).map_or(false, |(_, from_current_scope)| { *from_current_scope }) {
        println!("Semantic Analyzer | Duplicate variable declaration");
        process::exit(7);
    }

    let unique_name = generate_unique_name(declaration.name.clone());
    variable_map.insert(declaration.name.clone(), (unique_name.clone(), true));

    let mut init = None;
    if let Some(init_expr) = declaration.initializer.clone() {
        init = Some(var_resolve_expression(&init_expr, variable_map));
    }

    return Declaration{ name: unique_name, initializer: init };
}

fn var_resolve_statement(statement: &Statement, variable_map: &mut HashMap<String, (String, bool)>) -> Statement {
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
        Statement::Compound(block) => {
            let mut new_var_map = copy_variable_map(variable_map);
            Statement::Compound(var_resolve_block(block, &mut new_var_map))
        },
        Statement::For { init, condition, post, body, label } => {
            let mut new_variable_map = copy_variable_map(variable_map);

            Statement::For {
                init: var_resolve_for_init(init, &mut new_variable_map),
                condition: var_resolve_optional_expr(condition, &mut new_variable_map),
                post: var_resolve_optional_expr(post, &mut new_variable_map),
                body: Box::new(var_resolve_statement(body.as_ref(), &mut new_variable_map)),
                label: label.clone()
            }
        },
        Statement::DoWhile { body, condition, label } => Statement::DoWhile {
            body: Box::new(var_resolve_statement(body, variable_map)),
            condition: var_resolve_expression(condition, variable_map),
            label: label.clone(),
        },
        Statement::While { condition, body, label } => Statement::While {
            condition: var_resolve_expression(condition, variable_map),
            body: Box::new(var_resolve_statement(body, variable_map)),
            label: label.clone(),
        },
        Statement::Break(label) => Statement::Break(label.clone()),
        Statement::Continue(label) => Statement::Continue(label.clone()),
        Statement::Switch { control_expression, body, label } => {
            let mut new_variable_map = copy_variable_map(variable_map);
            Statement::Switch {
                control_expression: var_resolve_expression(control_expression, variable_map),
                body: var_resolve_block(body, &mut new_variable_map),
                label: label.clone(),
            }
        },
        Statement::Case(expr, label) => Statement::Case(var_resolve_expression(expr, variable_map), label.clone()),
        Statement::Default(label) => Statement::Default(label.clone()),
    }
}

fn var_resolve_optional_expr(expression_opt: &Option<Expression>, variable_map: &mut HashMap<String, (String, bool)>) -> Option<Expression> {
    return match expression_opt {
        Some(expr) => Some(var_resolve_expression(&expr, variable_map)),
        None => None,
    }
}

fn var_resolve_for_init(for_init: &ForInitializer, variable_map: &mut HashMap<String, (String, bool)>) -> ForInitializer {
    match for_init {
        ForInitializer::Declaration(decl) => ForInitializer::Declaration(var_resolve_declaration(decl, variable_map)),
        ForInitializer::Expression(expr) => ForInitializer::Expression(var_resolve_optional_expr(&expr, variable_map)),
    }
}

fn copy_variable_map(variable_map: &HashMap<String, (String, bool)>) -> HashMap<String, (String, bool)> {
    let new_map: HashMap<String, (String, bool)> = variable_map.iter()
        .map(|(key, (name, _))| {
            (key.clone(), (name.clone(), false))
        })
        .collect();
    return new_map
}

fn var_resolve_block(block: &Block, variable_map: &mut HashMap<String, (String, bool)>) -> Block {
    let block_items = block.block_items.iter().map(|block_item| {
        match block_item {
            BlockItem::Statement(statement) => BlockItem::Statement(var_resolve_statement(statement, variable_map)),
            BlockItem::Declaration(declaration) => BlockItem::Declaration(var_resolve_declaration(declaration, variable_map)),
        }
    }).collect();

    return Block { block_items };
}

fn var_resolve_expression(expression: &Expression, variable_map: &mut HashMap<String, (String, bool)>) -> Expression {
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
                return Expression::Var(variable_map.get(identifier).unwrap().0.to_string())
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
