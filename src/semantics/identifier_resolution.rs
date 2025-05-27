use std::{collections::HashMap, process, sync::Mutex};

use crate::parser::{Block, BlockItem, Declaration, Expression, ForInitializer, FunctionDeclaration, Program, Statement, VariableDeclaration};

pub fn resolve_identifiers(program: &Program) -> Program {
    let mut identifier_map = HashMap::<String, (String, bool, bool)>::new();

    let declarations = program.function_declarations
        .iter()
        .map(|decl| function_declaration_resolve_identifiers(decl, &mut identifier_map, &mut false))
        .collect();

    return Program {
        function_declarations: declarations,
    }
}

fn declaration_resolve_identifiers(declaration: &Declaration, identifier_map: &mut HashMap<String, (String, bool, bool)>, inside_fun: &mut bool) -> Declaration {
    return match declaration {
        Declaration::Variable(decl) => Declaration::Variable(variable_declaration_resolve_identifiers(decl, identifier_map, inside_fun)),
        Declaration::Function(decl) => Declaration::Function(function_declaration_resolve_identifiers(decl, identifier_map, inside_fun)),
    }
}

fn variable_declaration_resolve_identifiers(decl: &VariableDeclaration, identifier_map: &mut HashMap<String, (String, bool, bool)>, inside_fun: &mut bool) -> VariableDeclaration {
    if identifier_map.contains_key(&decl.name) && identifier_map.get(&decl.name.clone()).map_or(false, |(_, from_current_scope, has_linkage)| { *from_current_scope }) {
        println!("Semantic Analyzer | Duplicate variable declaration");
        process::exit(7);
    }

    let unique_name = generate_unique_name(&decl.name);
    identifier_map.insert(decl.name.clone(), (unique_name.clone(), true, false));

    let mut init = None;
    if let Some(init_expr) = decl.initializer.clone() {
        init = Some(expression_resolve_identifiers(&init_expr, identifier_map));
    }

    VariableDeclaration { name: unique_name, initializer: init }
}

fn function_declaration_resolve_identifiers(decl: &FunctionDeclaration, identifier_map: &mut HashMap<String, (String, bool, bool)>, inside_fun: &mut bool) -> FunctionDeclaration {
    if identifier_map.contains_key(&decl.name) {
        let (translated_name, from_current_scope, has_linkage) = identifier_map.get(&decl.name).unwrap();
        if *from_current_scope && !*has_linkage {
            println!("Duplicate function declaration");
            process::exit(3);
        }
    }

    identifier_map.insert(decl.name.clone(), (
        decl.name.clone(),
        true,
        true
    ));

    let mut inner_map = copy_identifier_map(identifier_map);

    let params = decl.params
        .iter()
        .map(|param| function_param_resolve_identifiers(param, &mut inner_map))
        .collect();

    let body = if let Some(block) = decl.body.clone() {
        if *inside_fun {
            println!("Cannot have function definitions inside of a function.");
            process::exit(4);
        }

        Some(block_resolve_identifiers(&block, &mut inner_map, &mut true))
    } else { None };

    return FunctionDeclaration {
        name: decl.name.clone(),
        params,
        body
    }
}

fn function_param_resolve_identifiers(param: &str, identifier_map: &mut HashMap<String, (String, bool, bool)>) -> String {
    if let Some((translated_name, in_current_scope, _)) = identifier_map.get(param) {
        if *in_current_scope {
            println!("Duplicate declaration in params");
            process::exit(5);
        }

        return translated_name.to_string()
    } else {
        let unique_name = generate_unique_name(param);
        identifier_map.insert(param.to_string(), (unique_name.clone(), true, false));

        println!("-- insert {}", param.to_string());

        return unique_name
    }
}

fn statement_resolve_identifiers(statement: &Statement, identifier_map: &mut HashMap<String, (String, bool, bool)>, inside_fun: &mut bool) -> Statement {
    return match statement {
        Statement::Return(expression) => Statement::Return(expression_resolve_identifiers(&expression, identifier_map)),
        Statement::Expression(expression) => Statement::Expression(expression_resolve_identifiers(&expression, identifier_map)),
        Statement::Null => Statement::Null,
        Statement::If { condition, then, _else } => Statement::If {
            condition: expression_resolve_identifiers(&condition, identifier_map),
            then: Box::new(statement_resolve_identifiers(then.as_ref(), identifier_map, inside_fun)),
            _else: _else.as_ref().map(|statement| Box::new(statement_resolve_identifiers(&statement, identifier_map, inside_fun)))
        },
        Statement::Goto(_) => statement.clone(),
        Statement::Label(label, statement) => Statement::Label(
            label.clone(),
            Box::new(statement_resolve_identifiers(statement, identifier_map, inside_fun)),
        ),
        Statement::Compound(block) => {
            let mut new_var_map = copy_identifier_map(identifier_map);
            Statement::Compound(block_resolve_identifiers(block, &mut new_var_map, inside_fun))
        },
        Statement::For { init, condition, post, body, label } => {
            let mut new_identifier_map = copy_identifier_map(identifier_map);

            Statement::For {
                init: for_init_resolve_identifiers(init, &mut new_identifier_map, inside_fun),
                condition: optional_expression_resolve_identifiers(condition, &mut new_identifier_map, inside_fun),
                post: optional_expression_resolve_identifiers(post, &mut new_identifier_map, inside_fun),
                body: Box::new(statement_resolve_identifiers(body.as_ref(), &mut new_identifier_map, inside_fun)),
                label: label.clone()
            }
        },
        Statement::DoWhile { body, condition, label } => Statement::DoWhile {
            body: Box::new(statement_resolve_identifiers(body, identifier_map, inside_fun)),
            condition: expression_resolve_identifiers(condition, identifier_map),
            label: label.clone(),
        },
        Statement::While { condition, body, label } => Statement::While {
            condition: expression_resolve_identifiers(condition, identifier_map),
            body: Box::new(statement_resolve_identifiers(body, identifier_map, inside_fun)),
            label: label.clone(),
        },
        Statement::Break(label) => Statement::Break(label.clone()),
        Statement::Continue(label) => Statement::Continue(label.clone()),
        Statement::Switch { control_expression, body, label, cases, default } => {
            let mut new_identifier_map = copy_identifier_map(identifier_map);
            Statement::Switch {
                control_expression: expression_resolve_identifiers(control_expression, identifier_map),
                body: block_resolve_identifiers(body, &mut new_identifier_map, inside_fun),
                label: label.clone(),
                cases: cases.clone(),
                default: default.clone()
            }
        },
        Statement::Case(expr, label) => Statement::Case(expression_resolve_identifiers(expr, identifier_map), label.clone()),
        Statement::Default(label) => Statement::Default(label.clone()),
    }
}

fn optional_expression_resolve_identifiers(expression_opt: &Option<Expression>, identifier_map: &mut HashMap<String, (String, bool, bool)>, inside_fun: &mut bool) -> Option<Expression> {
    return match expression_opt {
        Some(expr) => Some(expression_resolve_identifiers(&expr, identifier_map)),
        None => None,
    }
}

fn for_init_resolve_identifiers(for_init: &ForInitializer, identifier_map: &mut HashMap<String, (String, bool, bool)>, inside_fun: &mut bool) -> ForInitializer {
    match for_init {
        ForInitializer::Declaration(decl) => ForInitializer::Declaration(variable_declaration_resolve_identifiers(decl, identifier_map, inside_fun)),
        ForInitializer::Expression(expr) => ForInitializer::Expression(optional_expression_resolve_identifiers(&expr, identifier_map, inside_fun)),
    }
}

fn block_resolve_identifiers(block: &Block, identifier_map: &mut HashMap<String, (String, bool, bool)>, inside_fun: &mut bool) -> Block {
    let block_items = block.block_items.iter().map(|block_item| {
        match block_item {
            BlockItem::Statement(statement) => BlockItem::Statement(statement_resolve_identifiers(statement, identifier_map, inside_fun)),
            BlockItem::Declaration(declaration) => BlockItem::Declaration(declaration_resolve_identifiers(declaration, identifier_map, inside_fun)),
        }
    }).collect();

    return Block { block_items };
}

// (translated_name, in_current_scope, has_linkage)
fn expression_resolve_identifiers(expression: &Expression, identifier_map: &mut HashMap<String, (String, bool, bool)>) -> Expression {
    match expression {
        Expression::Assignment(assignment_operator, left, right) => {
            if let Expression::Var(_) = left.as_ref() {
                return Expression::Assignment(
                    assignment_operator.clone(),
                    Box::new(expression_resolve_identifiers(left, identifier_map)),
                    Box::new(expression_resolve_identifiers(right, identifier_map)),
                )
            }

            println!("Semantic Analyzer | Invalid lvalue");
            process::exit(8)
        },
        Expression::Var(identifier) => {
            if identifier_map.contains_key(identifier) {
                return Expression::Var(identifier_map.get(identifier).unwrap().0.to_string())
            }

            println!("Semantic Analyzer | Undeclared variable use {}", identifier);
            process::exit(8)
        },
        Expression::Binary { operator, left, right } => Expression::Binary {
            operator: operator.clone(),
            left: Box::new(expression_resolve_identifiers(left, identifier_map)),
            right: Box::new(expression_resolve_identifiers(right, identifier_map)),
        },
        Expression::Unary { operator, inner_expression } => Expression::Unary {
            operator: operator.clone(),
            inner_expression: Box::new(expression_resolve_identifiers(inner_expression, identifier_map)),
        },
        Expression::Const(int_value) => Expression::Const(*int_value),
        Expression::Conditional { condition, if_true, _else } => Expression::Conditional {
            condition: Box::new(expression_resolve_identifiers(condition, identifier_map)),
            if_true: Box::new(expression_resolve_identifiers(if_true, identifier_map)),
            _else: Box::new(expression_resolve_identifiers(_else, identifier_map)),
        },
        Expression::FunctionCall { identifier, args } => {
            if identifier_map.contains_key(identifier) {
                let translated_name = identifier_map.get(identifier).expect("Expect here in map").0.clone();
                let args = args.iter()
                    .map(|arg| expression_resolve_identifiers(arg, identifier_map))
                    .collect();

                return Expression::FunctionCall {
                    identifier: translated_name,
                    args,
                }
            } else {
                println!("Use of undeclared function");
                process::exit(2);
            }
        }
    }
}

fn copy_identifier_map(identifier_map: &HashMap<String, (String, bool, bool)>) -> HashMap<String, (String, bool, bool)> {
    let new_map: HashMap<String, (String, bool, bool)> = identifier_map.iter()
        .map(|(key, (name, _, has_linkage))| {
            (key.clone(), (name.clone(), false, *has_linkage))
        })
        .collect();
    return new_map
}

static UNIQUE_COUNT: Mutex<u32> = Mutex::new(0);

fn generate_unique_name(name: &str) -> String {
    let mut count = UNIQUE_COUNT.lock()
        .expect("Identifier Resolution | Couldn't lock the TMP_COUNTER");

    *count = *count + 1;
    return format!("__idr_{}_{}", name, *count)
}
