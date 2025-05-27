use std::{collections::{HashMap, HashSet}, process, sync::Mutex};

use crate::parser::{Block, BlockItem, Expression, FunctionDef, Program, Statement};

fn validate_switch_cases(switch_cases: &HashMap<String, Vec<Expression>>) {
    switch_cases.iter().for_each(|(_, cases)| {
        let mut used_consts = HashSet::<i32>::new();
        cases.iter()
            .filter(|case| matches!(case, Expression::Const(_)))
            .for_each(|case| {
                let Expression::Const(int_value) = case else { unreachable!() };
                if !used_consts.insert(*int_value) {
                    println!("defined duplicate cases in switch statement");
                    process::exit(48);
                }
            });
    });
}

pub fn perform(program: Program) -> Program {
    let mut label_stack = Vec::<String>::new();
    let mut switch_cases = HashMap::<String, Vec<Expression>>::new();

    let function_body = label_block(&program.function_definition.body, &mut label_stack, &mut switch_cases);

    validate_switch_cases(&switch_cases);

    return Program {
        function_definition: FunctionDef {
            name: program.function_definition.name,
            body: function_body,
        }
    }
}

fn label_statement(statement: &Statement, label_stack: &mut Vec<String>, switch_cases: &mut HashMap<String, Vec<Expression>>) -> Statement {
    let current_label = label_stack.last();

    match statement {
        Statement::Break(_) => {
            if current_label.is_none() {
                println!("break statement outside of loop");
                process::exit(88);
            }

            return Statement::Break(current_label.unwrap().to_string())
        },
        Statement::Continue(_) => {
            if current_label.is_none() {
                println!("continue statement outside of loop");
                process::exit(88);
            }

            let current_label = current_label.unwrap();

            let has_parent_loop = label_stack.iter().take(label_stack.len() - 1).any(|label| label.starts_with("__cs_for") || label.starts_with("__cs_while") || label.starts_with("__cs_do_while"));
            let direct_parent_is_switch = current_label.starts_with("__cs_switch");

            if direct_parent_is_switch && !has_parent_loop {
                println!("continue is not allowed inside of switch statement");
                process::exit(87);
            }

            let correct_parent = if direct_parent_is_switch {
                if let Some(label) = label_stack.iter().rev().find(|label| label.starts_with("__cs_for") || label.starts_with("__cs_while") || label.starts_with("__cs_do_while")) {
                    label
                } else {
                    println!("Can't find a loop to continue");
                    process::exit(58);
                }
            } else {
                current_label
            };

            return Statement::Continue(correct_parent.to_string())
        },
        Statement::While { condition, body, label: _ } => {
            let new_label = generate_unique_name("while");

            let mut label_stack = label_stack.clone();
            label_stack.push(new_label.clone());

            return Statement::While {
                condition: condition.clone(),
                body: Box::new(label_statement(body.as_ref(), &mut label_stack, switch_cases)),
                label: new_label,
            };
        }
        Statement::DoWhile { body, condition, label: _ } => {
            let new_label = generate_unique_name("do_while");

            let mut label_stack = label_stack.clone();
            label_stack.push(new_label.clone());

            return Statement::DoWhile {
                body: Box::new(label_statement(body.as_ref(), &mut label_stack, switch_cases)),
                condition: condition.clone(),
                label: new_label,
            };
        },
        Statement::For { init, condition, post, body, label: _ } => {
            let new_label = generate_unique_name("for");

            let mut label_stack = label_stack.clone();
            label_stack.push(new_label.clone());

            return Statement::For {
                init: init.clone(),
                condition: condition.clone(),
                post: post.clone(),
                body: Box::new(label_statement(body.as_ref(), &mut label_stack, switch_cases)),
                label: new_label,
            }
        },
        Statement::If { condition, then, _else } => Statement::If {
            condition: condition.clone(),
            then: Box::new(label_statement(then, label_stack, switch_cases)),
            _else: label_optional_statement(&_else.clone().map(|e| e.as_ref().clone()), label_stack, switch_cases).map(|statement| Box::new(statement)),
        },
        Statement::Switch { control_expression, body, label: _, cases: _, default } => {
            let new_label = generate_unique_name("switch");

            let mut label_stack = label_stack.clone();
            label_stack.push(new_label.clone());

            return Statement::Switch {
                control_expression: control_expression.clone(),
                body: label_block(body, &mut label_stack, switch_cases),
                label: new_label,
                cases: Some(Vec::new()),
                default: default.clone(),
            }
        },
        Statement::Case(expr, _) => {
            let current_label = current_label.expect("Expect label for case");

            if !matches!(expr, Expression::Const(_)) {
                println!("Case Expression must be constant");
                process::exit(58);
            }

            let has_parent_switch = label_stack.iter().any(|label| label.starts_with("__cs_switch"));

            if !has_parent_switch {
                println!("Case must always appear inside of a switch statement!");
                process::exit(59);
            }

            let current_cases = if let Some(cases) = switch_cases.get_mut(current_label) {
                cases
            } else {
                let cases = Vec::<Expression>::new();
                switch_cases.insert(current_label.clone(), cases);
                switch_cases.get_mut(current_label).unwrap()
            };

            current_cases.push(expr.clone());

            let switch_label = if current_label.to_string().starts_with("__cs_switch") {
                current_label
            } else {
                if let Some(label) = label_stack.iter().rev().find(|label| label.starts_with("__cs_switch")) {
                    label
                } else {
                    println!("Can't find a switch for the case");
                    process::exit(54);
                }
            };

            return Statement::Case(expr.clone(), switch_label.to_string());
        },
        Statement::Default(_) => {
            let current_label = current_label.expect("Expect label");
            let switch_label = if current_label.to_string().starts_with("__cs_switch") {
                current_label
            } else {
                if let Some(label) = label_stack.iter().rev().find(|label| label.starts_with("__cs_switch")) {
                    label
                } else {
                    println!("Can't find a switch for the case");
                    process::exit(54);
                }
            };

            Statement::Default(switch_label.to_string())
        },
        Statement::Compound(block) => Statement::Compound(label_block(block, label_stack, switch_cases)),
        Statement::Goto(label) => Statement::Goto(label.clone()),
        Statement::Label(label, statement) => Statement::Label(
            label.clone(),
            Box::new(label_statement(statement.as_ref(), label_stack, switch_cases)),
        ),
        Statement::Return(expr) => Statement::Return(expr.clone()),
        Statement::Expression(expr) => Statement::Expression(expr.clone()),
        Statement::Null => Statement::Null,
    }
}

fn label_block(block: &Block, label_stack: &mut Vec<String>, switch_cases: &mut HashMap<String, Vec<Expression>>) -> Block {
    let block_items = block.block_items.iter().map(|block_item| {
        match block_item {
            BlockItem::Statement(statement) => BlockItem::Statement(label_statement(statement, label_stack, switch_cases)),
            BlockItem::Declaration(decl) => BlockItem::Declaration(decl.clone()),
        }
    }).collect();

    return Block {
        block_items
    };
}

fn label_optional_statement(statement: &Option<Statement>, label_stack: &mut Vec<String>, switch_cases: &mut HashMap<String, Vec<Expression>>) -> Option<Statement> {
    return match statement {
        Some(statement) => Some(label_statement(statement, label_stack, switch_cases)),
        None => None
    }
}

static UNIQUE_COUNT: Mutex<u32> = Mutex::new(0);

fn generate_unique_name(name: &str) -> String {
    let mut count = UNIQUE_COUNT.lock()
        .expect("Loop Label Resolve | Couldn't lock the TMP_COUNTER");

    *count = *count + 1;
    return format!("__cs_{}_{}", name, *count)
}
