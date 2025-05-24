use std::{process, sync::Mutex};

use crate::parser::{Block, BlockItem, FunctionDef, Program, Statement};

pub fn perform(program: Program) -> Program {
    let function_body = label_block(&program.function_definition.body, None);
    return Program {
        function_definition: FunctionDef {
            name: program.function_definition.name,
            body: function_body,
        }
    }
}

fn label_statement(statement: &Statement, current_label: Option<&str>) -> Statement {
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
                println!("break statement outside of loop");
                process::exit(88);
            }

            return Statement::Continue(current_label.unwrap().to_string())
        },
        Statement::While { condition, body, label: _ } => {
            let new_label = generate_unique_name("while");

            return Statement::While {
                condition: condition.clone(),
                body: Box::new(label_statement(body.as_ref(), Some(&new_label))),
                label: new_label,
            };
        }
        Statement::DoWhile { body, condition, label } => {
            let new_label = generate_unique_name("do_while");

            return Statement::DoWhile {
                body: Box::new(label_statement(body.as_ref(), Some(&new_label))),
                condition: condition.clone(),
                label: new_label,
            };
        },
        Statement::For { init, condition, post, body, label } => {
            let new_label = generate_unique_name("for");

            return Statement::For {
                init: init.clone(),
                condition: condition.clone(),
                post: post.clone(),
                body: Box::new(label_statement(body.as_ref(), Some(&new_label))),
                label: new_label,
            }
        },
        Statement::If { condition, then, _else } => Statement::If {
            condition: condition.clone(),
            then: Box::new(label_statement(then, current_label)),
            _else: label_optional_statement(&_else.clone().map(|e| e.as_ref().clone()), current_label).map(|statement| Box::new(statement)),
        },
        Statement::Compound(block) => Statement::Compound(label_block(block, current_label)),
        Statement::Goto(label) => Statement::Goto(label.clone()),
        Statement::Label(label, statement) => Statement::Label(
            label.clone(),
            Box::new(label_statement(statement.as_ref(), current_label)),
        ),
        Statement::Return(expr) => Statement::Return(expr.clone()),
        Statement::Expression(expr) => Statement::Expression(expr.clone()),
        Statement::Null => Statement::Null,
    }
}

fn label_block(block: &Block, current_label: Option<&str>) -> Block {
    let block_items = block.block_items.iter().map(|block_item| {
        match block_item {
            BlockItem::Statement(statement) => BlockItem::Statement(label_statement(statement, current_label)),
            BlockItem::Declaration(decl) => BlockItem::Declaration(decl.clone()),
        }
    }).collect();

    return Block {
        block_items
    };
}

fn label_optional_statement(statement: &Option<Statement>, current_label: Option<&str>) -> Option<Statement> {
    return match statement {
        Some(statement) => Some(label_statement(statement, current_label)),
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
