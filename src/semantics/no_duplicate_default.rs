use std::process;

use crate::parser::{Block, BlockItem, Program, Statement};

pub fn check_for_duplicate_defaults(program: &Program) {
    let mut default_count = 0;
    program.function_declarations.iter().for_each(|decl| {
        if let Some(block) = decl.body.clone() {
            check_in_block(&block, &mut default_count);
        }
    });
}

fn check_in_block(block: &Block, default_count: &mut i32) {
    block.block_items.iter().for_each(|block_item| check_in_block_item(block_item, default_count));
}

fn check_in_block_item(block_item: &BlockItem, default_count: &mut i32) {
    match block_item {
        BlockItem::Declaration(_) => {},
        BlockItem::Statement(statement) => check_in_statement(statement, default_count),
    }
}

fn check_in_statement(statement: &Statement, default_count: &mut i32) {
    match statement {
        Statement::Goto(_)
        | Statement::Null
        | Statement::Continue(_)
        | Statement::Return(_)
        | Statement::Label(_, _)
        | Statement::Expression(_)
        | Statement::Case(_, _)
        | Statement::Break(_) => {},

        Statement::Default(_) => {
            *default_count += 1;
        },

        Statement::Compound(block) => check_in_block(block, default_count),
        Statement::If { then, _else, .. } => {
            check_in_statement(then.as_ref(), default_count);
            _else.as_ref().map(|els| check_in_statement(els.as_ref(), default_count));
        },
        Statement::For { body, .. } => check_in_statement(body.as_ref(), default_count),
        Statement::While { body, .. } => check_in_statement(body.as_ref(), default_count),
        Statement::DoWhile { body, .. } => check_in_statement(body.as_ref(), default_count),
        Statement::Switch { body, .. } => {
            let mut default_count = 0;

            check_in_block(body, &mut default_count);

            if default_count > 1 {
                println!("Declared more than one default case in switch block");
                process::exit(34);
            }
        }
    }
}
