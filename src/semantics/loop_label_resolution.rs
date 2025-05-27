use std::{collections::HashMap, process, sync::Mutex};

use crate::parser::{Block, BlockItem, FunctionDeclaration, Program, Statement};

pub fn resolve_loop_labels(program: &Program) -> Program {
    let mut undefined_used_labels = Vec::<String>::new();
    let mut label_map = HashMap::<String, String>::new();

    let declarations = program.function_declarations
        .iter()
        .map(|decl| FunctionDeclaration {
            name: decl.name.clone(),
            body: decl.body.as_ref().map(|block| block_resolve_loop_labels(block, &mut label_map, &mut undefined_used_labels)),
            params: decl.params.clone()
        })
        .collect();

    if undefined_used_labels.len() != 0 {
        println!("Loop Labeling | You've used labels, which are not declared");
        process::exit(40289);
    }

    return Program {
        function_declarations: declarations,
    };
}

pub fn block_resolve_loop_labels(block: &Block, label_map: &mut HashMap<String, String>, undefined_used_labels: &mut Vec<String>) -> Block {
    let block_items = block.block_items.iter().map(|block_item| match block_item {
        BlockItem::Statement(statement) => BlockItem::Statement(statement_resolve_loop_labels(statement, label_map, undefined_used_labels)),
        BlockItem::Declaration(..) => block_item.clone(),
    }).collect();

    return Block {
        block_items
    }
}

pub fn statement_resolve_loop_labels(statement: &Statement, label_map: &mut HashMap<String, String>, undefined_used_labels: &mut Vec<String>) -> Statement {
    match statement {
        Statement::Label(label, statement) => {
            if label_map.contains_key(label) && !undefined_used_labels.contains(label) {
                println!("Loop Labeling | Label must be unique.");
                process::exit(4989);
            }

            if let Some(position) = undefined_used_labels.iter().position(|str| str == label) {
                undefined_used_labels.remove(position);
                return Statement::Label(label_map.get(label).unwrap().clone(), Box::new(statement_resolve_loop_labels(statement, label_map, undefined_used_labels)));
            }

            let unique_name = generate_unique_label_name(label.clone());
            label_map.insert(label.clone(), unique_name.clone());

            return Statement::Label(unique_name, Box::new(statement_resolve_loop_labels(statement, label_map, undefined_used_labels)));
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
            then: Box::new(statement_resolve_loop_labels(then, label_map, undefined_used_labels)),
            _else: _else.clone().map(|else_stmt| Box::new(statement_resolve_loop_labels(else_stmt.as_ref(), label_map, undefined_used_labels)))
        },
        statement => statement.clone()
    }
}

static UNIQUE_COUNT: Mutex<u32> = Mutex::new(0);

fn generate_unique_label_name(name: String) -> String {
    let mut count = UNIQUE_COUNT.lock()
        .expect("Semantic Analyzer | Couldn't lock the TMP_COUNTER");

    *count = *count + 1;
    return format!("__llr_{}_{}", name, *count)
}
