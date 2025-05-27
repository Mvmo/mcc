use crate::parser::{Block, BlockItem, Expression, Program, Statement};

pub fn with_switch_cases(program: &Program) -> Program {
    // let function_body = block_collect_cases(&program.function_definition.body, &mut None, &mut false);

    todo!()
    // return Program {
    //     function_definition: FunctionDef {
    //         name: program.function_definition.name.clone(),
    //         body: function_body,
    //     }
    // }
}

fn statement_collect_cases(statement: &Statement, cases: &mut Option<Vec<Expression>>, default: &mut bool) -> Statement {
    match statement {
        Statement::Continue(..)
        | Statement::Break(..)
        | Statement::Goto(..)
        | Statement::Return(..)
        | Statement::Expression(..)
        | Statement::Null => statement.clone(),
        Statement::While { condition, body, label } => Statement::While {
            condition: condition.clone(),
            body: Box::new(statement_collect_cases(body, cases, default)),
            label: label.clone()
        },
        Statement::DoWhile { condition, body, label } => Statement::DoWhile {
            body: Box::new(statement_collect_cases(body, cases, default)),
            condition: condition.clone(),
            label: label.clone()
        },
        Statement::For { init, condition, post, body, label } => Statement::For {
            init: init.clone(),
            condition: condition.clone(),
            post: post.clone(),
            body: Box::new(statement_collect_cases(body.as_ref(), cases, default)),
            label: label.clone()
        },
        Statement::If { condition, then, _else } => Statement::If {
            condition: condition.clone(),
            then: Box::new(statement_collect_cases(then, cases, default)),
            _else: optional_statement_collect_cases(&_else.clone().map(|e| e.as_ref().clone()), cases, default).map(|statement| Box::new(statement)),
        },
        Statement::Case(expression, ..) => {
            cases.as_mut().unwrap().push(expression.clone());
            return statement.clone()
        },
        Statement::Default(..) => {
            *default = true;
            return statement.clone()
        }
        Statement::Switch { control_expression, body, label, cases: _, default: _ } => {
            let mut cases = Some(Vec::new());
            let mut default = false;

            return Statement::Switch {
                control_expression: control_expression.clone(),
                body: block_collect_cases(body, &mut cases, &mut default),
                label: label.clone(),
                cases,
                default
            }
        },
        Statement::Label(label, statement) => Statement::Label(
            label.clone(),
            Box::new(statement_collect_cases(statement.as_ref(), cases, default)),
        ),
        Statement::Compound(block) => Statement::Compound(block_collect_cases(block, cases, default)),
    }
}

fn block_collect_cases(block: &Block, cases: &mut Option<Vec<Expression>>, default: &mut bool) -> Block {
    let block_items = block.block_items.iter().map(|block_item| {
        match block_item {
            BlockItem::Statement(statement) => BlockItem::Statement(statement_collect_cases(statement, cases, default)),
            BlockItem::Declaration(decl) => BlockItem::Declaration(decl.clone()),
        }
    }).collect();

    return Block {
        block_items
    };
}

fn optional_statement_collect_cases(statement: &Option<Statement>, cases: &mut Option<Vec<Expression>>, default: &mut bool) -> Option<Statement> {
    return match statement {
        Some(statement) => Some(statement_collect_cases(statement, cases, default)),
        None => None
    }
}
