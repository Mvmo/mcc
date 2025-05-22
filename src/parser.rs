use std::{collections::VecDeque, process};

use crate::lexer::Token;

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub body: Vec<BlockItem>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub function_definition: FunctionDef,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub name: String,
    pub initializer: Option<Expression>,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
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
    LogicalAnd,
    LogicalOr,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Clone)]
pub enum AssignmentOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Remainder,
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

impl AssignmentOperator {
}

#[derive(Debug, Clone)]
pub enum Expression {
    Const(i32),
    Var(String),
    Unary {
        operator: UnaryOperator,
        inner_expression: Box<Expression>
    },
    Binary {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Assignment(Option<AssignmentOperator>, Box<Expression>, Box<Expression>)
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    Null,
}

type Tokens = VecDeque<Token>;

pub fn parse(tokens: Vec<Token>) -> Program {
    let mut tokens_deque: Tokens = Tokens::from(tokens.iter().cloned().collect::<Vec<_>>());
    let program = parse_program(&mut tokens_deque);

    expect_token(&mut tokens_deque, Token::Eof);

    return program;
}

fn parse_program(tokens: &mut Tokens) -> Program {
    let function = parse_function(tokens);
    return Program {
        function_definition: function,
    }
}

fn parse_block_item(tokens: &mut Tokens) -> BlockItem {
    let next_token = tokens.front().expect("Parser | Expected token but didn't get one");
    return match next_token {
        Token::Int => BlockItem::Declaration(parse_declaration(tokens)),
        _ => BlockItem::Statement(parse_statement(tokens)),
    }
}

fn parse_declaration(tokens: &mut Tokens) -> Declaration {
    expect_token(tokens, Token::Int);
    let identifier = parse_identifier(tokens);

    let mut init_expr = None;
    let next_token_opt = tokens.front();
    if let Some(Token::Assign) = next_token_opt {
        expect_token(tokens, Token::Assign);
        init_expr = Some(parse_expression(tokens, 0));
    }

    expect_token(tokens, Token::Semicolon);
    return Declaration { name: identifier, initializer: init_expr }
}

fn parse_function(tokens: &mut Tokens) -> FunctionDef {
    expect_token(tokens, Token::Int);
    let identifier = parse_identifier(tokens);
    expect_token(tokens, Token::LeftParen);
    expect_token(tokens, Token::Void);
    expect_token(tokens, Token::RightParen);

    expect_token(tokens, Token::LeftBrace);

    let mut block_items = Vec::<BlockItem>::new();
    while *tokens.front().unwrap() != Token::RightBrace {
        let block_item = parse_block_item(tokens);
        block_items.push(block_item);
    }

    expect_token(tokens, Token::RightBrace);

    FunctionDef {
        name: identifier,
        body: block_items,
    }
}

fn parse_identifier(tokens: &mut Tokens) -> String {
    let token_option = tokens.pop_front();
    if let Some(Token::Identifier(value)) = token_option {
        return value
    }

    process::exit(4);
}

fn parse_factor_expression(tokens: &mut Tokens) -> Expression {
    let next_token = tokens.front().expect("Parser | Expect token but didn't have one");
    println!("{:?}", next_token);
    match next_token {
        Token::Const(_) => return Expression::Const(parse_int(tokens)),
        Token::Identifier(_) => return Expression::Var(parse_identifier(tokens)),
        Token::ComplementOp | Token::MinusOp | Token::LogicalNot => return Expression::Unary { operator: parse_unary_operator(tokens), inner_expression: Box::new(parse_factor_expression(tokens)) },
        Token::LeftParen => {
            expect_token(tokens, Token::LeftParen);
            let expr = parse_expression(tokens, 0);
            expect_token(tokens, Token::RightParen);

            return expr;
        },
        _ => process::exit(9),
    }
}

fn parse_expression(tokens: &mut Tokens, min_prec: i32) -> Expression {
    let mut left_expr = parse_factor_expression(tokens);

    let mut next_token = tokens.front().cloned().expect("Parser | Expect token but didn't have one.");
    while is_binary_operator(&next_token) && precedence(&next_token) >= min_prec {
        if is_assignment_operator(&next_token) {
            let assignment_operator = parse_assignment_operator(tokens);
            let right_expr = parse_expression(tokens, precedence(&next_token));
            left_expr = Expression::Assignment(assignment_operator, Box::new(left_expr), Box::new(right_expr));
        } else {
            let operator = parse_binary_operator(tokens);
            let right_expr = parse_expression(tokens, precedence(&next_token) + 1);
            left_expr = Expression::Binary { operator, left: Box::new(left_expr), right: Box::new(right_expr) };
        }
        next_token = tokens.front().cloned().expect("Parser | Expect next token but didn't have one.");
    }

    return left_expr
}

fn is_assignment_operator(token: &Token) -> bool {
    return matches!(
        token,
        Token::Assign
        | Token::PlusAssign
        | Token::MinusAssign
        | Token::MultiplyAssign
        | Token::DivideAssign
        | Token::RemainderAssign
        | Token::LeftShiftAssign
        | Token::RightShiftAssign
        | Token::BitwiseAndAssign
        | Token::BitwiseOrAssign
        | Token::BitwiseXorAssign
    )
}

fn precedence(token: &Token) -> i32 {
    return match token {
        Token::MultiplyOp | Token::DivideOp | Token::RemainderOp => 50,
        Token::PlusOp | Token::MinusOp => 45,
        Token::BitwiseLeftShift | Token::BitwiseRightShift => 40,
        Token::LessThan | Token::LessThanOrEqual | Token::GreaterThan | Token::GreaterThanOrEqual => 35,
        Token::Equal | Token::NotEqual => 34,
        Token::BitwiseAnd => 32,
        Token::BitwiseXor => 30,
        Token::BitwiseOr => 28,
        Token::LogicalAnd => 10,
        Token::LogicalOr => 5,
        Token::Assign
        | Token::PlusAssign
        | Token::MinusAssign
        | Token::MultiplyAssign
        | Token::DivideAssign
        | Token::RemainderAssign
        | Token::LeftShiftAssign
        | Token::RightShiftAssign
        | Token::BitwiseAndAssign
        | Token::BitwiseOrAssign
        | Token::BitwiseXorAssign
        => 1,
        _ => process::exit(2),
    }
}

fn parse_unary_operator(tokens: &mut Tokens) -> UnaryOperator {
    let operator_token = tokens.pop_front().expect("Parser | Expect token but didn't get one.");
    return match operator_token {
        Token::ComplementOp => UnaryOperator::Complement,
        Token::MinusOp => UnaryOperator::Negate,
        Token::LogicalNot => UnaryOperator::Not,
        _ => process::exit(2),
    }
}

fn is_binary_operator(token: &Token) -> bool {
    return match token {
        Token::MinusOp
        | Token::PlusOp
        | Token::MultiplyOp
        | Token::DivideOp
        | Token::RemainderOp
        | Token::BitwiseAnd
        | Token::BitwiseXor
        | Token::BitwiseOr
        | Token::BitwiseLeftShift
        | Token::BitwiseRightShift
        | Token::LogicalAnd
        | Token::LogicalOr
        | Token::Equal
        | Token::NotEqual
        | Token::LessThan
        | Token::LessThanOrEqual
        | Token::GreaterThan
        | Token::GreaterThanOrEqual
        | Token::Assign
        | Token::PlusAssign
        | Token::MinusAssign
        | Token::MultiplyAssign
        | Token::DivideAssign
        | Token::RemainderAssign
        | Token::LeftShiftAssign
        | Token::RightShiftAssign
        | Token::BitwiseAndAssign
        | Token::BitwiseOrAssign
        | Token::BitwiseXorAssign
        => true,
        _ => false,
    }
}

fn parse_assignment_operator(tokens: &mut Tokens) -> Option<AssignmentOperator> {
    let assignment_token = tokens.pop_front().unwrap(); // todo remove unwrap
    match assignment_token {
        Token::Assign => None,
        Token::PlusAssign => Some(AssignmentOperator::Plus),
        Token::MinusAssign => Some(AssignmentOperator::Minus),
        Token::MultiplyAssign => Some(AssignmentOperator::Multiply),
        Token::DivideAssign => Some(AssignmentOperator::Divide),
        Token::RemainderAssign => Some(AssignmentOperator::Remainder),
        Token::LeftShiftAssign => Some(AssignmentOperator::LeftShift),
        Token::RightShiftAssign => Some(AssignmentOperator::RightShift),
        Token::BitwiseAndAssign => Some(AssignmentOperator::BitwiseAnd),
        Token::BitwiseOrAssign => Some(AssignmentOperator::BitwiseOr),
        Token::BitwiseXorAssign => Some(AssignmentOperator::BitwiseXor),
        _ => process::exit(890),
    }
}

fn parse_binary_operator(tokens: &mut Tokens) -> BinaryOperator {
    let operator_token = tokens.pop_front().expect("Parser | Expect token but didn't get one.");
    return match operator_token {
        Token::MinusOp => BinaryOperator::Subtract,
        Token::PlusOp => BinaryOperator::Add,
        Token::MultiplyOp => BinaryOperator::Multiply,
        Token::DivideOp => BinaryOperator::Divide,
        Token::RemainderOp => BinaryOperator::Remainder,
        Token::BitwiseAnd => BinaryOperator::BitwiseAnd,
        Token::BitwiseXor => BinaryOperator::BitwiseXor,
        Token::BitwiseOr => BinaryOperator::BitwiseOr,
        Token::BitwiseLeftShift => BinaryOperator::BitwiseLeftShift,
        Token::BitwiseRightShift => BinaryOperator::BitwiseRightShift,
        Token::LogicalAnd => BinaryOperator::LogicalAnd,
        Token::LogicalOr => BinaryOperator::LogicalOr,
        Token::Equal => BinaryOperator::Equal,
        Token::NotEqual => BinaryOperator::NotEqual,
        Token::LessThan => BinaryOperator::LessThan,
        Token::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
        Token::GreaterThan => BinaryOperator::GreaterThan,
        Token::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
        _ => process::exit(5),
    }
}

fn parse_int(tokens: &mut Tokens) -> i32 {
    let token_option = tokens.pop_front();
    if let Some(Token::Const(int_value)) = token_option {
        return int_value
    }

    process::exit(8);
}

fn parse_statement(tokens: &mut Tokens) -> Statement {
    let next_token = tokens.front().expect("Parser | Expected token but didn't get one.");
    if *next_token == Token::Return {
        expect_token(tokens, Token::Return);
        let return_value = parse_expression(tokens, 0);
        expect_token(tokens, Token::Semicolon);
        return Statement::Return(return_value)
    }

    if *next_token == Token::Semicolon {
        expect_token(tokens, Token::Semicolon);
        return Statement::Null;
    }

    let expression = parse_expression(tokens, 0);
    expect_token(tokens, Token::Semicolon);

    return Statement::Expression(expression);
}

fn expect_token(tokens: &mut Tokens, expected: Token) -> Token {
    let token_option = tokens.pop_front();
    if token_option.as_ref().is_none_or(|token| *token != expected) {
        println!("Expected token {:?} but got {:?}", expected, token_option);
        process::exit(9);
    }

    return token_option.unwrap();
}
