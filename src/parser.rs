use std::{collections::VecDeque, process};

use crate::lexer::Token;

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub body: Statement,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub function_definition: FunctionDef,
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
pub enum Expression {
    Const(i32),
    Unary {
        operator: UnaryOperator,
        inner_expression: Box<Expression>
    },
    Binary {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
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

fn parse_function(tokens: &mut Tokens) -> FunctionDef {
    expect_token(tokens, Token::Int);
    let identifier = parse_identifier(tokens);
    expect_token(tokens, Token::LeftParen);
    expect_token(tokens, Token::Void);
    expect_token(tokens, Token::RightParen);
    expect_token(tokens, Token::LeftBrace);
    let statement = parse_statement(tokens);
    expect_token(tokens, Token::RightBrace);

    FunctionDef {
        name: identifier,
        body: statement
    }
}

fn parse_identifier(tokens: &mut Tokens) -> String {
    let token_option = tokens.pop_front();
    if let Some(Token::Identifier(value)) = token_option {
        return value
    }

    process::exit(2);
}

fn parse_factor_expression(tokens: &mut Tokens) -> Expression {
    let next_token = tokens.front().expect("Parser | Expect token but didn't have one");
    match next_token {
        Token::Const(_) => return Expression::Const(parse_int(tokens)),
        Token::ComplementOp | Token::MinusOp | Token::LogicalNot => return Expression::Unary { operator: parse_unary_operator(tokens), inner_expression: Box::new(parse_factor_expression(tokens)) },
        Token::LeftParen => {
            expect_token(tokens, Token::LeftParen);
            let expr = parse_expression(tokens, 0);
            expect_token(tokens, Token::RightParen);

            return expr;
        },
        _ => process::exit(2),
    }
}

fn parse_expression(tokens: &mut Tokens, min_prec: i32) -> Expression {
    let mut left_expr = parse_factor_expression(tokens);

    let mut next_token = tokens.front().cloned().expect("Parser | Expect token but didn't have one.");
    while is_binary_operator(&next_token) && precedence(&next_token) >= min_prec {
        let operator = parse_binary_operator(tokens);
        let right_expr = parse_expression(tokens, precedence(&next_token) + 1);
        left_expr = Expression::Binary { operator, left: Box::new(left_expr), right: Box::new(right_expr) };
        next_token = tokens.front().cloned().expect("Parser | Expect next token but didn't have one.");
    }

    return left_expr
}

fn precedence(token: &Token) -> i32 {
    return match token {
        Token::MultiplyOp | Token::DivideOp | Token::RemainderOp => 50,
        Token::PlusOp | Token::MinusOp => 45,
        Token::BitwiseLeftShift | Token::BitwiseRightShift => 40,
        Token::LessThan | Token::LessThanOrEqual | Token::GreaterThan | Token::GreaterThanOrEqual => 38,
        Token::Equal | Token::NotEqual => 36,
        Token::BitwiseAnd => 32,
        Token::BitwiseXor => 30,
        Token::BitwiseOr => 28,
        Token::LogicalAnd => 24,
        Token::LogicalOr => 22,
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
        => true,
        _ => false,
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
        _ => process::exit(2),
    }
}

fn parse_int(tokens: &mut Tokens) -> i32 {
    let token_option = tokens.pop_front();
    if let Some(Token::Const(int_value)) = token_option {
        return int_value
    }

    process::exit(2);
}

fn parse_statement(tokens: &mut Tokens) -> Statement {
    expect_token(tokens, Token::Return);
    let return_value = parse_expression(tokens, 0);
    expect_token(tokens, Token::Semicolon);
    return Statement::Return(return_value)
}

fn expect_token(tokens: &mut Tokens, expected: Token) -> Token {
    let token_option = tokens.pop_front();
    if token_option.as_ref().is_none_or(|token| *token != expected) {
        process::exit(2);
    }

    return token_option.unwrap();
}
