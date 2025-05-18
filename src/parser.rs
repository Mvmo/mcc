use std::{collections::VecDeque, fmt, process};

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
pub enum Expression {
    Const(i32),
    Unary {
        operator: Token,
        inner_expression: Box<Expression>
    },
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

fn parse_expression(tokens: &mut Tokens) -> Expression {
    let ts = tokens.clone();
    let current_token = ts.front().unwrap(); // TODO: remove unwrap

    return match current_token {
        Token::Const(_) => Expression::Const(parse_int(tokens)),
        Token::ComplementOp | Token::MinusOp | Token::DecrementOp => {
            tokens.pop_front();
            return Expression::Unary { operator: current_token.clone(), inner_expression: Box::new(parse_expression(tokens)) }
        },
        Token::LeftParen => {
            expect_token(tokens, Token::LeftParen);
            let expr = parse_expression(tokens);
            expect_token(tokens, Token::RightParen);

            return expr;
        }
        _ => {
            process::exit(2);
        }
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
    let return_value = parse_expression(tokens);
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
