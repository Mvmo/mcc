use std::{collections::VecDeque, fmt, process};

use crate::lexer::Token;

struct FunctionDef {
    name: String,
    body: Statement,
}

pub struct Program {
    function_definition: FunctionDef,
}

enum Expression {
    Const(i32)
}

enum Statement {
    Return(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "")
    }
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
    return Expression::Const(parse_int(tokens))
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
