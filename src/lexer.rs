use std::{collections::HashMap, process};
use regex::Regex;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Token {
    Identifier(String),
    Const(i32),

    // Keywords
    Int,
    Void,
    Return,

    // Symbols
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,

    Eof,
}

fn find_keyword_or_symbol(input: &str) -> Option<(Token, &str)> {
    let mut regex_map = HashMap::new();

    // keywords
    regex_map.insert(r"^int\b", Token::Int);
    regex_map.insert(r"^void\b", Token::Void);
    regex_map.insert(r"^return\b", Token::Return);

    // symbols
    regex_map.insert(r"^\(", Token::LeftParen);
    regex_map.insert(r"^\)", Token::RightParen);
    regex_map.insert(r"^\{", Token::LeftBrace);
    regex_map.insert(r"^\}", Token::RightBrace);
    regex_map.insert(r"^;", Token::Semicolon);

    let result = regex_map.iter().find_map(|(regex_str, token)| {
        let regex = Regex::new(regex_str).unwrap();
        regex.find(input).map(|value| (token, value))
    });

    if let Some(value) = result {
        return Some((
            value.0.clone(),
            input.split_at(value.1.len()).1
        ))
    }

    None
}

fn find_identifier(input: &str) -> Option<(Token, &str)> {
    let regex = Regex::new(r"^[a-zA-Z_]\w*\b").unwrap();
    let result = regex.find(input);

    if result.is_none() {
        return None
    }

    let result = result.unwrap();

    return Some((
        Token::Identifier(result.as_str().into()),
        input.split_at(result.len()).1
    ))
}

fn find_number(input: &str) -> Option<(Token, &str)> {
    let regex = Regex::new(r"^[0-9]+\b").unwrap();
    let result = regex.find(input);

    if result.is_none() {
        return None
    }

    let result = result.unwrap();

    return Some((
        Token::Const(result.as_str().parse::<i32>().unwrap()),
        input.split_at(result.len()).1
    ))
}

fn find_token(input: &str) -> Option<(Token, &str)> {
    let keyword_option = find_keyword_or_symbol(input);
    if keyword_option.is_some() {
        return keyword_option;
    }

    let identifier_option = find_identifier(input);
    if identifier_option.is_some() {
        return identifier_option;
    }

    let constant_option = find_number(input);
    if constant_option.is_some() {
        return constant_option;
    }

    None
}

pub fn tokenize(input: String) -> Vec<Token> {
    let mut input = input;
    let mut tokens = Vec::<Token>::new();

    while input.len() != 0 {
        if input.starts_with(" ") || input.starts_with("\t") || input.starts_with("\n") {
            input = input.trim_start().to_string();
            continue;
        }

        let token_option = find_token(&input);
        if token_option.is_none() {
            process::exit(1);
        }

        let (token, remaining_input) = token_option.unwrap();
        tokens.push(token);
        input = remaining_input.to_string();
    }

    tokens.push(Token::Eof);
    return tokens
}
