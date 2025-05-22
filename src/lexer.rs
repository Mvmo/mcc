use std::process;
use regex::Regex;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Token {
    Identifier(String),
    Const(i32),

    // Keywords
    Int, // int
    Void, // void
    Return, // return
    If, // if
    Else, // else
    Goto, // goto

    // Symbols
    LeftParen, // (
    RightParen, // )
    LeftBrace, // {
    RightBrace, // }
    QuestionMark, // ?
    Colon, // :
    Semicolon, // ;
    DecrementOp, // --
    MinusOp, // -
    ComplementOp, // ~
    IncrementOp, // ++
    PlusOp, // +
    MultiplyOp, // *
    DivideOp, // /
    RemainderOp, // %
    BitwiseAnd, // &
    BitwiseOr, // |
    BitwiseXor, // ^
    BitwiseLeftShift, // <<
    BitwiseRightShift, // >>
    LogicalNot, // !
    LogicalAnd, // &&
    LogicalOr, // ||
    Equal, // ==
    NotEqual, // !=
    LessThan, // <
    LessThanOrEqual, // <=
    GreaterThan, // >
    GreaterThanOrEqual, // >=
    Assign, // =
    PlusAssign, // +=
    MinusAssign, // -=
    MultiplyAssign, // *=
    DivideAssign, // /=
    RemainderAssign, // %=
    BitwiseAndAssign, // &=
    BitwiseOrAssign, // |=
    BitwiseXorAssign, // ^=
    LeftShiftAssign, // <<=
    RightShiftAssign, // >>=

    Eof,
}

fn find_keyword_or_symbol(input: &str) -> Option<(Token, &str)> {
    let mut regexes_and_tokens = Vec::<(&str, Token)>::new();

    // keywords
    regexes_and_tokens.push((r"^int\b", Token::Int));
    regexes_and_tokens.push((r"^void\b", Token::Void));
    regexes_and_tokens.push((r"^return\b", Token::Return));
    regexes_and_tokens.push((r"^if\b", Token::If));
    regexes_and_tokens.push((r"^else\b", Token::Else));
    regexes_and_tokens.push((r"^int\b", Token::Int));
    regexes_and_tokens.push((r"^goto\b", Token::Goto));

    regexes_and_tokens.push((r"^\(", Token::LeftParen));
    regexes_and_tokens.push((r"^\)", Token::RightParen));
    regexes_and_tokens.push((r"^\{", Token::LeftBrace));
    regexes_and_tokens.push((r"^\}", Token::RightBrace));
    regexes_and_tokens.push((r"^\?", Token::QuestionMark));
    regexes_and_tokens.push((r"^\:", Token::Colon));
    regexes_and_tokens.push((r"^;", Token::Semicolon));
    regexes_and_tokens.push((r"^\+\+", Token::IncrementOp));
    regexes_and_tokens.push((r"^--", Token::DecrementOp));
    regexes_and_tokens.push((r"^-=", Token::MinusAssign));
    regexes_and_tokens.push((r"^-", Token::MinusOp));
    regexes_and_tokens.push((r"^~", Token::ComplementOp));
    regexes_and_tokens.push((r"^\+=", Token::PlusAssign));
    regexes_and_tokens.push((r"^\+", Token::PlusOp));
    regexes_and_tokens.push((r"^\*=", Token::MultiplyAssign));
    regexes_and_tokens.push((r"^\*", Token::MultiplyOp));
    regexes_and_tokens.push((r"^\/=", Token::DivideAssign));
    regexes_and_tokens.push((r"^\/", Token::DivideOp));
    regexes_and_tokens.push((r"^%=", Token::RemainderAssign));
    regexes_and_tokens.push((r"^%", Token::RemainderOp));
    regexes_and_tokens.push((r"^\&&", Token::LogicalAnd));
    regexes_and_tokens.push((r"^\&=", Token::BitwiseAndAssign));
    regexes_and_tokens.push((r"^\&", Token::BitwiseAnd));
    regexes_and_tokens.push((r"^\|\|", Token::LogicalOr));
    regexes_and_tokens.push((r"^\|=", Token::BitwiseOrAssign));
    regexes_and_tokens.push((r"^\|", Token::BitwiseOr));
    regexes_and_tokens.push((r"^\^=", Token::BitwiseXorAssign));
    regexes_and_tokens.push((r"^\^", Token::BitwiseXor));
    regexes_and_tokens.push((r"^<<=", Token::LeftShiftAssign));
    regexes_and_tokens.push((r"^<<", Token::BitwiseLeftShift));
    regexes_and_tokens.push((r"^>>=", Token::RightShiftAssign));
    regexes_and_tokens.push((r"^>>", Token::BitwiseRightShift));
    regexes_and_tokens.push((r"^!=", Token::NotEqual));
    regexes_and_tokens.push((r"^!", Token::LogicalNot));
    regexes_and_tokens.push((r"^==", Token::Equal));
    regexes_and_tokens.push((r"^=", Token::Assign));
    regexes_and_tokens.push((r"^<=", Token::LessThanOrEqual));
    regexes_and_tokens.push((r"^>=", Token::GreaterThanOrEqual));
    regexes_and_tokens.push((r"^<", Token::LessThan));
    regexes_and_tokens.push((r"^>", Token::GreaterThan));

    let result = regexes_and_tokens
        .iter()
        .find_map(|(regex_str, token)| {
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
