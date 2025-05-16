use std::{collections::HashMap, fs, path::PathBuf, process::{self, Command}};

use regex::Regex;
use clap::{Parser, command, arg};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct CompilerArgs {
    input_file: PathBuf,

    #[arg(long)]
    lex: bool,
}

#[derive(Clone, Debug)]
enum Token {
    Identifier(String),
    Constant(i32),

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
        Token::Constant(result.as_str().parse::<i32>().unwrap()),
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

fn get_sibling_file(original_path: &PathBuf, sibling_name: &str) -> Option<PathBuf> {
    if let Some(parent_dir) = original_path.parent() {
        let sibling_path = parent_dir.join(sibling_name);
        Some(sibling_path)
    } else {
        None
    }
}

fn format_command(command: &Command) -> String {
    let mut formatted = String::new();
    formatted.push_str(command.get_program().to_string_lossy().as_ref());

    for arg in command.get_args() {
        formatted.push(' ');
        formatted.push_str(arg.to_string_lossy().as_ref());
    }

    formatted
}

fn main() {
    let compiler_args = CompilerArgs::parse();
    // println!("Input file: {:?}", compiler_args.input_file);
    // println!("LexMode?: {}", compiler_args.lex);
    //

    let file_name = compiler_args.input_file.canonicalize().unwrap().to_string_lossy().into_owned();
    let file_name = dbg!(file_name);
    let sf = file_name.clone();
    let preproc_file_name = file_name + "-pregcc";

    let mut command = Command::new("gcc");
    command.args(vec![
        "-E",
        "-P",
        &sf,
        "-o" ,
        &preproc_file_name,
    ]);

    let fmt_cmd = format_command(&command);
    println!("GCC Command: {}", fmt_cmd);

    command.spawn().unwrap().wait().unwrap();

    let mut input = String::new();
    match get_sibling_file(&compiler_args.input_file, &preproc_file_name) {
        Some(sibling_path) => {
            match fs::read_to_string(&sibling_path) {
                Ok(contents) => {
                    input = contents;
                }
                Err(e) => {
                    eprintln!("Error reading preprocessor file '{}': {}", sibling_path.display(), e);
                }
            }
        }
        None => {
            eprintln!("Error: Could not determine the path to the preprocessor file.");
        }
    }

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
        println!("pushed token");
        input = remaining_input.to_string();
    }

    tokens.iter().for_each(|token| {
        dbg!(token);
    });
}
