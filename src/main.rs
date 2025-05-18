use std::path::PathBuf;

use clap::{Parser, command, arg};

mod preprocess;
mod lexer;
mod parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct CompilerArgs {
    input_file: PathBuf,

    #[arg(long)]
    lex: bool,
    #[arg(long)]
    parse: bool,
}

fn main() {
    let compiler_args = CompilerArgs::parse();
    let preprocessed = preprocess::with_gcc(compiler_args.input_file);
    let tokens = lexer::tokenize(preprocessed);
    let program = parser::parse(tokens);
}
