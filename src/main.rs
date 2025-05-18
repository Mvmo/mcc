use std::{path::PathBuf, process};

use clap::{Parser, command, arg};

mod preprocess;
mod lexer;
mod parser;
mod asm_gen;
mod code_emitter;
mod assembler;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct CompilerArgs {
    input_file: PathBuf,

    #[arg(long)]
    lex: bool,
    #[arg(long)]
    parse: bool,
    #[arg(long)]
    codegen: bool,
}

fn main() {
    let compiler_args = CompilerArgs::parse();

    let input_file = compiler_args.input_file.clone();
    let preprocessed = preprocess::with_gcc(input_file);

    let tokens = lexer::tokenize(preprocessed);
    if compiler_args.lex {
        process::exit(0);
    }

    let program = parser::parse(tokens);
    if compiler_args.parse {
        process::exit(0);
    }

    let asm = asm_gen::generate(program);
    if compiler_args.codegen {
        process::exit(0);
    }

    let output_assembly_file = compiler_args.input_file.with_extension("s");
    code_emitter::emit(asm, output_assembly_file.clone());
    assembler::assemble(output_assembly_file);
}
