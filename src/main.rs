use std::{path::PathBuf, process};

use clap::{Parser, command, arg};

mod preprocess;
mod lexer;
mod parser;
mod semantic_analyzer;
mod tacco_ir;
mod asm_gen;
mod asm_reg_resolver;
mod code_emitter;
mod assembler;
mod case_collector;

mod semantics;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct CompilerArgs {
    input_file: PathBuf,

    #[arg(long)]
    lex: bool,
    #[arg(long)]
    parse: bool,
    #[arg(long)]
    validate: bool,
    #[arg(long)]
    tacco: bool,
    #[arg(long)]
    codegen: bool,
}

fn main() {
    let compiler_args = CompilerArgs::parse();

    let input_file = compiler_args.input_file.clone();
    let preprocessed = preprocess::with_gcc(input_file);

    let tokens = lexer::tokenize(preprocessed);
    if compiler_args.lex {
        println!("{:?}", tokens);
        process::exit(0);
    }

    let program = parser::parse(tokens);
    if compiler_args.parse {
        println!("{:?}", program);
        process::exit(0);
    }

    semantics::no_duplicate_default::check_for_duplicate_defaults(&program);

    let validated_program = semantics::label_resolve::perform(
        semantic_analyzer::validate(program)
    );


    if compiler_args.validate {
        println!("{:?}", validated_program);
        process::exit(0);
    }

    let program = case_collector::with_switch_cases(&validated_program);

    let tacco_ir_program = tacco_ir::transform(program);
    if compiler_args.tacco {
        println!("{:?}", tacco_ir_program);
        process::exit(0);
    }

    let asm_program = asm_reg_resolver::resolve_pseudo_registers(
        &asm_gen::generate(tacco_ir_program)
    );

    if compiler_args.codegen {
        println!("{:?}", asm_program);
        process::exit(0);
    }

    let output_assembly_file = compiler_args.input_file.with_extension("s");
    code_emitter::emit(asm_program, output_assembly_file.clone());
    assembler::with_gcc(output_assembly_file);
}
