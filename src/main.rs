use std::{path::PathBuf, process};

use clap::{Parser, command, arg};

mod preprocess;
mod lexer;
mod parser;
mod tacco_ir;
mod asm_gen;
mod asm_reg_resolver;
mod code_emitter;
mod assembler;

mod semantics;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct CompilerArgs {
    input_files: Vec<PathBuf>,

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
    #[arg(short)]
    c: bool,
}

fn main() {
    let compiler_args = CompilerArgs::parse();

    compiler_args.input_files.iter().for_each(|source| {
        compile_file(source, &compiler_args);
    });
}

fn compile_file(source: &PathBuf, compiler_args: &CompilerArgs) {
    let input_file = source.clone();
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
    let program = semantics::identifier_resolution::resolve_identifiers(&program);
    let program = semantics::loop_label_resolution::resolve_loop_labels(&program);
    let program = semantics::switch_resolution::perform(&program); // TODO merge switch resolution and case collector
    let program = semantics::case_collector::with_switch_cases(&program);

    if compiler_args.validate {
        println!("{:?}", program);
        process::exit(0);
    }

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

    let output_assembly_file = source.with_extension("s");
    code_emitter::emit(asm_program, output_assembly_file.clone());

    if compiler_args.c {
        assembler::perform_object_assembly(output_assembly_file);
    } else {
        assembler::perform_executable_assembly(output_assembly_file);
    }
}
