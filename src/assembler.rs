use std::{path::PathBuf, process::{self, Command}};

pub fn with_gcc(source: PathBuf) {
    let command_result = Command::new("gcc")
        .args(vec![
            &source.to_string_lossy(),
            "-o" ,
            &source.with_extension("").to_string_lossy(),
        ])
        .spawn()
        .expect("Assembler | Something wen't wrong spawning the process of the gcc assembler.")
        .wait()
        .expect("Assembler | Something wen't wrong executing the gcc assembler.");

    if !command_result.success() {
        process::exit(6);
    }
}
