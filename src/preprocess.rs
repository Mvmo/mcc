use std::{fs, path::PathBuf, process::{self, Command}};

use uuid::Uuid;

pub fn with_gcc(source_file: PathBuf) -> String {
    let source_file_path = source_file.canonicalize().unwrap();

    let tmp_name = format!("{}.c.preprocessed", Uuid::new_v4().to_string().split_at(4).1);
    let tmp_preprocessed_path = source_file.parent().unwrap().join(tmp_name);

    let command_result = Command::new("gcc")
        .args(vec![
            "-E",
            "-P",
            &source_file_path.to_string_lossy(),
            "-o" ,
            &tmp_preprocessed_path.to_string_lossy(),
        ])
        .spawn()
        .expect("Preprocessor | Something wen't wrong spawning the process of the gcc preprocessor.")
        .wait()
        .expect("Preprocessor | Something wen't wrong executing the gcc preprocessor.");

    if !command_result.success() {
        println!("Preprocessor | Something went wrong invoking the gcc preprocessor. Now exiting.");
        process::exit(1);
    }

    let content = fs::read_to_string(tmp_preprocessed_path.clone())
        .expect("Preprocessor | Can't read preprocessed file");

    // just try and ignore :D
    fs::remove_file(tmp_preprocessed_path)
        .ok();

    return content
}
