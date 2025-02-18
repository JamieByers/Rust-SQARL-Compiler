use std::process::{Command, Output};
use std::fs;
use code_generator::CodeGenerator;
use inkwell::context::Context;
use std::env;
use crate::parser::AstNode;

pub mod lexer;
pub mod parser;
pub mod compiler;
pub mod code_generator;

fn main() {
    let cwd = match env::current_dir() {
        Ok(path) => {
            let path_str = path.to_string_lossy().to_string();
            path_str
        }
        _ => panic!("Error turning cwd into a string"),
    };

    let args: Vec<String> = env::args().collect();
    let file = &args[1];
    let file_split: Vec<_> = file.split(".").collect();
    let file_name = file_split[0];

    let input = format!("{}/{}", cwd, file);

    let binding = match fs::read_to_string(input) {
        Ok(contents) => contents,
        Err(..) => panic!("Cannot read file"),
    };

    println!("Binding: {}", binding);
    let mut lexer = lexer::Lexer::new(&binding);
    lexer.lex();
    let mut p = parser::Parser::new(&binding);
    let node: AstNode = p.parse();
    println!("");
    println!("Nodes: {:?}", node);

    let context = Context::create();
    let mut code_generator = CodeGenerator::new(&context, "SQARL Compiler");
    code_generator.compile(node);
    code_generator.output(file_name);

    if args.len() == 2 {
        output_llvm(file_name);
    } else if args.len() > 2 {
        if &args[2] == "--build" {
            build_llvm(file_name)
        }
    }

}

// lli filename.ll
fn output_llvm(file: &str) {
       let output: Output = Command::new("lli")
            .arg(format!("{}.ll", file))
            .output()
            .expect("Failed running lli");

        println!("");
        println!("Output: ");

        let stdout = String::from_utf8(output.stdout).expect("Couldnt turn stdout to string from bytes");
        let trimmed_stdout = stdout.trim();
        println!("{}", trimmed_stdout);
}

// llc -filetype=obj output.ll -o output.o
// clang output.o -o program
// (./program)

fn build_llvm(file: &str) {
    let status = Command::new("llc")
        .arg("-filetype=obj")
        .arg(format!("{}.ll", file))
        .arg("-o")
        .arg(format!("{}.o", file))
        .status()
        .expect("Failed to run llc");

    if !status.success() {
        panic!("llc failed with status: {}", status);
    }

    let status = Command::new("clang")
        .arg(format!("{}.o", file))
        .arg("-o")
        .arg(file)
        .status()
        .expect("Failed to run clang");

    if !status.success() {
        panic!("clang failed with status: {}", status);
    }

    println!("Compiled code into executable: {}", file);
}



