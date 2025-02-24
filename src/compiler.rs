use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::code_generator::*;
use std::process::{Command, Output};
use inkwell::context::Context;
use crate::parser::AstNode;


pub struct Compiler {
    pub binding: String,
}

impl Compiler {
    pub fn new(binding: &String) -> Self {
        Compiler {
            binding: binding.to_string(),
        }
    }

    pub fn test(binding: String, file_name: String) -> String {
        let mut compiler = Compiler::new(&binding);
        compiler.compile(file_name.to_string());
        let output = compiler.output_llvm(&file_name);
        output

    }

    pub fn compile(&mut self, file_name: String) {
        println!("Binding: \n {}", self.binding);
        let mut lexer = Lexer::new(&self.binding);
        lexer.lex();
        let mut p = Parser::new(&self.binding);
        let node: AstNode = p.parse();
        println!("");
        println!("Nodes: {:?}", node);

        let context = Context::create();
        let mut code_generator = CodeGenerator::new(&context, "SQARL Compiler");
        code_generator.compile(node);
        code_generator.output(&file_name);

    }

    pub fn cleanup_compiled_files(&self, file: &str) {
        let _ = std::fs::remove_file(format!("compiled_tests/{}.ll", file));
        let _ = std::fs::remove_file(format!("compiled_tests/{}.o", file));
        let _ = std::fs::remove_file(format!("compiled_tests/{}", file));
    }


    // lli filename.ll
    pub fn output_llvm(&mut self, file: &str) -> String {
        let mut path = "";
        if file.contains("test") {
            path = "compiled_tests/";
        }

        let output: Output = Command::new("lli")
            .arg(format!("{}{}.ll", path, file))
            .output()
            .expect("Failed running lli");

        println!("");
        println!("Output: ");

        let stdout = String::from_utf8(output.stdout).expect("Couldnt turn stdout to string from bytes");
        let trimmed_stdout = stdout.trim().to_string();
        println!("{}", trimmed_stdout);

        // self.cleanup_compiled_files(file);

        trimmed_stdout
    }

    // llc -filetype=obj output.ll -o output.o
    // clang output.o -o program
    // (./program)


    pub fn build_llvm(&mut self, file: &str) {
        let mut path = "";
        if file.contains("test") {
            path = "compiled_tests/";
        }

        let status = Command::new("llc")
            .arg("-filetype=obj")
            .arg(format!("{}{}.ll", path, file))
            .arg("-o")
            .arg(format!("{}{}.o", path, file))
            .status()
            .expect("Failed to run llc");

        if !status.success() {
            panic!("llc failed with status: {}", status);
        }

        let status = Command::new("clang")
            .arg(format!("{}{}.o", path, file))
            .arg("-o")
            .arg(format!("{}{}", path, file))
            .status()
            .expect("Failed to run clang");

        if !status.success() {
            panic!("clang failed with status: {}", status);
        }

        println!("Compiled code into executable: compiled_tests/{}", file);
    }


}
