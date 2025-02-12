use std::fs;
use code_generator::CodeGenerator;
use inkwell::context::Context;

use crate::parser::AstNode;

pub mod lexer;
pub mod parser;
pub mod compiler;
pub mod code_generator;

fn main() {
    let binding = match fs::read_to_string("src/example.sqarl") {
        Ok(contents) => contents,
        Err(..) => panic!("Cannot read file"),
    };
    println!("Binding: {}", binding);
    let mut lexer = lexer::Lexer::new(&binding);
    lexer.lex();
    let mut p = parser::Parser::new(&binding);
    let node: AstNode = p.parse();
    println!("Nodes: {:?}", node);

    let context = Context::create();
    let mut code_generator = CodeGenerator::new(&context, "SQARL Compiler");
    code_generator.compile(node);
    code_generator.output();

}
