use std::fs;
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

    let mut compiler = compiler::Compiler::new(node.clone());
    compiler.compile();

    println!("Nodes: {:?}", node);
}



