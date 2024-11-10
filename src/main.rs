use std::fs;

pub mod lexer;
pub mod parser;
pub mod code_generator;


fn main() {

    let binding = match fs::read_to_string("src/test.sqarl") {
        Ok(contents) => contents,
        Err(..) => panic!("Cannot read file"),
    };
    println!("Binding: {}", binding);
    let mut lexer = lexer::Lexer::new(&binding);
    lexer.lex();
    let mut p = parser::Parser::new(&binding);
    let tokens = p.parse();

    println!("Tokens: {:?}", tokens);
}



