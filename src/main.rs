use std::fs;
pub mod lexer;
pub mod parser;


fn main() {

    let binding: String = match fs::read_to_string("src/example.sqarl") {
        Ok(contents) => contents,
        Err(e) => panic!("Error reading file: {}", e),
    };

    let mut p = parser::Parser::new(&binding);
    p.next_token();

}



