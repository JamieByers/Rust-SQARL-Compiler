use std::fs;

fn main() {

    let binding: String = match fs::read_to_string("src/example.sqarl") {
        Ok(contents) => contents,
        Err(e) => panic!("Error reading file: {}", e),
    };

    let mut l = lexer::Lexer::new(&binding);
    l.lex();

}



