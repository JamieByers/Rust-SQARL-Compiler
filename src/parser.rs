use crate::lexer::{self, Lexer};

#[derive(Debug)]
pub enum AST_node {
    Program(Vec<Box<AST_node>>),
    VariableDeclaration{ identifier: String, value: String },
    VariableAssignment { identifier: String, value: String},
}

pub struct Parser<'a> {
    _string: String,
    lexer: Lexer<'a>
}

impl<'a> Parser<'a> {
    pub fn new(string: String) -> Self {
        let lexer = lexer::Lexer::new(&string);
        Parser {
            _string: string,
            lexer: lexer,
        }

    }

    fn parse() {

    }
}