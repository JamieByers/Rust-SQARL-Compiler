use crate::lexer::{Lexer, Token};

#[derive(Debug)]
pub enum AstNode {
    Program(Vec<Box<AstNode>>),
    VariableDeclaration{ identifier: String, value: String },
    VariableAssignment { identifier: String, value: String},
    Expression(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Literal(String),
    Identifier(String),
    Binary(i32),
    BinaryOp(Box<Expression>, String, Box<Expression>)
}

pub struct Parser<'a> {
    lexer: Lexer<'a>
}

impl<'a> Parser<'a> {
    pub fn new(string: &'a String) -> Self {
        let lexer = Lexer::new(string);
        Parser {
            lexer: lexer,
        }

    }

    fn advance(&mut self) -> Token {
        self.lexer.next_token()
    }

    fn expect(&mut self, tomatch: Token) -> Token {
        let token: Token = self.lexer.next_token();
        if token == tomatch {
            token
        } else {
            let msg = format!("Unexpected token: {} != {}", token, tomatch);
            panic!("{}", msg)
        }
    }


    pub fn next_token(&mut self) {
        let token = self.lexer.next_token();

        match token {
            Token::Declare => self.variable_declaration(),
            _ => panic!("cannot parse {}", token)
        }
    }

    fn expression(&mut self) -> Expression {
        let mut token: Token = self.advance();
        while token != Token::Keyword {
            todo!();
        }

        Expression::Literal("".to_string())
    }

    fn variable_declaration(&mut self) -> AstNode::VariableDeclaration {
        let identifier = self.advance();



        AstNode::VariableDeclaration { identifier: (), value: () }
    }

}
