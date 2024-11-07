use crate::lexer::{Lexer, Token};
use strum_macros::Display;

#[derive(Debug, Display, PartialEq)]
pub enum AstNode {
    Program(Vec<Box<AstNode>>),
    VariableDeclaration{ identifier: String, value: String },
    VariableAssignment { identifier: String, value: String},
    IfStatement { condition: AstNode::Expression , code_block: Vec<AstNode> },
    Expression { value: None },
    Eof
}

pub struct Parser<'a> {
    lexer: Lexer<'a>
}

impl<'a> Parser<'a> {
    pub fn new(string: &'a String) -> Self {
        let lexer = Lexer::new(string);
        Parser {
            lexer
        }

    }

    pub fn parse_into_tokens(&mut self) -> Vec<AstNode> {
        let mut token = self.next_token();
        let mut tokens = Vec::new();
        while token != AstNode::Eof {
            tokens.push(token);
            token = self.next_token();
        }
       tokens
    }

    pub fn parse(&mut self) -> AstNode{
        let mut token = self.next_token();
        let mut tokens = Vec::new();
        while token != AstNode::Eof {
            tokens.push(Box::new(token));
            token = self.next_token();
        }
        AstNode::Program(tokens)
    }


    fn advance(&mut self) -> Token {
        self.lexer.next_token()
    }

    pub fn next_token(&mut self) -> AstNode{
        let token = self.lexer.next_token();
        println!("Current token: {}", token);

        match token {
            Token::Declare => self.variable_declaration(),
            Token::Set => self.variable_assignment(),
            Token::Eof => AstNode::Eof,
            _ => panic!("cannot parse {}", token)
        }
    }

    fn variable_declaration(&mut self) -> AstNode {
        let identifier = self.advance().get_value(); // moves up to identifier
        self.advance(); // moves up to initially
        let value = self.advance().get_value(); // moves up to value

        AstNode::VariableDeclaration { identifier, value }
    }

    fn variable_assignment(&mut self) -> AstNode {
        let identifier = self.advance().get_value(); // moves up to identifier
        self.advance(); // moves up to initially
        let value = self.advance().get_value(); // moves up to value

        AstNode::VariableAssignment { identifier, value }
    }

    fn if_statement(&mut self) -> AstNode {

    }

}
