use std::fmt::format;

use strum_macros::Display;

use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq, Display)]
pub enum AstNode {
    Program(Vec<Box<AstNode>>),
    VariableDeclaration{ identifier: String, value: Expression },
    VariableAssignment { identifier: String, value: String},
    Expression(Expression),
    Eof,
}

#[derive(Debug, PartialEq, Display)]
pub enum Expression {
    Literal(String),
    Identifier(String),
    Binary(i32),
    BinaryOp(Box<Expression>, String, Box<Expression>),
    Elements(Vec<Token>),
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(string: &'a String) -> Self {
        let mut lexer = Lexer::new(string);
        let current_token = lexer.next_token();
        Parser {
            lexer,
            current_token,
        }

    }

    fn advance(&mut self) -> Token {
        self.current_token = self.lexer.next_token();
        let token = self.current_token.clone();
        token

    }


    fn expect(&mut self, tomatch: Token) -> Token {
        let token: Token = self.advance();
        if token == tomatch {
            token
        } else {
            let msg = format!("Unexpected token: {} != {}", token, tomatch);
            panic!("{}", msg)
        }
    }


    pub fn next_token(&mut self) -> AstNode {
        let token = self.current_token.clone();

        let node = match token {
            Token::Declare => self.variable_declaration(),
            Token::Set => self.variable_assignment(),
            Token::Eof => AstNode::Eof,
            _ => panic!("{}", format!("Cannot parse token: {:?}, next token: {:?}", token, self.advance()))
        };
        node
    }

    pub fn parse(&mut self) -> Vec<AstNode> {
        let mut node = self.next_token();
        let mut nodes = Vec::new();

        while node != AstNode::Eof {
            nodes.push(node);
            node = self.next_token();
        }

        nodes

    }

    pub fn parse_with_tokens(&mut self) -> Vec<AstNode> {
        let mut node = self.next_token();
        let mut nodes = Vec::new();

        while node != AstNode::Eof {
            nodes.push(node);
            node = self.next_token();
        }

        nodes

    }

    fn expression(&mut self) -> Expression {
        let mut token: Token = self.advance();
        let mut tokens = Vec::new();
        while !token.is_keyword() {
            tokens.push(token);
            token = self.advance();
        }

        Expression::Elements(tokens)
    }

    fn variable_declaration(&mut self) -> AstNode {
        let identifier = self.advance().to_string();
        self.expect(Token::Initially);
        let value = self.expression();

        AstNode::VariableDeclaration { identifier,  value }
    }

    fn variable_assignment(&mut self) -> AstNode {
        let identifier = self.advance().to_string();
        self.expect(Token::To);
        let value = self.advance().to_string();
        self.advance(); // temporary

        AstNode::VariableAssignment { identifier,  value }
    }
}
