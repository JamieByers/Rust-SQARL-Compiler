use strum_macros::Display;

use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq, Display)]
pub enum AstNode {
    Program(Vec<Box<AstNode>>),
    VariableDeclaration{ identifier: Expression, value: Expression },
    VariableAssignment { identifier: Expression, value: Expression },
    Expression(Expression),
    Eof,
}

#[derive(Debug, PartialEq, Display)]
pub enum Expression {
    Literal(String),
    IntegerLiteral(i32),
    FloatLiteral(f64),
    BooleanLiteral(bool),
    Identifier(String),
    BinaryOp(Box<Expression>, Token, Box<Expression>),
    Elements(Vec<Token>),
    Grouping(Box<Expression>),
    Unary(Token, Box<Expression>),
    ListIndex {
        list: Box<Expression>,
        index: Box<Expression>,
    },
    FunctionCall {
        name: Box<Expression>,
        parameters: Vec<Expression>,
    },
    MethodCall {
        prefix: Box<Expression>,
        postfix: Box<Expression>,
    }
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

    pub fn parse(&mut self) -> AstNode {
        let mut node = self.next_token();
        let mut nodes = Vec::new();

        while node != AstNode::Eof {
            nodes.push(Box::new(node));
            node = self.next_token();
        }

        AstNode::Program(nodes)

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

    fn expression(&mut self) -> Result<Expression, String> {
        let expr: Expression = self.equality()?;
        Ok(expr)

    }

    fn equality(&mut self) -> Result<Expression, String> {
        let mut expr = self.comparison()?;

        while matches!(self.current_token, Token::NotEquals | Token::EqualsEquals ) {
            let op = self.current_token.clone();
            self.advance();
            let right = self.expression()?;
            expr = Expression::BinaryOp(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression, String> {
        let mut expr = self.term()?;

        while matches!(self.current_token, Token::GreaterThan | Token::GreaterThanOrEqual | Token::LessThan | Token::LessThanOrEqual ) {
            let op = self.current_token.clone();
            self.advance();
            let right = self.expression()?;
            expr = Expression::BinaryOp(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression, String> {
        let mut expr = self.factor()?;

        while matches!(self.current_token, Token::Addition | Token::Subtraction ) {
            let op = self.current_token.clone();
            self.advance();
            let right = self.expression()?;
            expr = Expression::BinaryOp(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression, String> {
        let mut expr: Expression = self.unary()?;

        while matches!(self.current_token, Token::Division | Token::Multiplication) {
            let op = self.current_token.clone();
            self.advance();
            let right = self.expression()?;
            expr = Expression::BinaryOp(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, String> {
        if matches!(self.current_token, Token::Bang | Token::Subtraction) {
            let op = self.current_token.clone();
            self.advance();
            let right: Expression = self.unary()?;
            return Ok(Expression::Unary(op, Box::new(right)))
        }

        self.call()

    }

    fn call(&mut self) -> Result<Expression, String> {
        let mut expr = self.primary()?;

        loop {
            if self.current_token == Token::LeftBracket {
                expr = self.finish_call(expr)?;
            } else if self.current_token == Token::LeftSquareBracket {
                expr = self.finish_index(expr)?;
            } else if self.current_token == Token::Period {
                expr = self.method_call(expr)?;
            } else {
                break
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, func_name: Expression) -> Result<Expression, String> {
        self.advance(); // skip (
        let mut parameters = Vec::new();

        if self.current_token != Token::RightBracket {
            parameters.push(self.expression()?);

            while self.current_token == Token::Comma && self.current_token != Token::Eof {
                self.advance(); // consume comma
                parameters.push(self.expression()?);
            }
        }

        if self.current_token != Token::RightBracket {
            return Err(format!("Expected right bracket, got {}", self.current_token));
        }
        self.advance(); // skips )

        Ok(Expression::FunctionCall { name: Box::new(func_name), parameters  })

    }

    fn method_call(&mut self, prefix: Expression) -> Result<Expression, String> {
        self.advance(); // skipping .
        let postfix = self.expression()?;

        Ok(Expression::MethodCall { prefix: Box::new(prefix), postfix: Box::new(postfix) })
    }

    fn finish_index(&mut self, list_name: Expression) -> Result<Expression, String> {
        self.advance(); // skip [

        let index = self.expression()?;

        self.advance(); // skip ]

        Ok(Expression::ListIndex { list: Box::new(list_name), index: Box::new(index) })

    }

    fn primary(&mut self) -> Result<Expression, String>{
        let expr = match &self.current_token {
            Token::StringLiteral(val) => Ok(Expression::Literal(val.to_string())),
            Token::Int(val) => Ok(Expression::IntegerLiteral(*val)),
            Token::Float(val) => Ok(Expression::FloatLiteral(*val)),
            Token::Identifier(val) => Ok(Expression::Identifier(val.to_string())),
            Token::False => Ok(Expression::BooleanLiteral(false)),
            Token::True => Ok(Expression::BooleanLiteral(true)),
            Token::LeftBracket => {
                self.advance();
                let expr = self.expression()?;
                Ok(Expression::Grouping(Box::new(expr)))
            }

            _ => Err(format!("Failed primary parsing at token: {}", self.current_token)),

        };
        self.advance();
        expr
    }

    fn variable_declaration(&mut self) -> AstNode {
        self.advance();
        let identifier = self.expression().unwrap();
        self.advance();
        let value = self.expression().expect("Failed parsing expression in variable declaration");

        AstNode::VariableDeclaration { identifier,  value }
    }

    fn variable_assignment(&mut self) -> AstNode {
        self.advance();
        let identifier = self.expression().unwrap();
        self.advance();
        let value = self.expression().expect("Failed parsing expression");

        AstNode::VariableAssignment { identifier,  value }
    }
}
