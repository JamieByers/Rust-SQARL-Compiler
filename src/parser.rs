use strum_macros::Display;

use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq, Display)]
pub enum AstNode {
    Program(Vec<Box<AstNode>>),
    VariableDeclaration{ identifier: Expression, value: Expression },
    VariableAssignment { identifier: Expression, value: Expression },
    SendToDisplay { value: Expression },
    IfStatement { condition: Expression, code_block: Vec<AstNode> },
    WhileStatement { condition: Expression, code_block: Vec<AstNode> },
    RepeatUntilLoop { command: Expression, until: Expression },
    RepeatTimesLoop { command: Expression, times: Expression },
    ForFromLoop { id: Token, lower: Expression, higher: Expression, step: Option<Expression>, code_block: Vec<AstNode>},
    ForEachLoop { id: Token, from: Expression, code_block: Vec<AstNode>},
    FunctionDeclaration { identifier: Token, params: Vec<Expression>, code_block: Vec<AstNode>, return_type: Token },
    ProcedureDeclaration { identifier: Token, params: Vec<Expression>, code_block: Vec<AstNode> },
    ReturnStatement { value: Expression },
    CodeBlock (Vec<AstNode>),
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
            Token::Send => self.send_to_display(),
            Token::If => self.if_statement(),
            Token::For => self.handle_for_loops(),
            Token::While => self.while_statement(),
            Token::Function => self.function_declaration(),
            Token::Procedure => self.procedure_declaration(),
            Token::Return => self.return_statement(),
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

        while matches!(self.current_token, Token::NotEquals | Token::EqualsEquals | Token::Equals ) {
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

    fn send_to_display(&mut self) -> AstNode {
        self.advance(); // skip SEND
        let value = self.expression().unwrap();
        self.advance(); // skip TO
        self.advance(); // skip DISPLAY

        AstNode::SendToDisplay { value }
    }

    fn parse_block(&mut self) -> Vec<AstNode> {
        self.advance(); // skip THEN
        let mut block = Vec::new();
        while self.current_token != Token::End {
            let node = self.next_token();
            block.push(node);
        }
        self.advance(); // skip END

        block
    }

    fn if_statement(&mut self) -> AstNode {
        self.advance(); // skip if
        let condition = self.expression().unwrap();
        let code_block = self.parse_block();

        AstNode::IfStatement { condition, code_block }
    }

    fn while_statement(&mut self) -> AstNode {
        self.advance(); // skip if
        let condition = self.expression().unwrap();
        let code_block = self.parse_block();

        AstNode::WhileStatement { condition, code_block }
    }

    fn handle_for_loops(&mut self) -> AstNode {
        self.advance(); // skip FOR to either EACH or an identifier

        let identifier: Token = self.current_token.clone();
        if identifier == Token::Each {
            self.for_each_loop()
        } else {
            self.for_from_loop(identifier)
        }

    }

    fn for_each_loop(&mut self) -> AstNode {
        let identifier: Token = self.advance(); // skip EACH

        self.advance(); // skip past FROM 

        let from: Expression = self.expression().unwrap();

        let code_block: Vec<AstNode> = self.parse_block();

        self.advance(); // skip the EACH at the end of END FOR EACH 

        AstNode::ForEachLoop { id: identifier, from, code_block }
    }

    fn for_from_loop(&mut self, identifier: Token) -> AstNode {
        self.advance(); // move to from 
        self.advance(); // move up to expression 

        let lower: Expression = self.expression().unwrap();
        self.advance(); // skip TO 
        let higher: Expression = self.expression().unwrap();

        let mut step: Option<Expression> = None;
        if self.current_token == Token::Step {
            step = Some(self.handle_step());
        } 

        let code_block: Vec<AstNode> = self.parse_block();

        AstNode::ForFromLoop { id: identifier, lower, higher, step, code_block }

    }

    fn handle_step(&mut self) -> Expression {
        self.advance(); // skip step 
        let step = self.expression().unwrap();
        step
    }

    fn subprogram(&mut self) -> (Token, Vec<Expression>) {
        let identifier = self.advance(); // pass by FUNCTION or PROCEDURE toward the identifier
        self.expect(Token::LeftBracket); // move up to (
        self.advance(); // skip (

        let mut params = Vec::new();
        while self.current_token != Token::RightBracket {
            while self.current_token == Token::Comma {
                self.advance();
            }
            let expr = self.expression().unwrap();
            params.push(expr);
        }




        (identifier, params)

    }


    fn function_declaration(&mut self) -> AstNode {
        let (identifier, params) = self.subprogram();
        self.advance(); // skip )

        self.advance(); // skip RETURN
        let return_type = Some(self.current_token.clone());

        let code_block = self.parse_block();

        AstNode::FunctionDeclaration { identifier, params, code_block, return_type: return_type.unwrap() }

    }

    fn procedure_declaration(&mut self) -> AstNode {
        let (identifier, params) = self.subprogram();

        let code_block = self.parse_block();

        AstNode::ProcedureDeclaration { identifier, params, code_block }

    }

    fn return_statement(&mut self) -> AstNode {
        self.advance();
        let value = self.expression().unwrap();

        AstNode::ReturnStatement { value }
    }
}
