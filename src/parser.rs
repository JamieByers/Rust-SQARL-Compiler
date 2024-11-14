use strum_macros::Display;

use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq, Display)]
pub enum AstNode {
    Program(Vec<Box<AstNode>>),
    VariableDeclaration{ identifier: Expression, value: Expression, var_type: Type },
    VariableAssignment { identifier: Expression, value: Expression },
    SendToDisplay { value: Expression },
    IfStatement { condition: Expression, code_block: Vec<AstNode>, elif_statements: Vec<ElseIfStatement>, else_statement: Option<ElseStatement> },
    WhileStatement { condition: Expression, code_block: Vec<AstNode> },
    RepeatUntilLoop { command: Expression, until: Expression },
    RepeatTimesLoop { command: Expression, times: Expression },
    ForFromLoop { id: Token, lower: Expression, higher: Expression, step: Expression, code_block: Vec<AstNode>},
    ForEachLoop { id: Token, from: Expression, code_block: Vec<AstNode>},
    FunctionDeclaration { identifier: Token, params: Vec<Parameter>, code_block: Vec<AstNode>, return_type: Token },
    ProcedureDeclaration { identifier: Token, params: Vec<Parameter>, code_block: Vec<AstNode> },
    OpenFile { file: Expression },
    CloseFile { file: Expression },
    CreateFile { file: Expression },
    Input { value: Expression },
    Record { identifier: String, values: Vec<ObjectValue> },
    Class { identifier: String, values: Vec<ObjectValue> },
    ReturnStatement { value: Expression },
    CodeBlock (Vec<AstNode>),
    Expression(Expression),
    Eof,
}

#[derive(Debug, PartialEq, Display)]
pub enum Expression {
    StringLiteral(String),
    IntegerLiteral(i32),
    FloatLiteral(f64),
    ArrayLiteral(Vec<Box<Expression>>),
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

#[derive(Debug, PartialEq)]
pub enum Type {
    Str,
    Integer,
    FloatType,
    Boolean,
    Character,
    Array,
    Record,
    Class,
    ArrayOf { repetition: i32, array_type: Box<Type> },
    None,
    Identifier(String),

    Other(String),
}

#[derive(Debug, PartialEq)]
pub struct ElseIfStatement {
    condition: Expression,
    code_block: Vec<AstNode>,
}

#[derive(Debug, PartialEq)]
pub struct ElseStatement {
   code_block: Vec<AstNode>
}

#[derive(Debug, PartialEq)]
pub struct Parameter {
    param_type: Type,
    identifier: Expression,
}

#[derive(Debug, PartialEq)]
pub struct ObjectValue {
    value_type: Type,
    identifier: Token,
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
            Token::Open | Token::Close | Token::Create => self.handle_filing(),
            Token::Receive => self.handle_input(),
            Token::Record => self.handle_record(),
            Token::Class => self.handle_class(),
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

        while matches!(self.current_token, Token::NotEquals | Token::EqualsEquals | Token::Equals | Token::And ) {
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
            Token::StringLiteral(val) => Ok(Expression::StringLiteral(val.to_string())),
            Token::Int(val) => Ok(Expression::IntegerLiteral(*val)),
            Token::Float(val) => Ok(Expression::FloatLiteral(*val)),
            Token::Identifier(val) => Ok(Expression::Identifier(val.to_string())),
            Token::False => Ok(Expression::BooleanLiteral(false)),
            Token::True => Ok(Expression::BooleanLiteral(true)),
            Token::LeftBracket => {
                self.advance();
                let expr = self.expression()?;
                Ok(Expression::Grouping(Box::new(expr)))
            },
            Token::LeftSquareBracket => {
                self.advance(); // skip [
                let mut elemements = Vec::new();
                while self.current_token != Token::RightSquareBracket {
                    let element = self.expression().unwrap();
                    if self.current_token == Token::Comma {
                        self.advance(); // skip ,
                    }
                    elemements.push(Box::new(element));
                }

                Ok(Expression::ArrayLiteral(elemements))
            },

            _ => Err(format!("Failed primary parsing at token: {}", self.current_token)),

        };
        self.advance();
        expr
    }

    fn variable_declaration(&mut self) -> AstNode {
        self.advance();
        let mut var_type = Type::None;
        let identifier = self.expression().unwrap();
        self.advance();
        if self.current_token == Token::As {
            self.advance();
            var_type = self.handle_type(self.current_token.clone());
        }
        let value = self.expression().expect("Failed parsing expression in variable declaration");

        AstNode::VariableDeclaration { identifier, value, var_type }
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

    fn parse_block_without_advance(&mut self) -> Vec<AstNode> {
        let mut block = Vec::new();
        while !matches!(self.current_token, Token::End | Token::Else | Token::ElseIf ) {
            let node = self.next_token();
            block.push(node);
        }

        if self.current_token == Token::End {
            self.advance(); // skip END
        }

        block
    }

    fn parse_block(&mut self) -> Vec<AstNode>{
        self.advance();
        self.parse_block_without_advance()
    }


    fn if_statement(&mut self) -> AstNode {
        self.advance(); // skip if
        let condition = self.expression().unwrap();
        let code_block = self.parse_block();

        let mut elif_statements: Vec<ElseIfStatement> = Vec::new();
        let mut else_statement: Option<ElseStatement> = None;

        if self.current_token == Token::Else {
            (elif_statements, else_statement) = self.handle_else();
        }

        AstNode::IfStatement { condition, code_block, elif_statements, else_statement }
    }

    fn handle_else(&mut self) -> (Vec<ElseIfStatement>, Option<ElseStatement>) {
        println!("Running handle else");
        let mut elif_statements: Vec<ElseIfStatement> = Vec::new();
        let mut else_statement: Option<ElseStatement> = None;

        while self.current_token == Token::Else {
            println!("running while else: {}", self.current_token);
            self.advance(); // skip ELSE

            if self.current_token == Token::If {
                println!("running while else after skipping ELSE: {}", self.current_token);
                let elif_statement = self.else_if_statement();
                elif_statements.push(elif_statement);
            } else {
                let else_block = self.else_statement();
                else_statement = Some(ElseStatement { code_block: else_block });
            }

        }

        (elif_statements, else_statement)
    }

    fn else_if_statement(&mut self) -> ElseIfStatement {
        self.advance(); // skip IF
        let elif_condition = self.expression().unwrap();
        let elif_block = self.parse_block();

        ElseIfStatement { condition: elif_condition, code_block: elif_block }
    }

    fn else_statement(&mut self) -> Vec<AstNode> {
        let elif_block = self.parse_block_without_advance();

        elif_block
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
        let identifier = self.advance(); // moves up to identifier
        self.advance(); // move up to FROM
        self.advance(); // move past from

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

        let mut step: Expression = Expression::IntegerLiteral(1);
        if self.current_token == Token::Step {
            step = self.handle_step();
        }

        let code_block: Vec<AstNode> = self.parse_block();

        AstNode::ForFromLoop { id: identifier, lower, higher, step, code_block }

    }

    fn handle_step(&mut self) -> Expression {
        self.advance(); // skip step
        let step = self.expression().unwrap();
        step
    }

    fn handle_type(&mut self, input_type: Token ) -> Type {
        let out_type = match input_type {
            Token::Str => Type::Str,
            Token::Integer => Type::Integer,
            Token::FloatType => Type::FloatType,
            Token::Boolean => Type::Boolean,
            Token::Character => Type::Character,
            Token::Array => Type::Array,
            Token::Record => Type::Record,
            Token::Class => Type::Class,
            Token::Identifier(val) => Type::Identifier(val),

            input_type => Type::Other(input_type.to_string())
        };

        self.advance();

        if self.current_token == Token::Of {
            self.handle_array_of()
        } else {
            out_type
        }

    }

    fn handle_array_of(&mut self) -> Type {
        println!("running array of {}", self.current_token);
        let mut reps = 0;
        let mut final_type = Type::None;

        while self.current_token == Token::Of && self.current_token != Token::Eof {
            self.advance(); // skip past Of
            println!("running array of while loop {}", self.current_token);
            if self.current_token != Token::Array && self.current_token != Token::Of {
               final_type = self.handle_type(self.current_token.clone());
            } else if self.current_token == Token::Array {
                reps += 1;
                self.advance(); // skip ARRAY
            } else {
                panic!("Error handling array of: {}", self.current_token)
            }
        }

        Type::ArrayOf { repetition: reps, array_type: Box::new(final_type) }
    }

    fn subprogram(&mut self) -> (Token, Vec<Parameter>) {
        let identifier = self.advance(); // pass by FUNCTION or PROCEDURE toward the identifier
        self.expect(Token::LeftBracket); // move up to (
        self.advance(); // skip (

        let mut params: Vec<Parameter> = Vec::new();
        while self.current_token != Token::RightBracket {
            while self.current_token == Token::Comma {
                self.advance();
            }
            let param_type = self.handle_type(self.current_token.clone());
            let expr = self.expression().unwrap();
            let param = Parameter { param_type, identifier: expr };
            params.push(param);
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

    fn handle_filing(&mut self) -> AstNode {

        match self.current_token {
            Token::Open => {
                AstNode::OpenFile { file: self.handle_file() }
            },
            Token::Close => {
                AstNode::CloseFile { file: self.handle_file() }
            },
            Token::Create => {
                AstNode::CreateFile { file: self.handle_file() }
            },
            _ => panic!("Current token does not fit handle filing: {}", self.current_token),
        }

    }

    fn handle_file(&mut self) -> Expression {
        self.advance();
        let file = self.expression().unwrap();
        file
    }

    fn handle_input(&mut self) -> AstNode {
        self.advance(); // skip receive

        let value = self.expression().unwrap();
        self.advance(); // skip FROM
        self.advance(); // skip KEYBOARD

        AstNode::Input { value }
    }

    fn handle_object_declaration(&mut self) -> (String, Vec<ObjectValue>) {
        let identifier: String = self.advance().get_value().to_string();
        self.advance();
        self.expect(Token::LeftCurlyBracket); // skip to {
        self.advance();

        let mut values: Vec<ObjectValue> = Vec::new();
        while self.current_token != Token::RightCurlyBracket {
            while self.current_token == Token::Comma {
                self.advance();
            }
            let value_type = self.handle_type(self.current_token.clone());
            let value = self.current_token.clone();
            self.advance();
            let record_value = ObjectValue { value_type, identifier: value };
            values.push(record_value);
        }

        self.advance(); // skip }

        (identifier, values)

    }

    fn handle_record(&mut self) -> AstNode {
        let (identifier, values) = self.handle_object_declaration();
        AstNode::Record { identifier, values }
    }

    fn handle_class(&mut self) -> AstNode {
        let (identifier, values) = self.handle_object_declaration();
        AstNode::Class { identifier, values }
    }
}
