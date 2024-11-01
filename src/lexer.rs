use std::char;
use std::str::Chars;
use std::iter::Peekable;
use strum_macros::Display;


#[derive(Debug, Display, PartialEq)]
pub enum Token {
    Identifier(String),
    Int(String),
    StringLiteral(String),
    Eof,

    // SQARL KEYWORDS

    // Variables
    Declare,
    Set,
    Initially ,
    As,
    To,

    // Types
    Str,
    Integer,
    Float,
    Array,
    Of,

    // Operators
    Addition,
    Subtraction,
    Multiply,
    Division,
    Modulus,
    Equals,
    NotEquals,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    And,
    Or,
    Power,
    Bang,

    // Brackets
    LeftBracket,
    RightBracket,
    LeftSquareBracket,
    RightSquareBracket,
    LeftCurlyBracket,
    RightCurlyBracket,

    // Keyword
    If,
    ElseIf,
    Else,
    While,
    Function ,
    Procedure,
    End,
    Return,
    //  Printing
    Send,
    Display,

}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    current_char: char,
}

impl<'a> Lexer<'a> {
    pub fn new(string: &'a String) -> Self {
        let mut chrs = string.chars().peekable();
        let char = chrs.next();

        let result = if let Some(c) = char {
            c
        } else {
            '\0'
        };

        Lexer {
            chars: chrs,
            current_char: result,
        }
    }

    fn advance(&mut self) -> char {
        self.current_char = self.chars.next().unwrap_or('\0');
        self.current_char
    }

    fn skip_whitespace(&mut self) {
        while self.current_char.is_whitespace() {
            self.advance();
        }
    }

    pub fn lex(&mut self) {
        let mut tokens = Vec::new();
        while self.current_char != '\0' {
            let token = self.next_token();
            // println!("{}, {}", &token, self.current_char);
            tokens.push(token);
        }

        println!("{:?}", tokens);
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        match self.current_char {
            'a'..='z' | 'A'..='Z' | ' ' => {
                let identifier = self.tokenise_identifier();
                match identifier.as_str() {
                    // Variables
                    "DECLARE" => Token::Declare,
                    "INITIALLY" => Token::Initially,
                    "SET" => Token::Set,
                    "TO" => Token::To,
                    "AS" => Token::As,
                    // Types
                    "STRING" => Token::Str,
                    "INTEGER" => Token::Integer,
                    "FLOAT" => Token::Float,
                    "ARRAY" => Token::Array,
                    "OF" => Token::Of,
                    // Keyword
                    "If" => Token::If,
                    "ElseIf" => Token::ElseIf ,
                    "Else" => Token::Else,
                    "While" => Token::While,
                    "Function " => Token::Function,
                    "Procedure" => Token::Procedure,
                    "End" => Token::End,
                    "Return" => Token::Return,
                    "Send" => Token::Send,
                    "Display" => Token::Display,

                    _ => Token::Identifier(identifier),
                }
            },

            '0'..='9' => self.tokenise_number(),
            '\"' | '\'' => self.str_literal(),
            '+' | '-' | '–' | '*' | '/' | '%' | '=' | '<' | '>' | '&' | '|' | '^' | '!' => self.operator(self.current_char),
            '(' | ')' | '[' | ']' | '{' | '}' => self.brackets(self.current_char),
            '\0' => Token::Eof,
            _ => panic!("Token not recognised: '{}' Peek: {}", self.current_char, self.chars.peek().unwrap())
        }
    }

    fn str_literal(&mut self) -> Token {
        let mut str = String::new();
        let wrapper = self.current_char;
        self.advance(); // skip " or '
        while self.current_char != wrapper {
            str.push(self.current_char);
            self.advance();
        }
        self.advance(); // skip " or '

        Token::StringLiteral(str)
    }

    fn tokenise_identifier(&mut self) -> String {
        let mut str = String::new();

        while (self.current_char.is_alphabetic() || self.current_char == '_') && self.current_char != ' '  {
            str.push(self.current_char);
            self.advance();
        }

        str
    }

    fn tokenise_number(&mut self) -> Token {
        let mut num = String::new();
        while self.current_char.is_digit(10) {
            num.push(self.current_char);
            self.advance();
        }

        Token::Int(num)
    }

    fn operator(&mut self, op: char) -> Token {
        self.advance();
        match op {
            '+' => Token::Addition,
            '-' => Token::Subtraction,
            '–' => Token::Subtraction,
            '*' => Token::Multiply,
            '/' => Token::Division,
            '%' => Token::Modulus,
            '=' => Token::Equals,
            '<' => {
                if *self.chars.peek().unwrap() == '=' {
                    self.advance();
                    Token::GreaterThanOrEqual
                } else {
                    Token::GreaterThan
                }
            },
            '>' => {
                if *self.chars.peek().unwrap() == '=' {
                    self.advance();
                    Token::LessThanOrEqual
                } else {
                    Token::LessThan
                }
            },
            '&' => Token::And,
            '|' => Token::Or,
            '^' => Token::Power,
            '!' => {
                if *self.chars.peek().unwrap() == '=' {
                    self.advance();
                    Token::NotEquals
                } else {
                    Token::Bang
                }
            },
            _ => panic!("Operator not accepted"), // Catch-all case for non-operators
        }
    }

    fn brackets(&mut self, bracket: char) -> Token {
        self.advance();
        match bracket {
            '(' => Token::LeftBracket,
            ')' => Token::RightBracket,
            '[' => Token::LeftSquareBracket,
            ']' => Token::RightSquareBracket,
            '{' => Token::LeftCurlyBracket,
            '}' => Token::RightCurlyBracket,
            _ => panic!("Bracket not covered")
        }
    }

}


