use std::char;
use std::str::Chars;
use std::iter::Peekable;
use strum_macros::Display;


#[derive(Debug, Display, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    Int(i32),
    Float(f64),
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
    Flt,
    Array,
    Of,

    // Operators
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    Equals,
    NotEquals,
    EqualsEquals,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    And,
    Or,
    Power,
    Bang,
    Comma,
    Period,

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
    True,
    False,
    //  Printing
    Send,
    Display,

}

// add impl to return keyword constants
impl Token {
    pub fn is_keyword(&mut self) -> bool {
        let keywords = Vec::from([

            Token::Declare,
            Token::Set,
            Token::Initially,
            Token::As,
            Token::To,
            Token::If,
            Token::ElseIf,
            Token::Else,
            Token::While,
            Token::Function ,
            Token::Procedure,
            Token::End,
            Token::Return,
            Token::Send,
            Token::Display,

        ]);

        if keywords.contains(self) {
            true
        } else {
            false
        }
    }

    pub fn get_value(&mut self) -> String {
        match self {
            Token::Identifier(val) => val.to_string(),
            Token::Int(val) => val.to_string(),
            Token::Float(val) => val.to_string(),
            Token::StringLiteral(val) => format!("\"{}\"", val.to_string()),
            _ => panic!("Cannot return value"),
        }
    }

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
                    "FLOAT" => Token::Flt,
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

                    "true" => Token::True,
                    "TRUE" => Token::True,
                    "false" => Token::False,
                    "FALSE" => Token::False,

                    _ => Token::Identifier(identifier),
                }
            },

            '0'..='9' => self.tokenise_number(),
            '\"' | '\'' => self.str_literal(),
            '+' | '-' | '–' | '*' | '/' | '%' | '=' | '<' | '>' | '&' | '|' | '^' | '!' | ',' | '.' => self.operator(self.current_char),
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
        let mut num: String = String::new();
        let mut is_float: bool = false;
        while self.current_char.is_digit(10) || self.current_char == '.' {
             if self.current_char == '.' {
                is_float = true;
            }
            num.push(self.current_char);
            self.advance();
        }

        if is_float {
            let out: f64 = num.parse().expect("Cant turn num into Float");
            Token::Float(out)
        } else if !is_float {
            let out: i32 = num.parse().expect("Cant turn into int");
            Token::Int(out)
        } else {
            panic!("Failed turning strint into int");
        }
    }

    fn operator(&mut self, op: char) -> Token {
        self.advance();
        match op {
            '+' => Token::Addition,
            '-' => Token::Subtraction,
            '–' => Token::Subtraction,
            '*' => Token::Multiplication,
            '/' => Token::Division,
            '%' => Token::Modulus,
            '=' => {
                if *self.chars.peek().unwrap() == '=' {
                    self.advance();
                    Token::EqualsEquals
                } else {
                    Token::Equals
                }
            }
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
            ',' => Token::Comma,
            '.' => Token::Period,
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



