// use std::collections::HashMap;
// use std::fmt::Write;
// use crate::lexer::Token;

// use crate::parser::{AstNode, Expression, Type};

// struct Context {
//     temp_counter: i32,
//     bool_temp: i32,
//     variables: HashMap<String, String>,
//     output: String,
// }

// impl Context {
//     pub fn new() -> Self {
//         Context {
//             temp_counter: 0,
//             bool_temp: 0,
//             variables: HashMap::new(),
//             output: String::new(),
//         }
//     }

//     pub fn get_temp(&mut self) -> String {
//         self.temp_counter += 1 ;
//         return format!("%{}", self.temp_counter)
//     }

//     fn get_bool_temp(&mut self, value: String) -> String {
//         self.bool_temp += 1;
//         self.emit(&format!("@bool{} = global i1 {}", self.bool_temp, value));
//         format!("@bool{}", self.bool_temp)
//     }

//     pub fn emit(&mut self, line: &str) {
//         writeln!(&mut self.output, "{}", line).unwrap();
//     }
// }

// pub struct Compiler {
//     input: AstNode,
//     context: Context,
// }

// impl Compiler {
//     pub fn new(input: AstNode) -> Self {
//         Compiler {
//             input,
//             context: Context::new(),
//         }
//     }

//     pub fn compile(&mut self) {
//         let input: Vec<_> = match self.input.clone() {
//             AstNode::Program(nodes) => nodes,
//             _ => panic!("Expected program node")
//         };

//         for node in input {
//             match *node {
//                 AstNode::VariableDeclaration { identifier, value, var_type } => self.compile_variable_declaration(identifier, value, var_type),
//                 _ => panic!("Cannot compile with node: {}", node)
//             }
//         }

//         println!("{}", self.context.output)
//     }

//     fn compile_variable_declaration(&mut self, identifier: Expression, value: Expression, var_type: Type) {
//         let llvm_type: &str = match var_type {
//             Type::Integer => "i32",
//             Type::FloatType => "double",
//             Type::Boolean => "i1",
//             _ => "*i8"

//         };

//         let id = self.compile_expression(identifier);
//         let val = self.compile_expression(value);

//         self.context.emit(&format!("%{} = alloca {}", id, llvm_type));
//         self.context.emit(&format!("store {} {}, {}* %{}", llvm_type, val, llvm_type, id));

//         self.context.variables.insert(id, val);
//     }

//     fn compile_expression(&mut self, expr: Expression) -> String {
//         match expr {
//             Expression::Identifier(identifier) => {
//                 identifier
//             },

//             Expression::StringLiteral(value) => {
//                 let arr_size = value.len()+1;
//                 self.context.temp_counter += 1;

//                 self.context.emit(&format!("@string{} = global [{} x i8] c'{}\00'", self.context.temp_counter, arr_size, value));

//                 String::from(format!("@string{}", self.context.temp_counter))
//             },
//             Expression::IntegerLiteral(value) => {
//                 format!("{}", value)
//             },
//             Expression::FloatLiteral(value) => {
//                 format!("{}", value)
//             },

//             Expression::BinaryOp(left, op, right) => {
//                 let operation = match op {
//                     Token::Addition => "add".to_string(),
//                     Token::Subtraction => "sub".to_string(),
//                     Token::Multiplication => "mul".to_string(),
//                     Token::Division => "sdiv".to_string(),
//                     _ => panic!("Operation cannot be compiled: {:?}", op)
//                 };

//                 let lft = self.compile_expression(*left);
//                 let rght = self.compile_expression(*right);

//                 let temp = self.context.get_temp();
//                 self.context.emit(&format!("{} = {} i32 {}, {}", temp, operation, lft, rght));

//                 temp
//             }

//             _ => panic!("Cannot compile that type of expr: {:?}", expr)

//         }

//     }


// }
