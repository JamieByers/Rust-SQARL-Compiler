use core::panic;
use std::result;
use inkwell::values::FloatValue;
use crate::lexer::Token;
use inkwell::{types::BasicTypeEnum, values::BasicValueEnum};
use inkwell::context::Context;
use inkwell::values::PointerValue;
use inkwell::builder::Builder;
use std::collections::HashMap;

use crate::parser::{AstNode, ElseIfStatement, ElseStatement, Expression, Type};


pub struct Variable<'ctx> {
    alloca: PointerValue<'ctx>,
    var_type: BasicTypeEnum<'ctx>,
    var_counter: i32,
    parsed_type: Type,
}

pub struct CodeGenerator<'ctx> {
    pub context: &'ctx Context,
    builder: Builder<'ctx>,
    pub module: inkwell::module::Module<'ctx>,
    pub variables: HashMap<String, Variable<'ctx>>,
    temp_counter: i32,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        CodeGenerator {
            context,
            builder,
            module,
            variables: HashMap::new(),
            temp_counter: 0,
        }
    }

    pub fn compile(&mut self, input: AstNode) {
        let input: Vec<Box<AstNode>> = match input.clone() {
            AstNode::Program(nodes) => nodes,
            _ => panic!("Expected program node")
        };

        self.setup_compiler(input.clone());
        self.create_main_func();

        for node in input {
            self.match_node(*node);
        }

        self.add_return_to_main();
        self.module.print_to_stderr();
    }

    fn match_node(&mut self, node: AstNode) {
        match node {
            AstNode::VariableDeclaration { identifier, value, var_type } => self.compile_variable_declaration(identifier, value, var_type),
            AstNode::VariableAssignment { identifier, value } => self.compile_variable_assignment(identifier, value),
            AstNode::SendToDisplay { value } => self.compile_print(value),
            // AstNode::IfStatement { condition, code_block, elif_statements, else_statement } => self.compile_if_statement(condition, code_block, elif_statements, else_statement),
            _ => panic!("Cannot compile with node: {}", node)
        }
    }

    // fn compile_block(&mut self, nodes: Vec<AstNode>) {
    //     for node in nodes {
    //         self.match_node(node);
    //     }
    // }


    // write sys code to run this automatically -
    // llc -filetype=obj output.ll -o output.o
    // clang output.o -o program
    // (./program)


    pub fn output(&mut self) {
        let output_file = "output.ll";
        let _ = self.module.print_to_file(output_file);
        println!("LLVM IR written to {}", output_file);
    }

    pub fn create_main_func(&mut self) {
        let ret_type = self.context.i32_type();
        let fn_type = ret_type.fn_type(&[], false);

        let function = self.module.add_function("main", fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);
    }


    fn setup_compiler(&mut self, input: Vec<Box<AstNode>>) {
        if input.iter().any(|node| matches!(&**node, AstNode::SendToDisplay { .. })) {
            let i32_type = self.context.ptr_type(inkwell::AddressSpace::default());
            let printf_type = i32_type.fn_type(&[self.context.ptr_type(inkwell::AddressSpace::default()).into()], true);

            let _printf = match self.module.get_function("printf") {
                Some(func) => func,
                _  => self.module.add_function("printf", printf_type, None),
            };
        }
    }

    fn get_temp(&mut self, identifier: &str) -> String {
        self.temp_counter += 1;
        format!("{}{}", identifier, self.temp_counter)
    }

    fn compile_print(&mut self, value: Expression) {
        let printf = self.module.get_function("printf").expect("printf function does not exist: print compilation error");

        match value {
            Expression::BinaryOp(_, _, _) => {
                let result = self.compile_expr(value);

                // Create appropriate format string based on the result type
                let format_str = match result {
                    BasicValueEnum::IntValue(_) => {
                        self.builder.build_global_string_ptr("%d\n", "format_str")
                    },
                    BasicValueEnum::FloatValue(_) => {
                        self.builder.build_global_string_ptr("%f\n", "format_str")
                    },
                    _ => panic!("Unsupported type for printing binary operation result")
                }.expect("Could not build format string");

                let _ = self.builder.build_call(
                    printf,
                    &[format_str.as_pointer_value().into(), result.into()],
                    "printf"
                );
            },

            Expression::Identifier(identifier) => {
               let variable = if let Some(variable) = self.variables.get(&identifier) {
                    variable
                } else {
                    panic!("Could not get variable in compile print")
                };

                println!("parsed type: {:?}", variable.parsed_type);
                let format_str = match variable.parsed_type {
                    Type::Strl(_) => self.builder.build_global_string_ptr("%s\n", "format_str"),
                    Type::Integer => self.builder.build_global_string_ptr("%d\n", "format_str"),
                    Type::FloatType => self.builder.build_global_string_ptr("%f\n", "format_str"),
                    _ => {
                        match variable.var_type {
                            _ => panic!("Unsupported type for printing")
                        }
                    }
                }.expect("could not build format string");

                let var_alloca = variable.alloca.clone();

                if matches!(variable.parsed_type, Type::Strl(_)) {
                    let _ = self.builder.build_call(
                        printf,
                        &[format_str.as_pointer_value().into(), var_alloca.into()],
                        "printf"
                    );
                } else {
                    let var_value = self.builder.build_load(
                        variable.var_type,
                        var_alloca,
                        "var_value"
                    ).expect("Couldnt get var value");

                    let _ = self.builder.build_call(
                        printf,
                        &[format_str.as_pointer_value().into(), var_value.into()],
                        "printf"
                    );
                }
            },
            _ => {
                let val = self.compile_expr(value);
                let _ = self.builder.build_call(printf, &[val.into()], "printf");
            }
        }
    }

    fn infer_type(&mut self, ty: BasicValueEnum) -> Type {
        match ty {
            BasicValueEnum::IntValue(..) => Type::Integer,
            BasicValueEnum::FloatValue(..) => Type::FloatType,
            _ => panic!("Cannot infer type {:?}", ty)
        }
    }

    fn compile_variable_declaration(&mut self, identifier: Expression, value: Expression, var_type: Type) {
        let variable_identifier = if let Expression::Identifier(value) = identifier {
            value
        } else {
            panic!("No variable identifier to compile")
        };

        let ty: BasicTypeEnum;
        let mut parsed_type = var_type.clone();
        let inital_value = self.compile_expr(value);

        if var_type == Type::BinaryOp || var_type == Type::Other("BinaryOp".to_string()){
            let inferred_type: Type = self.infer_type(inital_value);
            parsed_type = inferred_type.clone();
            ty = self.llvm_type_converter(inferred_type);
        } else {
            ty = self.llvm_type_converter(var_type.clone());
        }

        let alloca = self.builder.build_alloca(ty, &variable_identifier).expect("ERROR with alloca in var dec");
        self.builder.build_store(alloca, inital_value).expect("Error building store");

        let variable = Variable {
            alloca,
            var_type: ty,
            var_counter: 0,
            parsed_type,
        };
        self.variables.insert(variable_identifier, variable);
    }

    fn compile_variable_assignment(&mut self, identifier: Expression, value: Expression) {
        let variable_identifier = if let Expression::Identifier(id) = identifier {
            id
        } else {
            panic!("No variable identifier to compile")
        };

        let val = self.compile_expr(value.clone());

        let (parsed_type, variable_counter, variable_alloca) = if let Some(variable) = self.variables.get(&variable_identifier) {
            (variable.parsed_type.clone(), variable.var_counter.clone(), variable.alloca.clone())
        } else {
            panic!("Could not get parsed type or variable counter");
        };

        match parsed_type {
            Type::Strl(_) => {
                let counter = variable_counter + 1;
                let temp = self.get_temp(&variable_identifier);

                let string_value = match value {
                    Expression::StringLiteral(s) => s.clone(),
                    _ => panic!("String expected in variable assignment, found: {:?}", &value)
                };

                let ty = self.llvm_type_converter(Type::Strl(string_value.len() + 1));

                let alloca = self.builder.build_alloca(ty, &temp).expect("Couldnt create alloca in variable assignment");
                let _store = self.builder.build_store(alloca, val).expect("Couldnt build store in variable assignment");

                if let Some(variable) = self.variables.get_mut(&variable_identifier) {
                    variable.var_counter = counter;
                    variable.alloca = alloca;
                }
            },
            _ => {
                let _ = self.builder.build_store(variable_alloca, val.clone());
            }
        }

    }

    fn compile_expr(&mut self, expr: Expression) -> inkwell::values::BasicValueEnum<'ctx> {
        match expr {
            Expression::StringLiteral(s) => inkwell::values::BasicValueEnum::ArrayValue(self.context.const_string(s.as_bytes(), true)),
            Expression::IntegerLiteral(val) => self.context.i32_type().const_int(val as u64, false).into(),
            Expression::FloatLiteral(val) => self.context.f64_type().const_float(val.parse::<f64>().expect("cannot turn val into f64")).into(),
            Expression::Identifier(id) => self.build_load(id),
            Expression::BinaryOp(left, op, right) => {
                self.compile_binary_op(*left, op, *right)
            }
            _ => panic!("Cannot compile expression: {:?}", expr),
        }
    }

    fn compile_binary_op(&mut self, left: Expression, op: Token, right: Expression ) -> inkwell::values::BasicValueEnum<'ctx> {
        let lft = self.compile_expr(left);
        let rght = self.compile_expr(right);

        match (lft, rght) {
            (
                inkwell::values::BasicValueEnum::IntValue(lhs),
                inkwell::values::BasicValueEnum::IntValue(rhs),
            ) => {
                let name = self.get_temp("temp");
                let result = match op {
                    Token::Addition => self.builder.build_int_add(lhs, rhs, &name).expect("int add failed"),
                    Token::Subtraction => self.builder.build_int_sub(lhs, rhs, &name).expect("int sub failed"),
                    Token::Multiplication => self.builder.build_int_mul(lhs, rhs, &name).expect("int mul failed"),
                    Token::Division => self.builder.build_int_signed_div(lhs, rhs, &name).expect("int deiv failed"),
                    Token::Modulus => self.builder.build_int_signed_rem(lhs, rhs, &name).expect("int rem failed"),
                    Token::Equals | Token::NotEquals | Token::GreaterThan |
                    Token::GreaterThanOrEqual | Token::LessThan | Token::LessThanOrEqual => {
                        let predicate = match op {
                            Token::Equals => inkwell::IntPredicate::EQ,
                            Token::NotEquals => inkwell::IntPredicate::NE,
                            Token::GreaterThan => inkwell::IntPredicate::SGT,
                            Token::GreaterThanOrEqual => inkwell::IntPredicate::SGE,
                            Token::LessThan => inkwell::IntPredicate::SLT,
                            Token::LessThanOrEqual => inkwell::IntPredicate::SLE,
                            _ => unreachable!()
                        };

                        // First create the comparison
                        let compare_result = self.builder.build_int_compare(predicate, lhs, rhs, &name)
                            .expect("int compare failed");
                        compare_result

                    },
                    _ => panic!("Cannot complete addition with type: {:?}", op)
                };

                inkwell::values::BasicValueEnum::IntValue(result)
            },
            (
                inkwell::values::BasicValueEnum::FloatValue(lhs),
                inkwell::values::BasicValueEnum::FloatValue(rhs),
            ) => {
                let result = self.float_match(lhs, rhs, op);
                inkwell::values::BasicValueEnum::FloatValue(result)
            },
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::IntValue(rhs))
            | (BasicValueEnum::IntValue(rhs), BasicValueEnum::FloatValue(lhs)) => {
                    let (lhs, rhs) = self.convert_to_float(lhs.into(), rhs.into());
                    let result = self.float_match(lhs, rhs, op);

                    BasicValueEnum::FloatValue(result)
                },

            _ => panic!("Addition cannot support different types or type used for addition"),
        }

    }

    fn float_match(&mut self, lhs: FloatValue<'ctx>, rhs: FloatValue<'ctx>, op: Token) -> FloatValue<'ctx> {
        let name = self.get_temp("temp");
        match op {
            Token::Addition => self.builder.build_float_add(lhs, rhs, &name).expect("float add failed"),
            Token::Subtraction => self.builder.build_float_sub(lhs, rhs, &name).expect("float sub failed"),
            Token::Multiplication => self.builder.build_float_mul(lhs, rhs, &name).expect("float mul failed"),
            Token::Division => self.builder.build_float_div(lhs, rhs, &name).expect("float deiv failed"),
            Token::Modulus => self.builder.build_float_rem(lhs, rhs, &name).expect("float rem failed"),
            Token::Equals | Token::NotEquals | Token::GreaterThan |
            Token::GreaterThanOrEqual | Token::LessThan | Token::LessThanOrEqual => {
                let predicate = match op {
                    Token::Equals => inkwell::FloatPredicate::OEQ,
                    Token::NotEquals => inkwell::FloatPredicate::ONE,
                    Token::GreaterThan => inkwell::FloatPredicate::OGT,
                    Token::GreaterThanOrEqual => inkwell::FloatPredicate::OGE,
                    Token::LessThan => inkwell::FloatPredicate::OLT,
                    Token::LessThanOrEqual => inkwell::FloatPredicate::OLE,
                    _ => unreachable!()
                };

                // First create the comparison
                let compare_result = self.builder.build_float_compare(predicate, lhs, rhs, &name)
                    .expect("float compare failed");

                // Convert the boolean result (IntValue) to a float
                self.builder.build_unsigned_int_to_float(
                    compare_result,
                    self.context.f64_type(),
                    "bool_to_float"
                ).expect("Failed to convert comparison result to float")
            },
            _ => panic!("Cannot complete operation with type: {:?}", op),
        }
    }

    fn convert_to_float(&self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> (FloatValue<'ctx>, FloatValue<'ctx>) {
        let lhs_float = match lhs {
            BasicValueEnum::IntValue(int) => {
                self.builder
                    .build_signed_int_to_float(int, self.context.f64_type(), "int_to_float_lhs")
                    .expect("Failed to convert LHS integer to float")
            },
            BasicValueEnum::FloatValue(float) => float,
            _ => panic!("Unsupported type for float conversion: {:?}", lhs),
        };

        let rhs_float = match rhs {
            BasicValueEnum::IntValue(int) => {
                self.builder
                    .build_signed_int_to_float(int, self.context.f64_type(), "int_to_float_rhs")
                    .expect("Failed to convert RHS integer to float")
            },
            BasicValueEnum::FloatValue(float) => float,
            _ => panic!("Unsupported type for float conversion: {:?}", rhs),
        };

        (lhs_float, rhs_float)
    }

    fn build_load(&mut self, id: String) -> inkwell::values::BasicValueEnum<'ctx> {
        self.temp_counter += 1;
        let temp = format!("temp{}_load", self.temp_counter);
        let variable = self.variables.get(&id).expect("Could not get either alloca or type in build load");
        let value = self.builder.build_load(variable.var_type, variable.alloca, &temp).expect("Couldnt build load").into();
        value
    }

    fn llvm_type_converter(&mut self, t: Type) -> BasicTypeEnum<'ctx> {
        match t {
            Type::Strl(l) => {
                let i8_type = self.context.i8_type();
                let array_type = i8_type.array_type(l.try_into().unwrap());
                array_type.into()
            }
            Type::Integer => self.context.i32_type().into(),
            Type::FloatType => self.context.f64_type().into(),
            Type::Boolean => self.context.bool_type().into(),
            Type::Character => self.context.i8_type().into(),

            _ => panic!("Typing is not compatable with type: {:?}", t)
        }
    }

    fn add_return_to_main(&mut self) {
        let function = self.module.get_function("main").expect("Could not get main function in ret function");
        let entry = function.get_first_basic_block().expect("No entry block found");

        self.builder.position_at_end(entry);

        let _ = self.builder.build_return(Some(&self.context.i32_type().const_int(0, false)));

    }

    fn compile_if_statement(&mut self, condition: Expression, code_block: Vec<AstNode>, elif_statements: Vec<ElseIfStatement>, else_statement: ElseStatement) {
        let cond = self.compile_expr(condition);
    }
}
