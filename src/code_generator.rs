use inkwell::types::BasicType;
use inkwell::AddressSpace;
use inkwell::basic_block::BasicBlock;
use inkwell::values::{BasicMetadataValueEnum, BasicValue, FloatValue, FunctionValue};
use crate::lexer::Token;
use inkwell::{types::BasicTypeEnum, values::BasicValueEnum};
use inkwell::context::Context;
use inkwell::values::PointerValue;
use inkwell::builder::Builder;
use std::collections::HashMap;

use crate::parser::{AstNode, ElseIfStatement, ElseStatement, Expression, Parameter, Type};

#[derive(Debug)]
pub struct Variable<'ctx> {
    alloca: PointerValue<'ctx>,
    var_type: BasicTypeEnum<'ctx>,
    var_counter: i32,
    parsed_type: Type,
    value: inkwell::values::BasicValueEnum<'ctx>,
}


pub struct Temps {
    temp_counter: i32,
    if_temp_counter: i32,
    while_loop_counter: i32,
}

impl Temps {
    fn new() -> Self {
        Temps {
            temp_counter: 0,
            if_temp_counter: 0,
            while_loop_counter: 0,
        }
    }
}

pub struct CodeGenerator<'ctx> {
    pub context: &'ctx Context,
    builder: Builder<'ctx>,
    pub module: inkwell::module::Module<'ctx>,
    pub variables: HashMap<String, Variable<'ctx>>,
    pub functions: HashMap<String, FunctionValue<'ctx>>,
    temps: Temps,
    current_func: Option<FunctionValue<'ctx>>,
    current_block: Option<BasicBlock<'ctx>>,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let temps = Temps::new();
        let variables = HashMap::new();
        let functions = HashMap::new();
        let current_func = None;
        let current_block = None;

        CodeGenerator {
            context,
            builder,
            module,
            variables,
            functions,
            temps,
            current_func,
            current_block,
        }

    }

    pub fn compile(&mut self, input: AstNode) {
        let input: Vec<Box<AstNode>> = match input.clone() {
            AstNode::Program(nodes) => nodes,
            _ => panic!("Expected program node")
        };

        self.create_print_func();
        self.create_main_func();

        for node in input {
            self.match_node(*node);
        }

        self.add_return_to_main();
    }

    fn match_node(&mut self, node: AstNode) {
        match node {
            AstNode::VariableDeclaration { identifier, value, var_type } => self.compile_variable_declaration(identifier, value, var_type),
            AstNode::VariableAssignment { identifier, value } => self.compile_variable_assignment(identifier, value),
            AstNode::SendToDisplay { value } => self.compile_print(value),
            AstNode::IfStatement { condition, code_block, elif_statements, else_statement } => self.compile_if_statement(condition, code_block, elif_statements, Some(else_statement).expect("No else_statement")),
            AstNode::WhileStatement { condition, code_block } => self.compile_while_loop( condition, code_block ),
            AstNode::FunctionDeclaration { identifier, params, code_block, return_type } => self.compile_function_declaration( identifier, params, code_block, return_type ),
            AstNode::ProcedureDeclaration { identifier, params, code_block } => self.compile_procedure_declaration(identifier, params, code_block),
            AstNode::FunctionCall { identifier, parameters, return_type } => self.compile_function_call( identifier, parameters, return_type ),
            AstNode::ReturnStatement { value }  => self.compile_return_statement( value ),
            _ => panic!("Cannot compile with node: {}", node)
        }
    }

    fn compile_block(&mut self, nodes: Vec<AstNode>) {
        for node in nodes {
            self.match_node(node);
        }
    }

    fn match_format_string(&mut self, ty: Type) -> &str {
        match ty {
            Type::Character => "%c",
            Type::Str | Type::Strl(_) => "%s",
            Type::Integer => "%i",
            Type::FloatType => "%f",
            _ => unreachable!()
        }
    }

    // write sys code to run this automatically -
    // llc -filetype=obj output.ll -o output.o
    // clang output.o -o program
    // (./program)


    pub fn output(&mut self, file_name: &str) -> String {
        std::fs::create_dir_all("compiled_tests").expect("Failed to create compiled_tests directory");

        let output_file;
        if file_name.contains("test") {
            output_file = format!("compiled_tests/{}.ll", file_name);
        } else {
            output_file = format!("{}.ll", file_name);
        }

        let _ = self.module.print_to_file(output_file.clone());
        println!("LLVM IR written to {}", output_file);

        self.module.print_to_stderr();
        self.module.print_to_string().to_string()
    }

    pub fn create_main_func(&mut self) {
        let ret_type = self.context.i32_type();
        let fn_type = ret_type.fn_type(&[], false);

        let function = self.module.add_function("main", fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.current_block = Some(entry);
        self.current_func = Some(function);

        self.builder.position_at_end(entry);
    }

    fn create_print_func(&mut self) {
        // let i32_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let i32_type = self.context.i32_type();
        let printf_type = i32_type.fn_type(&[self.context.ptr_type(inkwell::AddressSpace::default()).into()], true);

        let _printf = match self.module.get_function("printf") {
            Some(func) => func,
            _  => self.module.add_function("printf", printf_type, None),
        };
    }

    fn get_temp(&mut self, identifier: &str) -> String {
        self.temps.temp_counter += 1;
        format!("{}{}", identifier, self.temps.temp_counter)
    }

    fn get_if_temp(&mut self, identifier: &str) -> String {
        self.temps.if_temp_counter += 1;
        format!("{}{}", identifier, self.temps.if_temp_counter)
    }

    fn get_while_temp(&mut self, identifier: &str) -> String {
        self.temps.while_loop_counter += 1;
        format!("{}{}", identifier, self.temps.while_loop_counter)
    }

    fn compile_print(&mut self, value: Expression) {
        let printf = self.module.get_function("printf").unwrap_or_else(|| {
            self.create_print_func();
            self.module.get_function("printf").expect("printf function does not exist: print compilation error")
        });


        match value {
            Expression::StringLiteral(s) => {
                let str_format = self.builder.build_global_string_ptr("%s\n", "format_str");
                let str_temp = self.get_temp("display_str_temp");
                let var_type = Type::Strl(s.len()+1);
                self.compile_variable_declaration(
                    Expression::Identifier(str_temp.clone()),
                    Expression::StringLiteral(s.clone()),
                    var_type
                );

                let temp_var = self.variables.get(&str_temp).expect("Failed to find temp variable");

                let _ = self.builder.build_call(
                    printf,
                    &[str_format.expect("As pointer value failed?").as_pointer_value().into(),
                      temp_var.alloca.into()],
                    "printf"
                );
            },

            Expression::IntegerLiteral(v) => {
                let str_format = self.builder.build_global_string_ptr("%d\n", "format_str");
                let val = BasicValueEnum::IntValue(self.context.i32_type().const_int(v as u64, false));

                let _ = self.builder.build_call(
                    printf,
                    &[str_format.expect("As pointer value failed?").as_pointer_value().into(), val.into()],
                    "printf"
                );

            },

            Expression::FloatLiteral(v) => {
                let str_format = self.builder.build_global_string_ptr("%f\n", "format_str").unwrap();
                let v: f64 = v.parse().unwrap();
                let val = BasicValueEnum::FloatValue(self.context.f64_type().const_float(v));

                let _ = self.builder.build_call(
                    printf,
                    &[str_format.as_pointer_value().into(), val.into_float_value().into()],
                    "printf"
                );

            },

            Expression::BinaryOp(_, _, _) => {
                let result = self.compile_expr(value);

                println!("RESULT IN PRINT : {:?}", result);

                // Create appropriate format string based on the result type
                match result {
                    BasicValueEnum::ArrayValue(av) => {
                        let temp = self.get_temp("temp");
                        let alloca = self.builder.build_alloca(av.get_type(), &temp).unwrap();
                        self.builder.build_store(alloca, av).unwrap();

                        let format_str = self.builder.build_global_string_ptr("%s\n", "format_str").unwrap();

                        let _ = self.builder.build_call(
                            printf,
                            &[format_str.as_pointer_value().into(), alloca.into()],
                            "printf"
                        );
                    }
                    BasicValueEnum::IntValue(_) => {
                        let format_str = self.builder.build_global_string_ptr("%d\n", "format_str").expect("ERROR with format str in binary op");
                        let _ = self.builder.build_call(
                            printf,
                            &[format_str.as_pointer_value().into(), result.into()],
                            "printf"
                        );
                    },
                    BasicValueEnum::FloatValue(_) => {
                        let format_str = self.builder.build_global_string_ptr("%f\n", "format_str").expect("ERROR with format str in binary op");
                        let _ = self.builder.build_call(
                            printf,
                            &[format_str.as_pointer_value().into(), result.into()],
                            "printf"
                        );
                    },
                    BasicValueEnum::PointerValue(pv) => {
                        let temp = self.get_temp("load_ptr");
                        let alloca = self.builder.build_alloca(pv.get_type(), &temp).unwrap();
                        let _store = self.builder.build_store(alloca, pv);
                        let format_str = self.builder.build_global_string_ptr("%s\n", "format_str").expect("ERROR with format str in binary op");

                        let _ = self.builder.build_call(
                            printf,
                            &[format_str.as_pointer_value().into(), alloca.into()],
                              "printf"
                        );

                    },
                    _ => panic!("Unsupported type for printing binary operation result")
                }

            },

            Expression::Identifier(identifier) => {
               let variable = if let Some(variable) = self.variables.get(&identifier) {
                    variable
                } else {
                    panic!("Could not get variable in compile print")
                };


                let format_str = match variable.parsed_type {
                    Type::Str | Type::Strl(_) => self.builder.build_global_string_ptr("%s\n", "format_str"),
                    Type::Integer | Type::Boolean => self.builder.build_global_string_ptr("%d\n", "format_str"),
                    Type::FloatType => self.builder.build_global_string_ptr("%f\n", "format_str"),
                    _ =>  panic!("Unsupported type for printing")

                }.expect("could not build format string");

                let var_alloca = variable.alloca.clone();

                if matches!(variable.parsed_type, Type::Str | Type::Strl(_)) {
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
            BasicValueEnum::ArrayValue(array) => {
                // For string literals, which are represented as arrays of i8
                if array.get_type().get_element_type().is_int_type() {
                    let length = array.get_type().len();
                    Type::Strl(length as usize)
                } else {
                    panic!("Unsupported array type for inference")
                }
            },
            _ => panic!("Cannot infer type {:?}", ty)
        }
    }

    fn compile_variable_declaration(&mut self, identifier: Expression, value: Expression, var_type: Type) {
        let variable_identifier = if let Expression::Identifier(value) = identifier {
            value
        } else {
            panic!("No variable identifier to compile")
        };

        let mut parsed_type = var_type.clone();
        let inital_value = self.compile_expr(value.clone());

        let binary_op_string = String::from("BinaryOp");
        let function_call_string = String::from("FunctionCall");
        let ty: BasicTypeEnum = match var_type {
            Type::BinaryOp => {
                let inferred_type: Type = self.infer_type(inital_value);
                parsed_type = inferred_type.clone();
                self.llvm_type_converter(inferred_type)
            },
            Type::Other(ref s) if s == &binary_op_string => {
                let inferred_type: Type = self.infer_type(inital_value);
                parsed_type = inferred_type.clone();
                self.llvm_type_converter(inferred_type)
            },
            Type::Other(ref s) if s == &function_call_string => {
                let return_type = match value {
                   Expression::FunctionCall { name: _ , parameters: _, return_type } => {
                        return_type.clone()
                    },
                    _ => panic!("Expected a function expression")
                };
                self.llvm_type_converter(return_type)

            },
            _ => {
                self.llvm_type_converter(var_type)
            }
        };

        let alloca = self.builder.build_alloca(ty, &variable_identifier).expect("ERROR with alloca in var dec");
        self.builder.build_store(alloca, inital_value).expect("Error building store");

        let variable = Variable {
            alloca,
            var_type: ty,
            var_counter: 0,
            parsed_type,
            value: inital_value,
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
        println!("VAL {:?}", val);
        println!("VAL TyPE {:?}", val.get_type());

        let (_parsed_type, variable_counter, variable_alloca) = if let Some(variable) = self.variables.get(&variable_identifier) {
            (variable.parsed_type.clone(), variable.var_counter.clone(), variable.alloca.clone())
        } else {
            panic!("Could not get parsed type or variable counter");
        };

        match val.get_type() {
            BasicTypeEnum::ArrayType(at) => {
                let counter = variable_counter + 1;
                let temp = self.get_temp(&variable_identifier);
                let alloca = self.builder.build_alloca(at, &temp).expect("Couldnt build alloca in variable assignment");
                let _store = self.builder.build_store(alloca, val).expect("Couldnt build store in variable assignment");

                if let Some(variable) = self.variables.get_mut(&variable_identifier) {
                    variable.var_counter = counter;
                    variable.alloca = alloca;
                    variable.value = val;
                }
            },
            _ => {
                let _ = self.builder.build_store(variable_alloca, val.clone());
            }
        };
    }

    fn compile_expr(&mut self, expr: Expression) -> inkwell::values::BasicValueEnum<'ctx> {
        match expr {
            Expression::StringLiteral(s) => {
                let ptr = self.context.const_string(s.as_bytes(), true);

                BasicValueEnum::ArrayValue(ptr)
            },
            Expression::IntegerLiteral(val) => self.context.i32_type().const_int(val as u64, false).into(),
            Expression::FloatLiteral(val) => self.context.f64_type().const_float(val.parse::<f64>().expect("cannot turn val into f64")).into(),
            Expression::BooleanLiteral(b) => {
                if b == true {
                    self.context.bool_type().const_int(1, false).into()
                } else {
                    self.context.bool_type().const_int(0, false).into()
                }
            }
            Expression::Identifier(id) => self.build_load(id),
            Expression::BinaryOp(left, op, right) => {
                self.compile_binary_op(*left, op, *right)
            },
            Expression::FunctionCall { name, parameters, .. } => {
                let func_name = match *name {
                   Expression::Identifier(identifier) => identifier,
                    _ => panic!("Expected identifier"),
                };

                let temp_name = self.get_temp("func_result");

                let function = self.functions.get(&func_name.clone()).unwrap().clone();

                let mut compiled_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
                for param in parameters {
                    let compiled_param = self.compile_expr(param);
                    compiled_args.push(compiled_param.into());
                }

                let call_result = self.builder.build_call(function, &compiled_args, &temp_name).expect("Build call failed").try_as_basic_value().left().expect("Function call did not return a value");
                call_result
            },
            Expression::ArrayLiteral(values, array_type, array_len) => {

                let mut compiled_values = Vec::new();
                for value in values {
                    let compiled_value = self.compile_expr(*value);
                    compiled_values.push(compiled_value);
                }

                let arr_type_basic = match self.llvm_type_converter(array_type) {
                    BasicTypeEnum::IntType(int) => int.vec_type(array_len as u32),
                    BasicTypeEnum::FloatType(float) => float.vec_type(array_len as u32),
                    BasicTypeEnum::PointerType(ptr) => ptr.vec_type(array_len as u32),
                    _ => unreachable!(),
                };

                let mut expr_value = arr_type_basic.const_zero().as_basic_value_enum();
                for (index, value) in compiled_values.iter().enumerate() {
                    let index = self.context.i8_type().const_int(index as u64, false);
                    expr_value = expr_value.into_vector_value().const_insert_element(index, *value);
                }

                expr_value

            },

            _ => panic!("Cannot compile expression: {:?}", expr),
        }
    }

    fn compile_binary_op(&mut self, left: Expression, op: Token, right: Expression ) -> inkwell::values::BasicValueEnum<'ctx> {
        let lft = self.compile_expr(left.clone());
        let rght = self.compile_expr(right.clone());

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

            (BasicValueEnum::ArrayValue(lhsav), BasicValueEnum::ArrayValue(rhsav)) => {

                let lhs_len = lhsav.get_type().len() - 1;
                let rhs_len = rhsav.get_type().len() - 1;
                let buffer_size = lhs_len + rhs_len + 1;
                let buffer_type = self.context.i8_type().array_type(buffer_size);
                let buffer = self.builder.build_alloca(buffer_type, "buffer").unwrap();

                println!("BUFFER SIZE {} ", buffer_size);

                let lhs_array = self.builder.build_alloca(lhsav.get_type(), "lhs_array").unwrap();
                self.builder.build_store(lhs_array, lhsav).unwrap();

                let rhs_array = self.builder.build_alloca(rhsav.get_type(), "rhs_array").unwrap();
                self.builder.build_store(rhs_array, rhsav).unwrap();

                let lhs_ptr = unsafe {
                    self.builder.build_gep(
                        lhsav.get_type(),
                        lhs_array,
                        &[self.context.i32_type().const_int(0, false), self.context.i32_type().const_int(0, false)],
                        "lhs_ptr"
                    ).unwrap()
                };

                let rhs_ptr = unsafe {
                    self.builder.build_gep(
                        rhsav.get_type(),
                        rhs_array,
                        &[self.context.i32_type().const_int(0, false), self.context.i32_type().const_int(0, false)],
                        "rhs_ptr"
                    ).unwrap()
                };

                let sprintf = match self.module.get_function("sprintf") {
                    Some(func) => func,
                    None => {
                        let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
                        let sprintf_type = self.context.i32_type().fn_type(
                            &[i8_ptr_type.into(), i8_ptr_type.into()],
                            true, // varargs
                        );
                        self.module.add_function("sprintf", sprintf_type, None)
                    }
                };

                let format_global = match self.module.get_global("concat_format") {
                    Some(global) => global,
                    None => {
                        let format_str = self.context.const_string("%s%s".as_bytes(), true);
                        let global = self.module.add_global(
                            format_str.get_type(),
                            None,
                            "concat_format"
                        );
                        global.set_initializer(&format_str);
                        global
                    }
                };

                let _sprintf_call = self.builder.build_call(
                    sprintf,
                    &[
                        buffer.into(),
                        format_global.as_pointer_value().into(),
                        lhs_ptr.into(),
                        rhs_ptr.into()
                    ],
                    "sprintf_call"
                ).unwrap();

                let loaded_buffer = self.builder.build_load(buffer_type, buffer, "loaded_buffer").unwrap();

                loaded_buffer.into()

            },
            _ => {
                match (left.clone(), right.clone()) {
                    (Expression::StringLiteral(lhss), Expression::StringLiteral(rhss)) => {
                        match op {
                            Token::Addition => {
                                let combined_string = lhss + &rhss;
                                inkwell::values::BasicValueEnum::ArrayValue(self.context.const_string(combined_string.as_bytes(), true))
                            }
                            _ => panic!("Cannot use operator on two strings: {:?}", op),
                        }
                    },
                    _ => panic!("Cannot compile binary op")
                }
            }
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

                let compare_result = self.builder.build_float_compare(predicate, lhs, rhs, &name)
                    .expect("float compare failed");

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
        self.temps.temp_counter += 1;
        let temp = format!("temp{}_load", self.temps.temp_counter);
        let variable = self.variables.get(&id).expect("Could not get either alloca or type in build load");
        let value = self.builder.build_load(variable.var_type, variable.alloca, &temp).expect("Couldnt build load").into();
        value
    }

    fn llvm_type_converter(&mut self, t: Type) -> BasicTypeEnum<'ctx> {
        match t {
            Type::Str => {
                let i8_type = self.context.i8_type();
                let array_type = i8_type.array_type(256);
                array_type.into()

            },
            Type::Strl(l) => {
                let i8_type = self.context.i8_type();
                let array_type = i8_type.array_type(l.try_into().unwrap());
                array_type.into()
            }
            Type::Integer => self.context.i32_type().into(),
            Type::FloatType => self.context.f64_type().into(),
            Type::Boolean => self.context.bool_type().into(),
            Type::Character => self.context.i8_type().into(),
            Type::Arrayl(array_len, ty) => {
                let arr_type_basic = match self.llvm_type_converter(*ty) {
                    BasicTypeEnum::IntType(int) => int.vec_type(array_len as u32),
                    BasicTypeEnum::PointerType(ptr) => ptr.vec_type(array_len as u32),
                    _ => unreachable!(),
                };
                arr_type_basic.as_basic_type_enum()
            }

            _ => panic!("Typing is not compatable with type: {:?}", t)
        }
    }

    fn add_return_to_main(&mut self) {
        let _ = self.builder.build_return(Some(&self.context.i32_type().const_int(0, false)));
    }


    fn move_to_block(&mut self, entry: BasicBlock<'ctx>) {
        self.current_block = Some(entry);
        self.builder.position_at_end(entry);
    }

    fn compile_if_statement(&mut self, condition: Expression, code_block: Vec<AstNode>, elif_statements: Vec<ElseIfStatement>, else_statement: Option<ElseStatement>) {
        let function = self.current_func.expect("Could not get function from self.current_func");

        let merge_bb = self.context.append_basic_block(function, &self.get_if_temp("merge"));
        let if_then_bb = self.context.append_basic_block(function, &self.get_if_temp("if_then"));

        let mut elif_cond_blocks = vec![];
        let mut elif_body_blocks = vec![];
        for i in 0..elif_statements.len() {
            elif_cond_blocks.push(self.context.append_basic_block(function, &format!("elif_cond{}", i)));
            elif_body_blocks.push(self.context.append_basic_block(function, &format!("elif_body{}", i)));
        }

        let else_bb = if else_statement.is_some() {
            Some(self.context.append_basic_block(function, &self.get_if_temp("else")))
        } else {
            None
        };

        let cmp = self.compile_expr(condition);
        let comparison = match cmp {
            BasicValueEnum::IntValue(v) => v,
            _ => panic!("Condition should evaluate to an integer")
        };

        let next_block = if !elif_cond_blocks.is_empty() {
            elif_cond_blocks[0]
        } else if let Some(else_block) = else_bb {
            else_block
        } else {
            merge_bb
        };

        self.builder.build_conditional_branch(comparison, if_then_bb, next_block)
            .expect("Failed to build conditional branch");

        self.move_to_block(if_then_bb);
        self.compile_block(code_block);
        self.builder.build_unconditional_branch(merge_bb)
            .expect("Failed to build unconditional branch");

        for (i, elif_statement) in elif_statements.iter().enumerate() {
            self.move_to_block(elif_cond_blocks[i]);

            let elif_cmp = self.compile_expr(elif_statement.clone().condition);
            let elif_comparison = match elif_cmp {
                BasicValueEnum::IntValue(v) => v,
                _ => panic!("Elif condition should evaluate to an integer")
            };

            let next_block = if i < elif_cond_blocks.len() - 1 {
                elif_cond_blocks[i + 1]
            } else if let Some(else_block) = else_bb {
                else_block
            } else {
                merge_bb
            };

            self.builder.build_conditional_branch(elif_comparison, elif_body_blocks[i], next_block)
                .expect("Failed to build elif conditional branch");

            self.move_to_block(elif_body_blocks[i]);
            self.compile_block(elif_statement.clone().code_block);
            self.builder.build_unconditional_branch(merge_bb)
                .expect("Failed to build elif body unconditional branch");
        }

        if let Some(else_block) = else_bb {
            if let Some(else_stmt) = else_statement {
                self.move_to_block(else_block);
                self.compile_block(else_stmt.code_block);
                self.builder.build_unconditional_branch(merge_bb)
                    .expect("Failed to build else unconditional branch");
            }
        }

        self.move_to_block(merge_bb);
    }

    fn compile_while_loop(&mut self, condition: Expression, code_block: Vec<AstNode>) {
        let function = self.current_func.expect("Couldnt get current func");

        let name = self.get_while_temp("loop");
        let loop_branch = self.context.append_basic_block(function, &name);
        let _ = self.builder.build_unconditional_branch(loop_branch);
        self.move_to_block(loop_branch);

        let cmp = self.compile_expr(condition);
        let comparison = match cmp {
            BasicValueEnum::IntValue(v) => v,
            _ => panic!("could not get int value from cmp")
        };

        // create merge branch
        let merge = self.get_while_temp("merge");
        let merge_block = self.context.append_basic_block(function, &merge);

        // create while loop body branch
        let while_temp = self.get_while_temp("loop_body");
        let while_block = self.context.append_basic_block(function, &while_temp);

        // write conditional br in %loop_branch
        let _ = self.builder.build_conditional_branch(comparison, while_block, merge_block);

        // write while loop body
        self.move_to_block(while_block);
        self.compile_block(code_block);
        let _ = self.builder.build_unconditional_branch(loop_branch);

        self.move_to_block(merge_block);
    }

    fn compile_procedure_declaration(&mut self, identifier: Token, params: Vec<Parameter>, code_block: Vec<AstNode> ) {
        let prev_block = self.current_block;
        let prev_func = self.current_func;

        // get function identifier
        let function_name = match identifier {
            Token::Identifier(id) => id,
            _ => panic!("Expected token"),
        };

        // collect parameters and get types
        let mut param_types = Vec::new();
        for param in params.clone() {
           let param_type = param.param_type;
            let pty = self.llvm_type_converter(param_type);
            param_types.push(pty.into());
        }

        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&param_types, false);

        // create the llvm function
        let function = self.module.add_function(&function_name, fn_type, None);
        let entry_block = function.get_last_basic_block().unwrap_or_else(|| self.context.append_basic_block(function, "entry"));
        self.move_to_block(entry_block);

        // save params as variables
        for (i, param) in params.iter().enumerate() {
            let param_name = match &param.identifier {
                Expression::Identifier(name) => name.clone(),
                _ => panic!("Expected identifier for parameter"),
            };

            let param_value = function.get_nth_param(i as u32)
                .expect("Failed to get parameter value");

            match param.param_type {
                Type::Str | Type::Strl(_) => {
                    let array_type = match param_value {
                        BasicValueEnum::ArrayValue(av) => av,
                        _ => panic!("Expected array type for string parameter"),
                    };

                    let alloca = self.builder.build_alloca(array_type.get_type(), &param_name)
                        .expect("Failed to create alloca for string parameter");

                    self.builder.build_store(alloca, param_value)
                        .expect("Failed to store string parameter value");

                    let variable = Variable {
                        alloca,
                        var_type: array_type.get_type().into(),
                        var_counter: 0,
                        parsed_type: param.param_type.clone(),
                        value: param_value,
                    };

                    self.variables.insert(param_name, variable);
                },
                _ => {
                    let param_type = self.llvm_type_converter(param.param_type.clone());
                    let alloca = self.builder.build_alloca(param_type, &param_name)
                        .expect("Failed to create alloca for parameter");

                    self.builder.build_store(alloca, param_value)
                        .expect("Failed to store parameter value");

                    let variable = Variable {
                        alloca,
                        var_type: param_type,
                        var_counter: 0,
                        parsed_type: param.param_type.clone(),
                        value: param_value,
                    };

                    self.variables.insert(param_name, variable);
                }
            }
        }

        self.compile_block(code_block);
        let _ = self.builder.build_return(Some(&self.context.i32_type().const_int(1, false)));


        self.module.get_function(prev_func.expect("Couldnt get prev func").get_name().to_str().expect("Couldnt turn into str"));
        self.builder.position_at_end(prev_block.expect("Couldnt get prev block"));
        self.functions.insert(function_name, function);

    }

    fn compile_function_declaration(&mut self, identifier: Token, params: Vec<Parameter>, code_block: Vec<AstNode>, return_type: Type) {
        let prev_block = self.current_block;
        let prev_func = self.current_func;

        // get function identifier
        let function_name = match identifier {
            Token::Identifier(id) => id,
            _ => panic!("Expected token"),
        };

        // collect parameters and get types
        let mut param_types = Vec::new();
        for param in params.clone() {
           let param_type = param.param_type;
            let pty = self.llvm_type_converter(param_type);
            param_types.push(pty.into());
        }

        // get return type
        let ret_type = self.llvm_type_converter(return_type);
        println!("RET TYPE {:?}", ret_type);
        let fn_type = match ret_type {
            BasicTypeEnum::IntType(t) => t.fn_type(&param_types, false), // also handles bool types
            BasicTypeEnum::FloatType(t) => t.fn_type(&param_types, false),
            BasicTypeEnum::ArrayType(t) => t.fn_type(&param_types, false),
            _ => panic!("Unsupported return type for function"),
        };

        println!("FN TYPE {:?}", fn_type);

        // create the llvm function
        let function = self.module.add_function(&function_name, fn_type, None);
        let entry_block = function.get_last_basic_block().unwrap_or_else(|| self.context.append_basic_block(function, "entry"));
        self.move_to_block(entry_block);

        // save params as variables
        for (i, param) in params.iter().enumerate() {
            let param_name = match &param.identifier {
                Expression::Identifier(name) => name.clone(),
                _ => panic!("Expected identifier for parameter"),
            };

            let param_value = function.get_nth_param(i as u32)
                .expect("Failed to get parameter value");

            match param.param_type {
                Type::Str | Type::Strl(_) => {
                    let array_type = match param_value {
                        BasicValueEnum::ArrayValue(av) => av,
                        _ => panic!("Expected array type for string parameter"),
                    };

                    let alloca = self.builder.build_alloca(array_type.get_type(), &param_name)
                        .expect("Failed to create alloca for string parameter");

                    self.builder.build_store(alloca, param_value)
                        .expect("Failed to store string parameter value");

                    let variable = Variable {
                        alloca,
                        var_type: array_type.get_type().into(),
                        var_counter: 0,
                        parsed_type: param.param_type.clone(),
                        value: param_value,
                    };

                    self.variables.insert(param_name, variable);
                },
                _ => {
                    let param_type = self.llvm_type_converter(param.param_type.clone());
                    let alloca = self.builder.build_alloca(param_type, &param_name)
                        .expect("Failed to create alloca for parameter");

                    self.builder.build_store(alloca, param_value)
                        .expect("Failed to store parameter value");

                    let variable = Variable {
                        alloca,
                        var_type: param_type,
                        var_counter: 0,
                        parsed_type: param.param_type.clone(),
                        value: param_value,
                    };

                    self.variables.insert(param_name, variable);
                }
            }
        }

        self.compile_block(code_block);

        self.module.get_function(prev_func.expect("Couldnt get prev func").get_name().to_str().expect("Couldnt turn into str"));
        self.builder.position_at_end(prev_block.expect("Couldnt get prev block"));
        self.functions.insert(function_name, function);

    }

    fn compile_return_statement(&mut self, value: Expression) {
        let ret_value = self.compile_expr(value);

        let _ = self.builder.build_return(Some(&ret_value));
    }

    fn compile_function_call(&mut self, identifier: Box<Expression>, params: Vec<Expression>, _return_type: Type) {
        let function_name = match *identifier {
            Expression::Identifier(id) => id,
            _ => panic!("Couldnt get function identifier for function call"),
        };

        let name = self.get_temp("function_result");

        let function = self.functions.get(&function_name).expect(&format!("Function '{}' not found", function_name)).clone();

        let mut compiled_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
        for param in params {
            let compiled_param = self.compile_expr(param);
            let _cmpp = match compiled_param {
                BasicValueEnum::ArrayValue(av) => {
                    compiled_args.push(av.into());
                }
                _ => {
                    compiled_args.push(compiled_param.into());
                }
            };
        }

        let _ = self.builder.build_call(function, &compiled_args, &name);

    }
}

