use core::panic;
use inkwell::types::BasicTypeEnum;
use inkwell::context::Context;
use inkwell::values::PointerValue;
use inkwell::builder::Builder;
use std::collections::HashMap;

use crate::parser::{AstNode, Expression, Type};

pub struct CodeGenerator<'ctx> {
    pub context: &'ctx Context,
    builder: Builder<'ctx>,
    pub module: inkwell::module::Module<'ctx>,
    pub variables: HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,
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
        let input: Vec<_> = match input.clone() {
            AstNode::Program(nodes) => nodes,
            _ => panic!("Expected program node")
        };

        self.create_main_func();

        for node in input {
            match *node {
                AstNode::VariableDeclaration { identifier, value, var_type } => self.compile_variable_declaration(identifier, value, var_type),
                AstNode::VariableAssignment { identifier, value } => self.compile_variable_assignment(identifier, value),
                _ => panic!("Cannot compile with node: {}", node)
            }
        }

        self.add_return_to_main();
        self.module.print_to_stderr();
    }

    pub fn create_main_func(&mut self) {
        let ret_type = self.context.i32_type();
        let fn_type = ret_type.fn_type(&[], false);

        let function = self.module.add_function("main", fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);
    }

    fn compile_variable_declaration(&mut self, identifier: Expression, value: Expression, var_type: Type) {
        let variable_identifier = if let Expression::Identifier(value) = identifier {
            value
        } else {
            panic!("No variable identifier to compile")
        };

        let ty = self.llvm_type_converter(var_type);
        let alloca = self.builder.build_alloca(ty, &variable_identifier).expect("ERROR with alloca in var dec");

        let inital_value = self.compile_expr(value);
        self.builder.build_store(alloca, inital_value).expect("Error building store");

        self.variables.insert(variable_identifier, (alloca, ty));
    }

    // IDEA
    //
    // To combat the issue with string assignment ie storing more bits than possible -
    //   store [13 x i8] c"Hello world!\00", ptr %example, align 1
    //   store [9 x i8] c"Example!\00", ptr %example, align 1
    fn compile_variable_assignment(&mut self, identifier: Expression, value: Expression) {
        let variable_identifier = if let Expression::Identifier(id) = identifier {
            id
        } else {
            panic!("No variable identifier to compile")
        };

        let val = self.compile_expr(value);

        if let Some((alloca, _)) = self.variables.get(&variable_identifier) {
            let _ = self.builder.build_store(alloca.clone(), val.clone());
        } else {
            panic!("Couldnt get alloca ty in variable assignment");
        }

    }

    fn compile_expr(&mut self, expr: Expression) -> inkwell::values::BasicValueEnum<'ctx> {
        match expr {
            Expression::StringLiteral(s) => inkwell::values::BasicValueEnum::ArrayValue(self.context.const_string(s.as_bytes(), true)),
            Expression::IntegerLiteral(val) => self.context.i32_type().const_int(val as u64, false).into(),
            Expression::FloatLiteral(val) => self.context.f64_type().const_float(val.parse::<f64>().expect("cannot turn val into f64")).into(),
            Expression::Identifier(id) => self.build_load(id),
            // Expression::BinaryOp(left, op, right) => {
            //     self.compile_binary_op(*left, op, *right)
            // }
            _ => panic!("Cannot compile expression: {:?}", expr),
        }
    }

    // ill continue this later
    // fn compile_binary_op(&mut self, left: Expression, op: Token, right: Expression ) -> inkwell::values::BasicValueEnum<'ctx> {
    //     match op {
    //         Token::Addition => {

    //         },
    //         _ => panic!("Binary op compilation failed")
    //     }
    // }

    fn build_load(&mut self, id: String) -> inkwell::values::BasicValueEnum<'ctx> {
        self.temp_counter += 1;
        let temp = format!("temp{}_load", self.temp_counter);
        let (alloca, ty) = self.variables.get(&id).expect("Could not get either alloca or type in build load");
        let value = self.builder.build_load(*ty, *alloca, &temp).expect("Couldnt build load").into();
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
}
