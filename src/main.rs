use crate::compiler::Compiler;
use std::fs;
use std::env;

pub mod lexer;
pub mod parser;
pub mod compiler;
pub mod code_generator;
pub mod stdlib;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = &args[1];
    let (binding, file_name) = get_file(file.to_string());

    println!("FILE : {}", file);
    let mut compiler = Compiler::new(&binding);
    compiler.compile(file_name.to_string());

    if args.len() == 2 {
        compiler.output_llvm(&file_name);
    } else if args.len() > 2 {
        if &args[2] == "--build" {
            compiler.build_llvm(&file_name)
        }
    }
}

fn get_file(file: String) -> (String, String) {
    let file_split: Vec<_> = file.split(".").collect();
    let file_name = file_split[0];

    let cwd = match env::current_dir() {
        Ok(path) => {
            let path_str = path.to_string_lossy().to_string();
            path_str
        }
        _ => panic!("Error turning cwd into a string"),
    };

    let args: Vec<_> = env::args().collect();
    let input = if file_name.contains("test") && args.len() < 2 {
        format!("{}/src/tests/{}", cwd, file)
    } else if file_name.contains("test") && args.len() >= 2  {
        format!("{}/tests/{}", cwd, file)
    }
    else {
        format!("{}/{}", cwd, file)
    };

    let binding = match fs::read_to_string(input.clone()) {
        Ok(contents) => contents,
        Err(..) => panic!("{}", format!("Cannot read file, input: {}", input)),
    };


    (binding, file_name.to_string())
}

// example test
//
// binding = "DECLARE x INITIALLY 1"
// let mut compiler = Compiler::new(&binding);
// compiler.compile(file_name.to_string());
// let output = compiler.output_llvm(file_name);

#[allow(unused_macros)]
macro_rules! create_test {
    ($file_name:ident, $expected_result:expr) => {

        #[test]
        fn $file_name() {
            let (binding, file_name) = get_file(format!("{}.sqarl", stringify!($file_name)).to_string());
            let output = Compiler::test(binding, file_name);
            println!("OUTPUT: \n {}", output);
            assert_eq!(output, $expected_result)
        }
    };
}

#[cfg(test)]
mod test {
    use super::*;

    // string testing
    create_test!(basic_string_concat_test, "Hello world!");
    create_test!(basic_string_test, "Hello world!");
    create_test!(variable_string_test, "Hello world!");

    // function testing
    create_test!(function_string_test, "Hello world!\nfunction string: Hello world!");
    create_test!(function_integer_test, "123");
    create_test!(function_float_test, "123.123000");
    create_test!(function_boolean_test, "1");

    // procedure testing
    create_test!(procedure_string_test, "Hello world!");
    create_test!(procedure_integer_test, "123");
    create_test!(procedure_float_test, "123.123000");
    create_test!(procedure_boolean_test, "1");

    // loops testing
    create_test!(while_loop_test, "0\n1\n2\n3\n4\n5");

}
