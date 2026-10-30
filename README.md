# SQARL Compiler in Rust

SQARL Compiler is a small compiler project written in Rust. It was created as a
way to learn the fundamentals of compiler construction while becoming more
comfortable with Rust and memorising the SQA reference language syntax for computing exams in school.

The project experiments with the main stages of a compiler:

- **Lexing** source code into tokens
- **Parsing** tokens into an abstract syntax tree (AST)
- **Compiling** AST nodes
- **Generating LLVM IR** with [Inkwell](https://github.com/TheDan64/inkwell)

SQARL is a small, English-like language. Example source code looks like this:

```sqarl
DECLARE hello INITIALLY AS BOOLEAN TRUE
```

## Current features

The lexer and parser contain support for concepts including:

- Variables and assignments
- Integers, floats, strings, booleans, arrays, records, and classes
- Arithmetic and comparison operators
- Conditional statements
- `while` and `for` loops
- Functions and procedures
- Return statements
- Basic input, output, and file operations

The code generator is an ongoing work in progress, so not every parsed language
feature is compiled yet.

## Getting started

### Prerequisites

- [Rust](https://www.rust-lang.org/tools/install) and Cargo
- LLVM 15, required by the `inkwell` and `llvm-sys` dependencies

### Run the project

Clone the repository, then run:

```bash
cargo run
```

The current executable reads the example program from
`src/example.sqarl`, lexes and parses it, and prints the resulting output.

### Build and check

```bash
cargo build
cargo check
```

## Project structure

```text
src/
├── main.rs            # Program entry point
├── lexer.rs           # SQARL tokenizer
├── parser.rs          # Parser and AST definitions
├── compiler.rs        # Early AST-to-output compiler
├── code_generator.rs  # LLVM IR code generation
└── example.sqarl     # Example SQARL program

compiled_tests/        # Example generated LLVM IR files
```

## Project status

This is primarily a learning project rather than a complete or production-ready
compiler. The language design, parser, and LLVM backend are still evolving.
The implementation may be incomplete, and compiler errors currently tend to be
reported with panics while the core ideas are being explored.

## Learning goals

This project is intended to provide hands-on practice with:

- Rust modules, enums, structs, ownership, and pattern matching
- Designing a small programming language
- Tokenization and recursive-descent parsing
- AST design
- Translating language constructs into LLVM IR

