use crate::parser::AstNode;

struct Compiler {
    input: Vec<AstNode>,
    output: String,
    variable_count: i32,
}

impl Compiler {
    fn new(&mut self, input: Vec<AstNode>) -> Self {
        Compiler {
            input,
            output: String::new(),
            variable_count: 0,
        }
    }

    fn compile(&mut self) {
        for node in self.input.iter_mut() {
            match node {
                AstNode::VariableDeclaration { identifier, value, var_type } => self.compile_variable_declaration(),
                _ => panic!("Cannot compile with node: {}", node)
            }
        }

    }

    fn compile_variable_declaration(&mut self) {

    }


}
