use crate::parser::AstNode;

pub struct CodeGenerator {
    ast_nodes: Vec<AstNode>,
    out: String,
}

impl CodeGenerator {
    fn new(&mut self, nodes: Vec<AstNode>) -> Self {
        CodeGenerator {
            ast_nodes: nodes,
            out: String::new(),
        }
    }

    fn generate(&mut self) {
        for node in self.ast_nodes {
            match node {
                AstNode::VariableDeclaration { identifier, value } => self.out.push_str(format!("{} = {}", identifier, value)),
                _ => panic!("cannot generate code from node: {}", node)
            }
        }

    }
}
