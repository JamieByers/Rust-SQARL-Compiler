fn display(node: &ASTNode, indent: usize) {
    let indentation = "    ".repeat(indent);
    match node {
        ASTNode::Program(nodes) => {
            println!("{}Program {{", indentation);
            for node in nodes {
                display(node, indent + 1);
            }
            println!("{}}}", indentation);
        }
        ASTNode::VariableDeclaration { identifier, value, var_type } => {
            println!("{}VariableDeclaration ({}):", indentation, var_type);
            display(identifier, indent + 1);
            display(value, indent + 1);
        }
        ASTNode::VariableAssignment { identifier, value } => {
            println!("{}VariableAssignment:", indentation);
            display(identifier, indent + 1);
            display(value, indent + 1);
        }
        ASTNode::SendToDisplay { value } => {
            println!("{}SendToDisplay:", indentation);
            display(value, indent + 1);
        }
        ASTNode::IfStatement { condition, code_block, elif_statements, else_statement } => {
            println!("{}IfStatement:", indentation);
            println!("{}    condition:", indentation);
            display(condition, indent + 2);
            println!("{}    code_block:", indentation);
            for node in code_block {
                display(node, indent + 2);
            }
            for elif in elif_statements {
                display(elif, indent + 1);
            }
            if let Some(else_stmt) = else_statement {
                println!("{}    else_statement:", indentation);
                display(else_stmt, indent + 2);
            }
        }
        ASTNode::WhileStatement { condition, code_block } => {
            println!("{}WhileStatement:", indentation);
            println!("{}    condition:", indentation);
            display(condition, indent + 2);
            println!("{}    code_block:", indentation);
            for node in code_block {
                display(node, indent + 2);
            }
        }
        ASTNode::RepeatUntilLoop { command, until } => {
            println!("{}RepeatUntilLoop:", indentation);
            display(command, indent + 1);
            display(until, indent + 1);
        }
        ASTNode::RepeatTimesLoop { command, times } => {
            println!("{}RepeatTimesLoop:", indentation);
            display(command, indent + 1);
            display(times, indent + 1);
        }
        ASTNode::ReturnStatement { value } => {
            println!("{}ReturnStatement:", indentation);
            display(value, indent + 1);
        }
        ASTNode::Expression(expr) => {
            println!("{}Expression:", indentation);
            display(expr, indent + 1);
        }
        ASTNode::CodeBlock(nodes) => {
            println!("{}CodeBlock:", indentation);
            for node in nodes {
                display(node, indent + 1);
            }
        }
        ASTNode::Eof => {
            println!("{}Eof", indentation);
        }
    }
}

