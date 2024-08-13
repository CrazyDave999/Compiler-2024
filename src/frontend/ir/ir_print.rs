use super::IRNode;

pub fn print_ir(ir: Vec<IRNode>) {
    let mut indent = 0;
    for node in ir {
        if let IRNode::FuncEnd = node {
            indent -= 2;
        }
        print!("{}{}"," ".repeat(indent), node);
        if let IRNode::FuncBegin(_, _, _) = node {
            indent += 2;
        }
    }
}