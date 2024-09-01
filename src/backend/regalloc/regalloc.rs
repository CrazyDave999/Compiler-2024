use std::collections::HashMap;
use super::IRNode;
use super::mem2reg;
use super::Allocator;

pub fn pass(ir: Vec<IRNode>) -> Vec<IRNode> {
    let mut res = Vec::new();
    let mut in_func = false;
    let mut func_inner = Vec::new();
    for node in ir {
        match node {
            IRNode::FuncBegin(_, _, _) => {
                res.push(node);
                in_func = true;
            }
            IRNode::FuncEnd => {
                let mut alloc = Allocator::from(func_inner.clone());
                alloc.main();
                res.extend(alloc.get_ir());
                func_inner.clear();
                in_func = false;
                res.push(node);
            }
            _ => {
                if in_func {
                    func_inner.push(node);
                } else {
                    res.push(node);
                }
            }
        }
    }
    res
}