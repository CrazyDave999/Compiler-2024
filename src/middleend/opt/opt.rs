use super::IRNode;
use super::inline;

pub fn pass(ir: Vec<IRNode>)->Vec<IRNode>{
    inline::pass(ir)
}