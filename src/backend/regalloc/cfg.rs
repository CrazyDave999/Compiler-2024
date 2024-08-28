use std::collections::{HashMap, HashSet};
use super::IRNode;
use super::mem2reg;
pub struct Instruction {
    pub ir_: IRNode,
    pub use_: HashSet<String>,
    pub def_: HashSet<String>,
    pub in_: HashSet<String>,
    pub out_: HashSet<String>,
}
impl Instruction {
    pub fn from(ir: &IRNode) -> Self {
        Instruction {
            ir_: ir.clone(),
            use_: ir.get_use(),
            def_: ir.get_def(),
            in_: HashSet::new(),
            out_: HashSet::new(),
        }
    }
}
pub struct BasicBlock {
    pub succ: HashSet<String>,
    pub pred: HashSet<String>,
    pub ch: Vec<Instruction>,
    pub use_: HashSet<String>,
    pub def_: HashSet<String>,
    pub in_: HashSet<String>,
    pub out_: HashSet<String>,
}

impl BasicBlock {
    pub fn from(bb: &mem2reg::BasicBlock) -> Self {
        BasicBlock {
            succ: bb.succ.clone(),
            pred: bb.pred.clone(),
            ch: bb.ir.iter().map(|x| Instruction::from(x)).collect(),
            use_: HashSet::new(),
            def_: HashSet::new(),
            in_: HashSet::new(),
            out_: HashSet::new(),
        }
    }
}
pub struct Node {
    pub neighbors: HashSet<String>,
    pub color: i32,
}

impl Node{
    pub fn get_degree(&self) -> i32 {
        self.neighbors.len() as i32
    }
}
pub struct CFG {
    pub cfg: HashMap<String, BasicBlock>, // control flow graph
    pub ig: HashMap<String, Node>, // interference graph
}

impl CFG {
    pub fn from(cfg: &HashMap<String, mem2reg::BasicBlock>) -> Self {
        CFG {
            cfg: cfg.iter().map(|(k, v)| (k.clone(), BasicBlock::from(v))).collect(),
            ig: HashMap::new(),
        }
    }
    pub fn get_ir(&self) -> Vec<Instruction> {
        Vec::new()
    }

    pub fn add_edge(&mut self, from: &str, to: &str) {
        self.ig.get_mut(from).unwrap().neighbors.insert(to.to_string());
        self.ig.get_mut(to).unwrap().neighbors.insert(from.to_string());
    }
}