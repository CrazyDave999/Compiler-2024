use super::IRNode;
use bit_set::BitSet;
use std::collections::{HashMap, HashSet};

#[derive(Eq, PartialEq, Clone)]
pub struct Instruction {
    pub ir_: IRNode,
    pub use_: BitSet,
    pub def_: BitSet,
    pub in_: BitSet,
    pub out_: BitSet,
    pub idx: usize,
}
impl Instruction {
    pub fn from(ir: &IRNode) -> Self {
        Instruction {
            ir_: ir.clone(),
            use_: BitSet::new(),
            def_: BitSet::new(),
            in_: BitSet::new(),
            out_: BitSet::new(),
            idx: 0,
        }
    }
}

#[derive(Clone)]
pub struct BasicBlock {
    pub succ: BitSet,
    pub pred: BitSet,
    pub ch: Vec<Instruction>,
    pub use_: BitSet,
    pub def_: BitSet,
    pub in_: BitSet,
    pub out_: BitSet,
}
impl BasicBlock {
    pub fn new() -> Self {
        BasicBlock {
            succ: BitSet::new(),
            pred: BitSet::new(),
            ch: Vec::new(),
            use_: BitSet::new(),
            def_: BitSet::new(),
            in_: BitSet::new(),
            out_: BitSet::new(),
        }
    }
}

pub struct AllocResult {
    pub ir: Vec<IRNode>,
    pub color: HashMap<String, Box<HashMap<String, String>>>,
    pub spill_temps: HashMap<String, Box<HashSet<String>>>,
}
impl AllocResult {
    pub fn new() -> Self {
        AllocResult {
            ir: Vec::new(),
            color: HashMap::new(),
            spill_temps: HashMap::new(),
        }
    }
}

pub fn is_reg(s: &str) -> bool {
    s.starts_with("%")
}
