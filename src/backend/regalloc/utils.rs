use std::collections::{HashMap, HashSet};
use super::IRNode;

pub struct AllocResult {
    pub ir: Vec<IRNode>,
    pub color: HashMap<String, Box<HashMap<String, String>>>,
    pub spill_temps: HashMap<String, Box<HashSet<String>>>,
    pub caller_saved_regs: Vec<String>,
    pub callee_saved_regs: Vec<String>,
}
impl AllocResult {
    pub fn new() -> Self {
        AllocResult {
            ir: Vec::new(),
            color: HashMap::new(),
            spill_temps: HashMap::new(),
            caller_saved_regs: [
                "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%a6", "%a7", "%t0", "%t1", "%t2", "%t3", "%t4", "%t5", "%t6"
            ].into_iter().map(|x| x.to_string()).collect(),
            callee_saved_regs: [
                "%s0", "%s1", "%s2", "%s3", "%s4", "%s5", "%s6", "%s7", "%s8", "%s9", "%s10", "%s11"
            ].into_iter().map(|x| x.to_string()).collect(),
        }
    }
}