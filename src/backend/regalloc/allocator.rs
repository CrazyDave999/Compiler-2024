use std::collections::{HashMap, HashSet};
use super::mem2reg;
use super::utils::SelectStack;
use super::ig::InterferenceGraph;
use super::cfg::ControlFlowGraph;


pub struct Allocator {
    pub cfg: ControlFlowGraph, // control flow graph
    pub ig: InterferenceGraph, // interference graph
    pub k: i32,
    pub phy_regs: HashSet<String>,
    pub sel_stk: SelectStack,
}

impl Allocator {
    pub fn from(cfg: &HashMap<String, mem2reg::BasicBlock>) -> Self {
        let mut res = Allocator {
            cfg: ControlFlowGraph::from(cfg),
            ig: InterferenceGraph::new(),
            k: 28,
            phy_regs: [
                "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
            ].iter().map(|&x| x.to_string()).collect(),
            sel_stk: SelectStack::new(),
        };
        res.cfg.live_analysis();
        res.ig.build(res.cfg.get_inst());
        res
    }
}