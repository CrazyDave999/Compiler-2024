use std::collections::{HashMap, HashSet};
use crate::middleend::ir::IRNode;
use super::mem2reg;
use super::cfg::{ControlFlowGraph, Instruction};


pub struct Allocator {
    cfg: ControlFlowGraph,
    k: i32,
    phy_regs: HashSet<String>,

    // node sets and work lists
    pre_colored: HashSet<String>,
    initial: HashSet<String>,
    simplify_work_list: HashSet<String>,
    freeze_work_list: HashSet<String>,
    spill_work_list: HashSet<String>,
    spilled_nodes: HashSet<String>,
    coalesced_nodes: HashSet<String>,
    colored_nodes: HashSet<String>,
    select_stack: Vec<String>,

    // move inst sets
    coalesced_moves: HashSet<Instruction>,
    constrained_moves: HashSet<Instruction>,
    frozen_moves: HashSet<Instruction>,
    work_list_moves: HashSet<Instruction>,
    active_moves: HashSet<Instruction>,

    // other data structures
    adj_set: HashSet<(String, String)>,
    adj_list: HashMap<String, HashSet<String>>,
    degree: HashMap<String, i32>,
    move_list: HashMap<String, HashSet<Instruction>>,
    alias: HashMap<String, String>,
    color: HashMap<String, String>,
}

impl Allocator {
    pub fn from(cfg: &HashMap<String, mem2reg::BasicBlock>) -> Self {
        Allocator {
            cfg: ControlFlowGraph::from(cfg),
            k: 28,
            phy_regs: [
                "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
            ].into_iter().map(|x| x.to_string()).collect(),
            pre_colored: HashSet::new(),
            initial: HashSet::new(),
            simplify_work_list: HashSet::new(),
            freeze_work_list: HashSet::new(),
            spill_work_list: HashSet::new(),
            spilled_nodes: HashSet::new(),
            coalesced_nodes: HashSet::new(),
            colored_nodes: HashSet::new(),
            select_stack: Vec::new(),
            coalesced_moves: HashSet::new(),
            constrained_moves: HashSet::new(),
            frozen_moves: HashSet::new(),
            work_list_moves: HashSet::new(),
            active_moves: HashSet::new(),
            adj_set: HashSet::new(),
            adj_list: HashMap::new(),
            degree: HashMap::new(),
            move_list: HashMap::new(),
            alias: HashMap::new(),
            color: HashMap::new(),
        }
    }
    pub fn main(&mut self) {
        self.live_analysis();
        self.build();
        self.make_work_list();
        while !self.simplify_work_list.is_empty() || !self.work_list_moves.is_empty() || !self.freeze_work_list.is_empty() || !self.spill_work_list.is_empty() {
            if !self.simplify_work_list.is_empty() {
                self.simplify();
            } else if !self.work_list_moves.is_empty() {
                self.coalesce();
            } else if !self.freeze_work_list.is_empty() {
                self.freeze();
            } else if !self.spill_work_list.is_empty() {
                self.select_spill();
            }
        }
        self.assign_colors();
        if !self.spilled_nodes.is_empty() {
            self.rewrite_program();
            self.main();
        }
    }
    fn live_analysis(&mut self) {
        self.cfg.live_analysis();
    }
    fn build(&mut self) {
        for inst in self.cfg.get_inst() {
            match &inst.ir_ {
                IRNode::Move(_, _) => {
                    for n in inst.use_.union(&inst.def_) {
                        self.move_list.entry(n.to_string()).or_insert(HashSet::new()).insert(inst.clone());
                    }
                    self.work_list_moves.insert(inst.clone());
                }
                IRNode::Call(_, _, _, _) => {
                    for (i, n) in inst.use_.iter().enumerate() {
                        self.pre_colored.insert(n.to_string());
                        self.color.insert(n.to_string(), format!("a{}", i));
                    }
                    for n in &inst.def_ {
                        self.pre_colored.insert(n.to_string());
                        self.color.insert(n.to_string(), "a0".to_string());
                    }
                }
                _ => {}
            }
            for (u, v) in inst.def_.iter().zip(inst.in_.iter()) {
                self.add_edge(u, v);
            }
        }
    }

    fn add_edge(&mut self, u: &str, v: &str) {
        if u != v && !self.adj_set.contains(&(u.to_string(), v.to_string())) {
            self.adj_set.insert((u.to_string(), v.to_string()));
            self.adj_set.insert((v.to_string(), u.to_string()));
            if !self.pre_colored.contains(u) {
                self.adj_list.entry(u.to_string()).or_insert(HashSet::new()).insert(v.to_string());
                *self.degree.entry(u.to_string()).or_insert(0) += 1;
            }
            if !self.pre_colored.contains(v) {
                self.adj_list.entry(v.to_string()).or_insert(HashSet::new()).insert(u.to_string());
                *self.degree.entry(v.to_string()).or_insert(0) += 1;
            }
        }
    }

    fn make_work_list(&mut self) {
        for n in self.initial.clone() {
            if self.degree[&n] >= self.k {
                self.spill_work_list.insert(n);
            } else if self.move_related(&n) {
                self.freeze_work_list.insert(n);
            } else {
                self.simplify_work_list.insert(n);
            }
        }
        self.initial.clear();
    }
    fn adjacent(&self, n: &str) -> HashSet<String> {
        self.adj_list.get(n).unwrap().clone().difference(
            &self.select_stack.iter().cloned().collect::<HashSet<_>>().union(
                &self.coalesced_nodes
            ).cloned().collect()
        ).cloned().collect()
    }
    fn node_moves(&self, n: &str) -> HashSet<Instruction> {
        self.move_list.get(n).unwrap().clone().intersection(
            &self.active_moves.union(&self.work_list_moves).cloned().collect()
        ).cloned().collect()
    }
    fn move_related(&self, n: &str) -> bool {
        !self.node_moves(n).is_empty()
    }

    fn simplify(&mut self) {
        let n = self.simplify_work_list.iter().next().unwrap().to_string();
        self.simplify_work_list.remove(&n);
        self.select_stack.push(n.clone());
        for m in self.adjacent(&n) {
            self.decrement_degree(&m);
        }
    }
    fn decrement_degree(&mut self, m: &str) {
        let d = self.degree[m];
        *self.degree.get_mut(m).unwrap() = d - 1;
        if d == self.k {
            self.enable_moves(
                self.adjacent(m).union(
                    &[m.to_string()].into_iter().collect::<HashSet<_>>()
                ).cloned().collect()
            );
            self.spill_work_list.remove(m);
            if self.move_related(m) {
                self.freeze_work_list.insert(m.to_string());
            } else {
                self.simplify_work_list.insert(m.to_string());
            }
        }
    }
    fn enable_moves(&mut self, nodes: HashSet<String>) {
        for n in nodes {
            for m in self.node_moves(&n) {
                if self.active_moves.contains(&m) {
                    self.active_moves.remove(&m);
                    self.work_list_moves.insert(m.clone());
                }
            }
        }
    }

    fn coalesce(&mut self) {
        let m = self.work_list_moves.iter().next().unwrap().clone();
        let (x, y) = (m.def_.iter().next().unwrap(), m.use_.iter().next().unwrap());
        let (u, v) = if self.pre_colored.contains(y) {
            (y, x)
        } else {
            (x, y)
        };
        self.work_list_moves.remove(&m);
        if u == v {
            self.coalesced_moves.insert(m.clone());
            self.add_work_list(&u);
        } else if self.pre_colored.contains(v) || self.adj_set.contains(&(u.to_string(), v.to_string())) {
            self.constrained_moves.insert(m.clone());
            self.add_work_list(&u);
            self.add_work_list(&v);
        } else if (self.pre_colored.contains(u) && self.adjacent(v).iter().all(|t| self.ok(t, u)))
            || (!self.pre_colored.contains(u) && self.conservative(self.adjacent(u).union(&self.adjacent(v)).cloned().collect())
        ) {
            self.coalesced_moves.insert(m.clone());
            self.combine(u, v);
            self.add_work_list(&u);
        } else {
            self.active_moves.insert(m.clone());
        }
    }
    fn add_work_list(&mut self, u: &str) {
        if !self.pre_colored.contains(u) && !self.move_related(u) && self.degree[u] < self.k {
            self.freeze_work_list.remove(u);
            self.simplify_work_list.insert(u.to_string());
        }
    }
    fn ok(&self, t: &str, r: &str) -> bool {
        self.degree[t] < self.k || self.pre_colored.contains(t) || self.adj_set.contains(&(t.to_string(), r.to_string()))
    }
    fn conservative(&self, nodes: HashSet<String>) -> bool {
        let mut k = 0;
        for n in nodes {
            if self.degree[&n] >= self.k {
                k += 1;
            }
        }
        k < self.k
    }
    fn get_alias(&self, n: &str) -> String {
        if self.coalesced_nodes.contains(n) {
            self.get_alias(&self.alias[n])
        } else {
            n.to_string()
        }
    }
    fn combine(&mut self, u: &str, v: &str) {
        if self.freeze_work_list.contains(v) {
            self.freeze_work_list.remove(v);
        } else {
            self.spill_work_list.remove(v);
        }
        self.coalesced_nodes.insert(v.to_string());
        self.alias.insert(v.to_string(), u.to_string());

        let move_list_v = self.move_list.get(v).unwrap().clone();
        self.move_list.get_mut(u).unwrap().extend(move_list_v.into_iter());
        self.enable_moves([v.to_string()].into_iter().collect());
        for t in self.adjacent(v) {
            self.add_edge(&t, u);
            self.decrement_degree(&t);
        }
        if self.degree[u] >= self.k && self.freeze_work_list.contains(u) {
            self.freeze_work_list.remove(u);
            self.spill_work_list.insert(u.to_string());
        }
    }

    fn freeze(&mut self) {
        let u = self.freeze_work_list.iter().next().unwrap().clone();
        self.freeze_work_list.remove(&u);
        self.simplify_work_list.insert(u.clone());
        self.freeze_moves(&u);
    }
    fn freeze_moves(&mut self, u: &str) {
        for m in self.node_moves(u) {
            let (x, y) = (m.def_.iter().next().unwrap(), m.use_.iter().next().unwrap());
            let v = if self.get_alias(y) == self.get_alias(u) {
                self.get_alias(x)
            } else {
                self.get_alias(y)
            };
            self.active_moves.remove(&m);
            self.frozen_moves.insert(m.clone());
            if self.node_moves(&v).is_empty() && self.degree[&v] < self.k {
                self.freeze_work_list.remove(&v);
                self.simplify_work_list.insert(v.clone());
            }
        }
    }
    fn select_spill(&mut self) {
        // 启发式地寻找一个高度数节点，让它入栈，从而被溢出。
        // 选择度数最大的节点。
        let mut max_deg = 0;
        let mut m = String::new();
        for n in self.spill_work_list.iter() {
            if self.degree[n] > max_deg {
                max_deg = self.degree[n];
                m = n.clone();
            }
        }
        self.spill_work_list.remove(&m);
        self.simplify_work_list.insert(m.clone());
        self.freeze_moves(&m);
    }
    fn assign_colors(&mut self) {
        while !self.select_stack.is_empty() {
            let n = self.select_stack.pop().unwrap();
            let mut ok_colors = self.phy_regs.clone();
            for w in self.adj_list[&n].iter() {
                let alias_w = self.get_alias(w);
                if self.colored_nodes.contains(&alias_w) || self.pre_colored.contains(&alias_w) {
                    ok_colors.remove(&self.color[&alias_w]);
                }
            }
            if ok_colors.is_empty() {
                self.spilled_nodes.insert(n.clone());
            } else {
                self.colored_nodes.insert(n.clone());
                let c = ok_colors.iter().next().unwrap().clone();
                self.color.insert(n.clone(), c);
            }
        }
        for n in self.coalesced_nodes.iter() {
            self.color.insert(n.clone(), self.color[&self.get_alias(n)].clone());
        }
    }
    fn rewrite_program(&mut self) {
        // 此时spill_nodes中即为实际溢出的节点，为每一个
    }
}