use std::collections::{HashMap, HashSet};
use crate::middleend::ir::IRNode;
use super::cfg::{ControlFlowGraph, Instruction};


pub struct Allocator {
    cfg: ControlFlowGraph,
    k: i32,
    phy_regs: HashSet<String>,
    caller_saved_regs: HashSet<String>,
    callee_saved_regs: HashSet<String>,
    spill_temps: HashSet<String>, // 为溢出变量创建的临时变量
    spill_vars: HashSet<String>, // 溢出变量的名字

    phy_colors: HashSet<String>,
    caller_saved_colors: HashSet<String>,
    callee_saved_colors: HashSet<String>,

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
    pub fn from(ir: Vec<IRNode>) -> Self {
        let mut res = Allocator {
            cfg: ControlFlowGraph::from(ir.clone()),
            k: 27,
            phy_regs: [
                "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%a6", "%a7", "%t0", "%t1", "%t2", "%t3", "%t4", "%t5", "%t6",  "%s0", "%s1", "%s2", "%s3", "%s4", "%s5", "%s6", "%s7", "%s8", "%s9", "%s10", "%s11"
            ].into_iter().map(|x| x.to_string()).collect(),
            caller_saved_regs: [
                "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%a6", "%a7", "%t0", "%t1", "%t2", "%t3", "%t4", "%t5", "%t6"
            ].into_iter().map(|x| x.to_string()).collect(),
            callee_saved_regs: [
                "%s0", "%s1", "%s2", "%s3", "%s4", "%s5", "%s6", "%s7", "%s8", "%s9", "%s10", "%s11"
            ].into_iter().map(|x| x.to_string()).collect(),
            phy_colors: [
                "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "t0", "t1", "t2", "t3", "t4", "t5", "t6", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11"
            ].into_iter().map(|x| x.to_string()).collect(),
            caller_saved_colors: [
                "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "t0", "t1", "t2", "t3", "t4", "t5", "t6"
            ].into_iter().map(|x| x.to_string()).collect(),
            callee_saved_colors: [
                "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11"
            ].into_iter().map(|x| x.to_string()).collect(),
            spill_temps: HashSet::new(),
            spill_vars: HashSet::new(),
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
        };
        res.pre_colored = res.phy_regs.clone();

        for inst in ir.iter() {
            res.initial.extend(inst.get_use().clone());
            res.initial.extend(inst.get_def().clone());
        }
        res.initial = res.initial.difference(&res.pre_colored).cloned().collect();
        res
    }
    fn reset(&mut self) {
        self.simplify_work_list.clear();
        self.freeze_work_list.clear();
        self.spill_work_list.clear();
        self.spilled_nodes.clear();
        self.coalesced_nodes.clear();
        self.colored_nodes.clear();
        self.select_stack.clear();
        self.coalesced_moves.clear();
        self.constrained_moves.clear();
        self.frozen_moves.clear();
        self.work_list_moves.clear();
        self.active_moves.clear();
        self.adj_set.clear();
        self.adj_list.clear();
        self.degree.clear();
        self.move_list.clear();
        self.alias.clear();
        self.color.clear();

        for reg in self.pre_colored.iter() {
            self.degree.insert(reg.to_string(), i32::MAX);
            self.color.insert(reg.to_string(), reg.chars().skip(1).collect());
        }

        for reg in self.initial.iter() {
            self.degree.insert(reg.to_string(), 0);
            self.adj_list.insert(reg.to_string(), HashSet::new());
        }
    }
    pub fn main(&mut self) {
        // println!("main procedure invoked");
        self.reset();
        // println!("live analysis begins");
        self.live_analysis();
        // println!("live analysis ends");
        // println!("build begins");
        self.build();
        // println!("build ends");
        // println!("make work list begins. initial len {}, spill len {}, freeze len {}, simplify len {}", self.initial.len(), self.spill_work_list.len(), self.freeze_work_list.len(), self.simplify_work_list.len());
        self.make_work_list();
        // println!("make work list ends");
        while !self.simplify_work_list.is_empty() || !self.work_list_moves.is_empty() || !self.freeze_work_list.is_empty() || !self.spill_work_list.is_empty() {
            if !self.simplify_work_list.is_empty() {
                // println!("simplify");
                self.simplify();
            } else if !self.work_list_moves.is_empty() {
                // println!("coalesce");
                self.coalesce();
            } else if !self.freeze_work_list.is_empty() {
                // println!("freeze");
                self.freeze();
            } else if !self.spill_work_list.is_empty() {
                // println!("select spill");
                self.select_spill();
            }
        }
        // println!("assign colors begins");
        self.assign_colors();
        // println!("assign colors ends");
        if !self.spilled_nodes.is_empty() {
            // println!("rewrite program begins");
            self.rewrite_program();
            // println!("rewrite program ends");
            self.main();
        }
    }
    fn live_analysis(&mut self) {
        self.cfg.live_analysis();
    }
    fn build(&mut self) {
        let mut edges = Vec::new();
        for (_, bb) in self.cfg.nodes.iter_mut() {
            let mut live = bb.out_.clone();
            for inst in bb.ch.iter_mut().rev() {
                match &inst.ir_ {
                    IRNode::Move(_, _, rs) => {
                        if rs.chars().next().unwrap() == '%' {
                            live = live.difference(&inst.use_).cloned().collect();
                            for n in inst.use_.union(&inst.def_) {
                                self.move_list.entry(n.to_string()).or_insert(HashSet::new()).insert(inst.clone());
                            }
                            self.work_list_moves.insert(inst.clone());
                        }
                    }
                    _ => {}
                }
                inst.out_ = live.clone();
                live = live.union(&inst.def_).cloned().collect();
                for d in inst.def_.iter() {
                    for l in live.iter() {
                        edges.push((l.clone(), d.clone()));
                    }
                }
                live = inst.use_.union(&live.difference(&inst.def_).cloned().collect()).cloned().collect();
                inst.in_ = live.clone();
            }
        }
        for (u, v) in edges {
            self.add_edge(&u, &v);
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
        for n in self.initial.iter() {
            if self.degree[n] >= self.k {
                self.spill_work_list.insert(n.clone());
            } else if self.move_related(n) {
                self.freeze_work_list.insert(n.clone());
            } else {
                self.simplify_work_list.insert(n.clone());
            }
        }
        self.initial.clear();
    }
    fn adjacent(&self, n: &str) -> HashSet<String> {
        self.adj_list.get(n).unwrap_or(&HashSet::new()).clone().difference(
            &self.select_stack.iter().cloned().collect::<HashSet<_>>().union(
                &self.coalesced_nodes
            ).cloned().collect()
        ).cloned().collect()
    }
    fn node_moves(&self, n: &str) -> HashSet<Instruction> {
        let res = self.move_list.get(n).unwrap_or(&HashSet::new()).clone().intersection(
            &self.active_moves.union(&self.work_list_moves).cloned().collect()
        ).cloned().collect();
        res
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
        if !self.pre_colored.contains(m) {
            *self.degree.get_mut(m).unwrap() = d - 1;
        }
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
            for m in &self.node_moves(&n) {
                if self.active_moves.contains(m) {
                    self.active_moves.remove(m);
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
        if let (Some(x_col), Some(y_col)) = (self.color.get(x), self.color.get(y)) {
            if x_col == y_col {
                return;
            }
        }

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
    // u <- v
    fn combine(&mut self, u: &str, v: &str) {
        if self.freeze_work_list.contains(v) {
            self.freeze_work_list.remove(v);
        } else {
            self.spill_work_list.remove(v);
        }

        self.alias.insert(v.to_string(), u.to_string());
        self.coalesced_nodes.insert(v.to_string());
        if let Some(u_alias) = self.alias.get(u) {
            if u_alias == v {
                self.alias.remove(u);
                self.coalesced_nodes.remove(u);
            }
        }

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
        // 我们选择度数最大的节点。
        let mut max_deg = 0;
        let mut m = String::new();
        for n in self.spill_work_list.iter() {
            if !self.spill_temps.contains(n) && self.degree[n] > max_deg {
                max_deg = self.degree[n];
                m = n.clone();
            }
        }
        if !m.is_empty() {
            self.spill_work_list.remove(&m);
            self.simplify_work_list.insert(m.clone());
            self.freeze_moves(&m);
        }
    }
    fn assign_colors(&mut self) {
        while !self.select_stack.is_empty() {
            let n = self.select_stack.pop().unwrap();
            let mut ok_colors = self.phy_colors.clone();
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

                // 优先分配callee saved寄存器
                if !ok_colors.is_disjoint(&self.callee_saved_colors) {
                    ok_colors = ok_colors.intersection(&self.callee_saved_colors).cloned().collect();
                }
                let c = ok_colors.iter().next().unwrap().clone();

                self.color.insert(n.clone(), c);
            }
        }
        for n in self.coalesced_nodes.iter() {
            self.color.insert(n.clone(), self.color[&self.get_alias(n)].clone());
        }
    }
    fn rewrite_program(&mut self) {
        // 此时spill_nodes中即为实际溢出的节点，为每一个实际溢出节点分配一个存储单元
        let new_temps = self.cfg.insert_use_def(self.spilled_nodes.clone());
        self.spill_temps.extend(new_temps.clone());
        self.spill_vars.extend(self.spilled_nodes.clone());
        self.spilled_nodes.clear();
        // self.initial = self.colored_nodes.union(
        //     &self.coalesced_nodes.union(&new_temps).cloned().collect()
        // ).cloned().collect();
        self.initial = new_temps.clone();
        self.colored_nodes.clear();
        self.coalesced_nodes.clear();
    }

    pub fn get_ir(&mut self) -> Vec<IRNode> {
        let mut res = Vec::new();
        let mut callee_protect = HashSet::new();
        for name in self.cfg.names.iter() {
            for inst in &self.cfg.nodes[name].ch {
                callee_protect.extend(
                    inst.ir_.get_def().iter().map(|x| self.color[x].clone())
                );
            }
        }
        let callee_protect = callee_protect.intersection(&self.callee_saved_colors).cloned().collect::<Vec<_>>();
        res.push(IRNode::CalleeProtect(callee_protect.clone()));

        let mut caller_protect_map = HashMap::new();
        for name in self.cfg.names.iter() {
            for inst in &self.cfg.nodes[name].ch {
                match &inst.ir_ {
                    IRNode::Call(_, _, name, _) => {
                        let live_colors: HashSet<String> = inst.out_.iter().filter_map(
                            |x| {
                                if !self.pre_colored.contains(x) {
                                    Some(self.color[x].clone())
                                } else {
                                    None
                                }
                            }
                        ).collect();
                        let caller_protect = live_colors.intersection(&self.caller_saved_colors).cloned().collect::<Vec<_>>();
                        caller_protect_map.insert(name.clone(), caller_protect);
                    }
                    IRNode::Ret(_, _) => {
                        res.push(IRNode::CalleeRecover(callee_protect.clone()));
                    }
                    _ => {}
                }
                res.push(inst.ir_.clone())
            }
        }
        for ir in res.iter_mut() {
            match ir {
                IRNode::CallerProtect(name, regs) => {
                    *regs = caller_protect_map[name].clone();
                }
                IRNode::CallerRecover(name, regs) => {
                    *regs = caller_protect_map[name].clone();
                }
                _ => {}
            }
        }

        res
    }
    pub fn get_color(&self) -> HashMap<String, String> {
        self.color.clone()
    }
    pub fn get_spill_vars(&self) -> HashSet<String> {
        self.spill_vars.clone()
    }
}