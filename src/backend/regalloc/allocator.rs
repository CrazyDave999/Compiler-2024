use std::collections::{HashMap, HashSet, VecDeque};
use std::usize;
use bit_set::BitSet;
use crate::middleend::ir::{IRNode, IRType};
use super::utils::BasicBlock;
use super::utils::Instruction;
use super::utils::is_reg;

/*
color: 0,1,...,31
 */
pub struct Allocator {
    names: Vec<String>, // labels
    nodes: Vec<BasicBlock>, // bb
    virtual_regs: Vec<String>,
    virtual_rnk: HashMap<String, usize>,
    exit: BitSet,
    move_inst: Vec<Instruction>,
    phy_reg_names: Vec<String>,

    k: usize,
    phy_regs: BitSet,
    // caller_saved_regs: BitSet,
    // callee_saved_regs: BitSet,
    spill_temps: BitSet, // 为溢出变量创建的临时变量
    spill_vars: BitSet, // 溢出变量的名字

    phy_colors: BitSet,
    caller_saved_colors: BitSet,
    callee_saved_colors: BitSet,

    // node sets and work lists
    pre_colored: BitSet,
    initial: BitSet,
    simplify_work_list: BitSet,
    freeze_work_list: BitSet,
    spill_work_list: BitSet,
    spilled_nodes: BitSet,
    coalesced_nodes: BitSet,
    colored_nodes: BitSet,
    select_stack: Vec<usize>,

    // move inst sets
    coalesced_moves: BitSet,
    constrained_moves: BitSet,
    frozen_moves: BitSet,
    work_list_moves: BitSet,
    active_moves: BitSet,

    // other data structures
    adj_set: HashSet<(usize, usize)>,
    adj_list: Vec<BitSet>,
    degree: Vec<usize>,
    move_list: Vec<BitSet>,
    alias: Vec<usize>,
    color: Vec<usize>,

    num: usize,
}

impl Allocator {
    pub fn from(ir: Vec<IRNode>) -> Self {
        let mut res = Allocator {
            names: Vec::new(),
            nodes: Vec::new(),
            virtual_regs: Vec::new(),
            virtual_rnk: HashMap::new(),
            exit: BitSet::new(),
            move_inst: Vec::new(),
            phy_reg_names: Vec::from_iter([
                "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
            ].iter().map(|&x| x.to_string())),
            k: 27,
            phy_regs: BitSet::from_iter(
                5..32
            ),
            // caller_saved_regs: BitSet::from_iter(
            //     (5..8).chain(10..18).chain(28..32)
            // ),
            // callee_saved_regs: BitSet::from_iter(
            //     (8..10).chain(18..28)
            // ),
            phy_colors: BitSet::from_iter(
                5..32
            ),
            caller_saved_colors: BitSet::from_iter(
                (5..8).chain(10..18).chain(28..32)
            ),
            callee_saved_colors: BitSet::from_iter(
                (8..10).chain(18..28)
            ),
            spill_temps: BitSet::new(),
            spill_vars: BitSet::new(),
            pre_colored: BitSet::new(),
            initial: BitSet::new(),
            simplify_work_list: BitSet::new(),
            freeze_work_list: BitSet::new(),
            spill_work_list: BitSet::new(),
            spilled_nodes: BitSet::new(),
            coalesced_nodes: BitSet::new(),
            colored_nodes: BitSet::new(),
            select_stack: Vec::new(),
            coalesced_moves: BitSet::new(),
            constrained_moves: BitSet::new(),
            frozen_moves: BitSet::new(),
            work_list_moves: BitSet::new(),
            active_moves: BitSet::new(),
            adj_set: HashSet::new(),
            adj_list: Vec::new(),
            degree: Vec::new(),
            move_list: Vec::new(),
            alias: Vec::new(),
            color: Vec::new(),
            num: 0,
        };
        let mut bb = BasicBlock::new();
        let mut label_rnk = HashMap::new();
        label_rnk.insert("entry".to_string(), 0);
        res.names.push("entry".to_string());
        // 建立cfg
        for inst in ir.iter() {
            match inst {
                IRNode::Label(name) => {
                    let l = res.names.len();
                    res.names.push(name.clone());
                    label_rnk.insert(name.clone(), l);
                }
                _ => {}
            }
            bb.ch.push(Instruction::from(inst));
            if inst.is_terminator() {
                res.nodes.push(bb.clone());
                bb = BasicBlock::new();
            }
        }


        let mut cfg_edges = Vec::new();
        for (i, bb) in res.nodes.iter().enumerate() {
            match &bb.ch.last().unwrap().ir_ {
                IRNode::Br(label) => {
                    cfg_edges.push((i, label_rnk[label]));
                }
                IRNode::BrCond(_, label1, label2) => {
                    cfg_edges.push((i, label_rnk[label1]));
                    cfg_edges.push((i, label_rnk[label2]));
                }
                _ => {}
            }
        }
        for (u, v) in cfg_edges {
            res.nodes[u].succ.insert(v);
            res.nodes[v].pred.insert(u);
        }
        for (i, bb) in res.nodes.iter().enumerate() {
            if bb.succ.is_empty() {
                res.exit.insert(i);
            }
        }

        res.virtual_regs.extend(
            ["%zero", "%ra", "%sp", "%gp", "%tp", "%t0", "%t1", "%t2", "%s0", "%s1", "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%a6", "%a7", "%s2", "%s3", "%s4", "%s5", "%s6", "%s7", "%s8", "%s9", "%s10", "%s11", "%t3", "%t4", "%t5", "%t6"].into_iter().map(|x| x.to_string())
        );

        // 去除一些只有def没有use的寄存器对应的指令
        let mut changed = true;
        while changed {
            changed = false;
            let mut all_use = HashSet::new();
            for bb in res.nodes.iter() {
                for inst in bb.ch.iter() {
                    all_use.extend(inst.ir_.alloc_get_use().into_iter());
                }
            }
            for bb in res.nodes.iter_mut() {
                bb.ch.retain(|inst| {
                    let def = inst.ir_.alloc_get_def();
                    if def.is_empty() {
                        true
                    } else {
                        for x in def.iter() {
                            if all_use.contains(x) || res.virtual_regs.contains(x) {
                                return true;
                            }
                        }
                        changed = true;
                        false
                    }
                });
            }
        }


        // 给虚拟寄存器编号，给move指令编号

        for (i, reg) in res.virtual_regs.iter().enumerate() {
            res.virtual_rnk.insert(reg.clone(), i);
        }
        for bb in res.nodes.iter_mut() {
            for inst in bb.ch.iter_mut() {
                for reg in inst.ir_.alloc_get_use().iter() {
                    inst.use_.insert(*res.virtual_rnk.get(reg).unwrap_or(&0));
                }
                for reg in inst.ir_.alloc_get_def().iter() {
                    if !res.virtual_rnk.contains_key(reg) {
                        let l = res.virtual_regs.len();
                        res.virtual_regs.push(reg.clone());
                        res.virtual_rnk.insert(reg.clone(), l);
                    }
                    inst.def_.insert(res.virtual_rnk[reg]);
                }
                match &inst.ir_ {
                    IRNode::Move(_, _, _) => {
                        inst.idx = res.move_inst.len();
                        res.move_inst.push(inst.clone());
                    }
                    _ => {}
                }
            }
        }


        res.pre_colored = res.phy_regs.clone();

        for bb in res.nodes.iter() {
            for inst in bb.ch.iter() {
                res.initial.extend(inst.use_.iter());
                res.initial.extend(inst.use_.iter());
                res.initial.extend(inst.def_.iter());
            }
        }
        res.initial = res.initial.difference(&res.pre_colored).collect();

        // topological sort
        // let mut marked = BitSet::new();
        // let mut stk = Vec::new();
        // let mut stat = Vec::new();
        // stat.resize(res.nodes.len(), 0usize);
        // for i in

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
        self.adj_list.resize(self.virtual_regs.len(), BitSet::new());
        self.degree.resize(self.virtual_regs.len(), 0);
        self.move_list.resize(self.virtual_regs.len(), BitSet::new());
        self.alias.resize(self.virtual_regs.len(), usize::MAX);
        self.color.resize(self.virtual_regs.len(), usize::MAX);

        for reg in self.pre_colored.iter() {
            self.degree[reg] = usize::MAX;
            self.color[reg] = reg;
        }

        for reg in self.initial.iter() {
            self.degree[reg] = 0;
            self.adj_list[reg].clear();
        }
        self.num = self.initial.len();
    }

    pub fn main(&mut self) {
        // println!("#####main#####");
        self.reset();
        // println!("live_analysis");
        self.live_analysis();
        // println!("build");
        self.build();
        // println!("make_work_list");
        self.make_work_list();
        // println!("algorithms");
        while !self.simplify_work_list.is_empty() || !self.work_list_moves.is_empty() || !self.freeze_work_list.is_empty() || !self.spill_work_list.is_empty() {
            // self.check();
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
                // println!("select_spill");
                self.select_spill();
            }
        }
        // println!("assign_colors");
        self.assign_colors();
        if !self.spilled_nodes.is_empty() {
            // println!("rewrite_program");
            self.rewrite_program();
            self.main();
        }
    }
    // fn check(&self) {
    //     // 检查不变式的正确性
    //     // 所有工作表的节点数总和不变
    //     let sum = self.simplify_work_list.len() + self.freeze_work_list.len() + self.spill_work_list.len() + self.spilled_nodes.len() + self.coalesced_nodes.len() + self.colored_nodes.len() + self.select_stack.len();
    //     assert_eq!(sum, self.num);
    //     // 度的不变式
    //     let deg_check_list: BitSet = self.simplify_work_list.union(
    //         &self.freeze_work_list.union(
    //             &self.spill_work_list
    //         ).collect()
    //     ).collect();
    //     for u in deg_check_list.iter() {
    //         let deg = self.adj_list[u].intersection(
    //             &self.pre_colored.union(
    //                 &deg_check_list
    //             ).collect()
    //         ).count();
    //         assert_eq!(deg, self.degree[u]);
    //     }
    //     // 简化工作表的不变式
    //     for u in self.simplify_work_list.iter() {
    //         assert!(self.degree[u] < self.k);
    //         assert!(
    //             self.move_list[u].is_disjoint(
    //                 &self.active_moves.union(
    //                     &self.work_list_moves
    //                 ).collect()
    //             )
    //         );
    //     }
    //     // 冻结工作表的不变式
    //     for u in self.freeze_work_list.iter() {
    //         assert!(self.degree[u] < self.k);
    //         assert!(
    //             !self.move_list[u].is_disjoint(
    //                 &self.active_moves.union(
    //                     &self.work_list_moves
    //                 ).collect()
    //             )
    //         );
    //     }
    //     // 溢出工作表的不变式
    //     for u in self.spill_work_list.iter() {
    //         assert!(self.degree[u] >= self.k);
    //     }
    // }
    fn live_analysis(&mut self) {
        // 先计算各个基本块的等效use,def
        for bb in self.nodes.iter_mut() {
            bb.use_.clear();
            bb.def_.clear();
            bb.in_.clear();
            bb.out_.clear();
            for inst in bb.ch.iter() {
                bb.use_.extend(inst.use_.difference(&bb.def_).into_iter());
                bb.def_.extend(inst.def_.iter());
            }
        }
        let mut changed = true;
        // let mut iter = 0;
        while changed {
            changed = false;
            let mut queue = VecDeque::new();
            let mut visited = BitSet::new();
            queue.extend(self.exit.iter());
            while !queue.is_empty() {
                let cur = queue.pop_front().unwrap();
                if visited.contains(cur) {
                    continue;
                }

                let use_ = &self.nodes[cur].use_.clone();
                let def_ = &self.nodes[cur].def_.clone();

                let succ = &self.nodes[cur].succ;
                let mut new_out_ = BitSet::new();
                for node in succ.iter() {
                    new_out_.extend(self.nodes[node].in_.iter());
                }
                if new_out_ != self.nodes[cur].out_ {
                    changed = true;
                    self.nodes[cur].out_ = new_out_;
                }

                let out_ = &self.nodes[cur].out_;
                let new_in_: BitSet = use_.union(&out_.difference(def_).collect()).collect();
                if new_in_ != self.nodes[cur].in_ {
                    changed = true;
                    // println!("iter {} failed. {} 's in_ changed to {:?}", iter, self.names[cur], new_in_);
                    self.nodes[cur].in_ = new_in_;
                }

                if !visited.contains(cur) {
                    for node in self.nodes[cur].pred.iter() {
                        if !visited.contains(node) {
                            queue.push_back(node);
                        }
                    }
                }
                visited.insert(cur);
            }
            // println!("iter: {} ok", iter);
            // iter += 1;
        }
    }

    // 建图，顺便记录每条指令的in_和out_
    fn build(&mut self) {
        let mut edges = Vec::new();
        for bb in self.nodes.iter_mut() {
            let mut live = bb.out_.clone();
            for inst in bb.ch.iter_mut().rev() {
                match &inst.ir_ {
                    IRNode::Move(_, _, rs) => {
                        if is_reg(rs) {
                            live.remove(*self.virtual_rnk.get(rs).unwrap_or(&0));
                            for n in inst.use_.union(&inst.def_) {
                                self.move_list[n].insert(inst.idx);
                            }
                            self.work_list_moves.insert(inst.idx);
                        }
                    }
                    _ => {}
                }
                inst.out_ = live.clone();
                live.extend(inst.def_.iter());
                for d in inst.def_.iter() {
                    for l in live.iter() {
                        if l != d {
                            edges.push((l, d));
                        }
                    }
                }
                live = inst.use_.union(&live.difference(&inst.def_).collect()).collect();
                inst.in_ = live.clone();
            }
        }
        for (u, v) in edges {
            self.add_edge(u, v);
        }
    }

    fn add_edge(&mut self, u: usize, v: usize) {
        if !self.adj_set.contains(&(u, v)) {
            self.adj_set.insert((u, v));
            self.adj_set.insert((v, u));
            if !self.pre_colored.contains(u) {
                self.adj_list[u].insert(v);
                self.degree[u] += 1;
            }
            if !self.pre_colored.contains(v) {
                self.adj_list[v].insert(u);
                self.degree[v] += 1;
            }
        }
    }

    fn make_work_list(&mut self) {
        for n in self.initial.iter() {
            if self.degree[n] >= self.k {
                self.spill_work_list.insert(n);
            } else if self.move_related(n) {
                self.freeze_work_list.insert(n);
            } else {
                self.simplify_work_list.insert(n);
            }
        }
        self.initial.clear();
    }
    fn adjacent(&self, n: usize) -> BitSet {
        self.adj_list[n].difference(
            &BitSet::from_iter(self.select_stack.iter().cloned()).union(
                &self.coalesced_nodes
            ).collect()
        ).collect()
    }
    fn node_moves(&self, n: usize) -> BitSet {
        self.move_list[n].intersection(
            &self.active_moves.union(&self.work_list_moves).collect()
        ).collect()
    }
    fn move_related(&self, n: usize) -> bool {
        !self.move_list[n].is_disjoint(&self.active_moves) || !self.move_list[n].is_disjoint(&self.work_list_moves)
    }

    fn simplify(&mut self) {
        let n = self.simplify_work_list.iter().next().unwrap();
        self.simplify_work_list.remove(n);
        self.select_stack.push(n);
        for m in self.adjacent(n).iter() {
            self.decrement_degree(m);
        }
    }
    fn decrement_degree(&mut self, m: usize) {
        let d = self.degree[m];
        if !self.pre_colored.contains(m) {
            self.degree[m] = d - 1;
        }
        if d == self.k {
            self.enable_moves(
                self.adjacent(m).union(
                    &BitSet::from_iter([m].into_iter())
                ).collect()
            );
            self.spill_work_list.remove(m);
            if self.move_related(m) {
                self.freeze_work_list.insert(m);
            } else {
                self.simplify_work_list.insert(m);
            }
        }
    }
    fn enable_moves(&mut self, nodes: BitSet) {
        for n in nodes.iter() {
            for m in self.node_moves(n).iter() {
                if self.active_moves.contains(m) {
                    self.active_moves.remove(m);
                    self.work_list_moves.insert(m);
                }
            }
        }
    }

    fn coalesce(&mut self) {
        let m = self.work_list_moves.iter().next().unwrap().clone();
        let (x, y) = (
            self.move_inst[m].def_.iter().next().unwrap(),
            self.move_inst[m].use_.iter().next().unwrap()
        );
        let (mut u, mut v) = if self.pre_colored.contains(y) {
            (y, x)
        } else {
            (x, y)
        };
        self.work_list_moves.remove(m);

        u = self.get_alias(u);
        v = self.get_alias(v);
        if u == v {
            self.coalesced_moves.insert(m);
            self.add_work_list(u);
        } else if self.pre_colored.contains(v) || self.adj_set.contains(&(u, v)) {
            self.constrained_moves.insert(m.clone());
            self.add_work_list(u);
            self.add_work_list(v);
        } else if (self.pre_colored.contains(u) && self.adjacent(v).iter().all(|t| self.ok(t, u)))
            || (!self.pre_colored.contains(u) && self.conservative(self.adjacent(u).union(&self.adjacent(v)).collect())
        ) {
            self.coalesced_moves.insert(m);
            self.combine(u, v);
            self.add_work_list(u);
        } else {
            self.active_moves.insert(m);
        }
    }
    fn add_work_list(&mut self, u: usize) {
        if !self.pre_colored.contains(u) && !self.move_related(u) && self.degree[u] < self.k {
            self.freeze_work_list.remove(u);
            self.simplify_work_list.insert(u);
        }
    }
    fn ok(&self, t: usize, r: usize) -> bool {
        self.degree[t] < self.k || self.pre_colored.contains(t) || self.adj_set.contains(&(t, r))
    }
    fn conservative(&self, nodes: BitSet) -> bool {
        let mut k = 0;
        for n in nodes.iter() {
            if self.degree[n] >= self.k {
                k += 1;
            }
        }
        k < self.k
    }
    fn get_alias(&self, n: usize) -> usize {
        if self.coalesced_nodes.contains(n) {
            self.get_alias(self.alias[n])
        } else {
            n
        }
    }
    // u <- v
    fn combine(&mut self, u: usize, v: usize) {
        if self.freeze_work_list.contains(v) {
            self.freeze_work_list.remove(v);
        } else {
            self.spill_work_list.remove(v);
        }
        self.coalesced_nodes.insert(v);
        self.alias[v] = u;

        let move_list_v = self.move_list[v].clone();
        self.move_list[u].extend(move_list_v.into_iter());
        self.enable_moves([v].into_iter().collect());
        for t in self.adjacent(v).iter() {
            self.add_edge(t, u);
            self.decrement_degree(t);
        }
        if self.degree[u] >= self.k && self.freeze_work_list.contains(u) {
            self.freeze_work_list.remove(u);
            self.spill_work_list.insert(u);
        }
    }

    fn freeze(&mut self) {
        let u = self.freeze_work_list.iter().next().unwrap().clone();
        self.freeze_work_list.remove(u);
        self.simplify_work_list.insert(u);
        self.freeze_moves(u);
    }
    fn freeze_moves(&mut self, u: usize) {
        for m in self.node_moves(u).iter() {
            let (x, y) = (
                self.move_inst[m].def_.iter().next().unwrap(),
                self.move_inst[m].use_.iter().next().unwrap()
            );
            let v = if self.get_alias(y) == self.get_alias(u) {
                self.get_alias(x)
            } else {
                self.get_alias(y)
            };
            self.active_moves.remove(m);
            self.frozen_moves.insert(m);
            if self.node_moves(v).is_empty() && self.degree[v] < self.k {
                self.freeze_work_list.remove(v);
                self.simplify_work_list.insert(v);
            }
        }
    }
    fn select_spill(&mut self) {
        // 启发式地寻找一个高度数节点，让它入栈，从而被溢出。
        // 我们选择度数最大的节点。
        let mut max_deg = 0;
        let mut m = usize::MAX;
        for n in self.spill_work_list.iter() {
            if !self.spill_temps.contains(n) && self.degree[n] > max_deg {
                max_deg = self.degree[n];
                m = n;
            }
        }
        if m == usize::MAX {
            m = self.spill_work_list.iter().next().unwrap().clone();
        }
        self.spill_work_list.remove(m);
        self.simplify_work_list.insert(m);
        self.freeze_moves(m);
    }
    fn assign_colors(&mut self) {
        while !self.select_stack.is_empty() {
            let n = self.select_stack.pop().unwrap();
            let mut ok_colors = self.phy_colors.clone();
            for w in self.adj_list[n].iter() {
                let alias_w = self.get_alias(w);
                if self.colored_nodes.contains(alias_w) || self.pre_colored.contains(alias_w) {
                    ok_colors.remove(self.color[alias_w]);
                }
            }
            if ok_colors.is_empty() {
                self.spilled_nodes.insert(n);
            } else {
                self.colored_nodes.insert(n);

                // 优先分配callee saved寄存器
                if !ok_colors.is_disjoint(&self.callee_saved_colors) {
                    ok_colors = ok_colors.intersection(&self.callee_saved_colors).collect();
                }
                let c = ok_colors.iter().next().unwrap();

                self.color[n] = c;
            }
        }
        for n in self.coalesced_nodes.iter() {
            self.color[n] = self.color[self.get_alias(n)];
        }
    }
    fn rewrite_program(&mut self) {
        // 此时spill_nodes中即为实际溢出的节点，为每一个实际溢出节点分配一个存储单元
        let new_temps = self.insert_use_def(self.spilled_nodes.clone());
        self.spill_temps.extend(new_temps.iter());
        self.spill_vars.extend(self.spilled_nodes.iter());
        self.spilled_nodes.clear();
        self.initial = self.colored_nodes.union(
            &self.coalesced_nodes.union(&new_temps).collect()
        ).collect();
        self.colored_nodes.clear();
        self.coalesced_nodes.clear();
    }
    fn insert_use_def(&mut self, spill_nodes: BitSet) -> BitSet {
        let mut spill_temps = BitSet::new();
        for node in spill_nodes.iter() {
            let mut spill_cnt = 0;
            for bb in self.nodes.iter_mut() {
                // 对溢出变量，将其use和def都替换成生命周期很短的临时变量v
                let mut spill_store = Vec::new();
                for (i, inst) in bb.ch.iter_mut().enumerate() {
                    if inst.def_.contains(node) {
                        let spill_name = format!("%spill.{}.{}", node, spill_cnt);
                        spill_cnt += 1;

                        let tmp = self.virtual_regs.len();
                        self.virtual_regs.push(spill_name.clone());
                        self.virtual_rnk.insert(spill_name.clone(), tmp);

                        spill_temps.insert(tmp);
                        inst.def_.remove(node);
                        inst.def_.insert(tmp);

                        let node_name = self.virtual_regs[node].clone();
                        for def_ in inst.ir_.alloc_get_def_mut() {
                            if *def_ == node_name {
                                *def_ = spill_name.clone();
                            }
                        }

                        let mut new_inst = Instruction::from(&IRNode::SpillStore(
                            // inst.ir_.get_ir_type(&spill_name).clone(),
                            IRType::i32(),
                            spill_name.clone(),
                            node_name.clone(),
                        ));
                        new_inst.use_.insert(tmp);

                        spill_store.push((
                            i,
                            new_inst
                        ))
                    }
                }
                for (i, inst) in spill_store.into_iter().rev() {
                    bb.ch.insert(i + 1, inst);
                }

                let mut spill_load = Vec::new();
                for (i, inst) in bb.ch.iter_mut().enumerate() {
                    if inst.use_.contains(node) {
                        let spill_name = format!("%spill.{}.{}", node, spill_cnt);
                        spill_cnt += 1;

                        let tmp = self.virtual_regs.len();
                        self.virtual_regs.push(spill_name.clone());
                        self.virtual_rnk.insert(spill_name.clone(), tmp);

                        spill_temps.insert(tmp);
                        inst.use_.remove(node);
                        inst.use_.insert(tmp);

                        let node_name = self.virtual_regs[node].clone();
                        for use_ in inst.ir_.alloc_get_use_mut() {
                            if *use_ == node_name {
                                *use_ = spill_name.clone();
                            }
                        }

                        let mut new_inst = Instruction::from(&IRNode::SpillLoad(
                            // inst.ir_.get_ir_type(&spill_name).clone(),
                            IRType::i32(),
                            spill_name.clone(),
                            node_name.clone(),
                        ));
                        new_inst.def_.insert(tmp);

                        spill_load.push((
                            i,
                            new_inst
                        ))
                    }
                }
                for (i, inst) in spill_load.into_iter().rev() {
                    bb.ch.insert(i, inst);
                }
            }
        }
        spill_temps
    }

    pub fn get_ir(&mut self) -> Vec<IRNode> {
        let mut res = Vec::new();
        let mut callee_protect = BitSet::new();
        for node in self.nodes.iter() {
            for inst in node.ch.iter() {
                callee_protect.extend(
                    inst.def_.iter().map(|x| {
                        if self.color[x] != usize::MAX {
                            self.color[x].clone()
                        } else {
                            0
                        }
                    })
                )
            }
        }
        let callee_protect = callee_protect.intersection(&self.callee_saved_colors)
            .map(|x| self.phy_reg_names[x].clone()).collect::<Vec<_>>();
        res.push(IRNode::CalleeProtect(callee_protect.clone()));

        let mut caller_protect_map = Vec::new();

        for node in self.nodes.iter() {
            for inst in node.ch.iter() {
                match &inst.ir_ {
                    IRNode::Call(_, _, _, _) => {
                        let live_colors: BitSet = inst.out_.iter().filter_map(
                            |x| {
                                if self.color[x] != usize::MAX && !self.pre_colored.contains(x) {
                                    Some(self.color[x].clone())
                                } else {
                                    None
                                }
                            }
                        ).collect();
                        let caller_protect = live_colors.intersection(&self.caller_saved_colors)
                            .map(|x| self.phy_reg_names[x].clone()).collect::<Vec<_>>();
                        caller_protect_map.push(caller_protect);
                    }
                    IRNode::Ret(_, _) => {
                        res.push(IRNode::CalleeRecover(callee_protect.clone()));
                    }
                    _ => {}
                }
                res.push(inst.ir_.clone())
            }
        }
        let mut call_cnt = 0;
        for ir in res.iter_mut() {
            match ir {
                IRNode::CallerProtect(_, regs) => {
                    *regs = caller_protect_map[call_cnt].clone();
                }
                IRNode::CallerRecover(_, regs) => {
                    *regs = caller_protect_map[call_cnt].clone();
                    call_cnt += 1;
                }
                _ => {}
            }
        }

        res
    }
    pub fn get_color(&self) -> HashMap<String, String> {
        let mut res: HashMap<String, String> = HashMap::new();
        for (k, v) in self.virtual_rnk.iter() {
            if self.color[*v] != usize::MAX {
                res.insert(k.clone(), self.phy_reg_names[self.color[*v]].clone());
            }
        }
        res
    }
    pub fn get_spill_vars(&self) -> HashSet<String> {
        let mut res = HashSet::new();
        for n in self.spill_vars.iter() {
            res.insert(self.virtual_regs[n].clone());
        }
        res
    }
}