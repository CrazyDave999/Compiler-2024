// 考虑使用全局值标号GVN来实现公共子表达式消除CSE
// 考虑在GVN之后实现全局代码移动GCM

use super::IRNode;
use bit_set::BitSet;
use std::collections::{HashMap, VecDeque};

struct Optimizer {
    ir: Vec<IRNode>,
    dom: Vec<BitSet>,
    dom_pred: Vec<usize>, // 支配树，指向父亲
    dom_succ: Vec<BitSet>,
    dom_depth: Vec<usize>,           // 每个块在支配树中的深度
    loop_nest: Vec<usize>,           // 每个块在循环嵌套中的深度
    pred: Vec<BitSet>,               // CFG
    succ: Vec<BitSet>,               // CFG
    cfg_rnk: HashMap<String, usize>, // CFG
    ch: Vec<BitSet>,                 // 块中指令

    def_pos: HashMap<String, Box<BitSet>>,
    use_pos: HashMap<String, Box<BitSet>>,

    block: Vec<usize>,  // 每个指令在schedule_early中的结果位置
    visited: Vec<bool>, // 每个指令否被访问过
    pinned: Vec<bool>,  // 每个指令是否是pinned

    phi_alias: Vec<usize>,
}
impl Optimizer {
    pub fn new() -> Self {
        Self {
            ir: Vec::new(),
            dom: Vec::new(),
            dom_pred: Vec::new(),
            dom_succ: Vec::new(),
            def_pos: HashMap::new(),
            use_pos: HashMap::new(),
            block: Vec::new(),
            dom_depth: Vec::new(),
            loop_nest: Vec::new(),
            pred: Vec::new(),
            succ: Vec::new(),
            cfg_rnk: HashMap::new(),
            visited: Vec::new(),
            pinned: Vec::new(),
            ch: Vec::new(),
            phi_alias: Vec::new(),
        }
    }

    pub fn from(ir: Vec<IRNode>) -> Self {
        let mut opt = Self::new();
        let mut name = String::from("entry");
        let mut edges = Vec::new();

        opt.block.resize(ir.len(), usize::MAX);
        opt.pinned.resize(ir.len(), false);

        for (i, inst) in ir.iter().enumerate() {
            if let IRNode::Label(n) = inst {
                name = n.clone();
            }
            if name.is_empty() {
                continue;
            }

            for use_ in inst.get_use().iter() {
                opt.def_pos
                    .entry(use_.clone())
                    .or_insert(Box::new(BitSet::new()));
                opt.use_pos
                    .entry(use_.clone())
                    .or_insert(Box::new(BitSet::new()))
                    .insert(i);
            }
            for def in inst.get_def().iter() {
                opt.def_pos
                    .entry(def.clone())
                    .or_insert(Box::new(BitSet::new()))
                    .insert(i);
                opt.use_pos
                    .entry(def.clone())
                    .or_insert(Box::new(BitSet::new()));
            }
            let rnk = opt.cfg_rnk.len();
            opt.block[i] = rnk;
            opt.pinned[i] = inst.is_pinned();

            if inst.is_terminator() {
                match inst {
                    IRNode::Br(label) => {
                        edges.push((name.clone(), label.clone()));
                    }
                    IRNode::BrCond(_, label1, label2) => {
                        edges.push((name.clone(), label1.clone()));
                        edges.push((name.clone(), label2.clone()));
                    }
                    _ => {}
                }

                opt.cfg_rnk.insert(name.clone(), rnk);
                opt.ch.push(BitSet::new());

                name = String::new();
            }
        }
        opt.succ.resize(opt.cfg_rnk.len(), BitSet::new());
        opt.pred.resize(opt.cfg_rnk.len(), BitSet::new());

        for (u, v) in edges.iter() {
            let (u, v) = (opt.cfg_rnk[u], opt.cfg_rnk[v]);
            opt.succ[u].insert(v);
            opt.pred[v].insert(u);
        }

        opt.phi_alias.resize(ir.len(), usize::MAX);
        for def_pos in opt.def_pos.values() {
            if def_pos.len() > 1 {
                let mut succ = BitSet::from_iter(0..opt.cfg_rnk.len());
                for def_ in def_pos.iter() {
                    succ.intersect_with(
                        &opt.succ[opt.block[def_]]
                    );
                }
                let w = succ.iter().next().unwrap();
                for def_ in def_pos.iter() {
                    opt.phi_alias[def_] = w;
                }
            }
        }
        opt.ir = ir;
        opt
    }
    pub fn main(&mut self) {
        self.build_dom();
        self.calc_nest();
        self.gvn();
        self.gcm();
        for i in 0..self.ir.len() {
            self.ch[self.block[i]].insert(i);
        }
    }
    fn build_dom(&mut self) {
        let len = self.cfg_rnk.len();
        self.dom.resize(len, BitSet::from_iter(0..len));

        let mut changed = true;
        while changed {
            changed = false;
            let mut queue = VecDeque::new();
            let mut visited = BitSet::new();
            queue.push_back(0usize);
            while !queue.is_empty() {
                let cur = queue.pop_front().unwrap();
                if visited.contains(cur) {
                    continue;
                }
                visited.insert(cur);
                queue.extend(self.succ[cur].iter().filter(|x| !visited.contains(*x)));
                let preds = &self.pred[cur];
                let mut new_dom = BitSet::from_iter(0..len);
                if preds.is_empty() {
                    new_dom.clear();
                } else {
                    for pred in preds.iter() {
                        new_dom.intersect_with(&self.dom[pred]);
                    }
                }
                new_dom.insert(cur);
                if new_dom != self.dom[cur] {
                    self.dom[cur] = new_dom;
                    changed = true;
                }
            }
        }
        self.dom_pred.resize(len, usize::MAX);
        self.dom_succ.resize(len, BitSet::new());
        self.dom_depth.resize(len, 0);
        for n in 0..self.cfg_rnk.len() {
            let n_dom_num = self.dom[n].len();
            let i_dom_of_n = self.dom[n]
                .iter()
                .find(|x| self.dom[*x].len() == n_dom_num - 1);
            if let Some(i_dom_of_n) = i_dom_of_n {
                self.dom_pred[n] = i_dom_of_n;
                self.dom_succ[i_dom_of_n].insert(n);
                self.dom_depth[n] = self.dom_depth[i_dom_of_n] + 1;
            }
        }
    }
    fn calc_nest(&mut self) {
        self.loop_nest.resize(self.cfg_rnk.len(), 0);
        for i in 0..self.cfg_rnk.len() {
            for j in self.succ[i].iter() {
                if self.dom[i].contains(j) {
                    // i -> j 是一条回边
                    let mut queue = VecDeque::new();
                    queue.push_back(i);
                    let mut visited = BitSet::new();
                    while !queue.is_empty() {
                        let cur = queue.pop_front().unwrap();
                        if visited.contains(cur) {
                            continue;
                        }
                        self.loop_nest[cur] += 1;
                        if cur == j {
                            break;
                        }
                        visited.insert(cur);
                        queue.extend(self.pred[cur].iter());
                    }
                }
            }
        }
    }
    fn inputs(&self, id: usize) -> BitSet {
        let mut res = BitSet::new();
        match &self.ir[id] {
            IRNode::Move(_, rd, _) => {
                for def_ in self.def_pos.get(rd).unwrap().iter() {
                    for use_ in self.ir[def_].get_use() {
                        res.union_with(self.def_pos.get(&use_).unwrap());
                    }
                }
            }
            _ => {
                for use_ in self.ir[id].get_use() {
                    res.union_with(self.def_pos.get(&use_).unwrap());
                }
            }
        }
        res
    }
    fn users(&self, id: usize) -> BitSet {
        let mut res = BitSet::new();
        for def_ in self.ir[id].get_def().iter() {
            res.union_with(self.use_pos.get(def_).unwrap());
        }
        res
    }
    fn gvn(&mut self) {}
    fn gcm(&mut self) {
        self.visited.resize(self.ir.len(), false);
        for i in 0..self.ir.len() {
            self.visited[i] = self.pinned[i];
        }
        for i in 0..self.ir.len() {
            if self.pinned[i] {
                for x in self.inputs(i).iter() {
                    self.schedule_early(x);
                }
            }
        }
        for i in 0..self.ir.len() {
            self.visited[i] = self.pinned[i];
        }
        for i in 0..self.ir.len() {
            if self.pinned[i] {
                for x in self.users(i).iter() {
                    self.schedule_late(x);
                }
            }
        }
    }
    fn get_block(&self, id: usize) -> usize {
        if self.phi_alias[id] != usize::MAX {
            self.phi_alias[id]
        } else {
            self.block[id]
        }
    }

    // id代表当前的ir指令，为这条指令找到最早的位置
    fn schedule_early(&mut self, id: usize) {
        if self.visited[id] {
            return;
        }
        self.visited[id] = true;
        self.block[id] = 0;
        for x in self.inputs(id).iter() {
            self.schedule_early(x);
            if self.dom_depth[self.block[id]] < self.dom_depth[self.get_block(x)] {
                self.block[id] = self.get_block(x);
            }
        }
    }
    fn schedule_late(&mut self, id: usize) {
        if self.visited[id] {
            return;
        }
        self.visited[id] = true;
        let mut lca = usize::MAX;
        for y in self.users(id).iter() {
            self.schedule_late(y);
            let use_ = self.block[y];
            lca = self.find_lca(lca, use_);
        }
        if lca == usize::MAX {
            lca = self.block[id];
        }
        let mut best = lca;
        while lca != self.block[id] {
            lca = self.dom_pred[lca];
            if self.loop_nest[lca] < self.loop_nest[best] {
                best = lca;
            }
        }
        self.block[id] = best;
    }
    fn find_lca(&self, mut a: usize, mut b: usize) -> usize {
        if a == usize::MAX {
            return b;
        }
        while self.dom_depth[a] > self.dom_depth[b] {
            a = self.dom_pred[a];
        }
        while self.dom_depth[b] > self.dom_depth[a] {
            b = self.dom_pred[b];
        }
        while a != b {
            a = self.dom_pred[a];
            b = self.dom_pred[b];
        }
        a
    }
    pub fn get_ir(&self) -> Vec<IRNode> {
        let mut res = Vec::new();
        for ir in self.ch.iter() {
            let mut terminator = usize::MAX;
            for i in ir.iter() {
                if self.ir[i].is_terminator() {
                    terminator = i;
                } else {
                    res.push(self.ir[i].clone());
                }
            }
            res.push(self.ir[terminator].clone());
        }
        res
    }
}
pub fn pass(ir: Vec<IRNode>) -> Vec<IRNode> {
    ir
    // let mut res = Vec::new();
    // let mut in_func = false;
    // let mut func_inner = Vec::new();
    // for node in ir.iter() {
    //     match node {
    //         IRNode::FuncBegin(_, _, _) => {
    //             res.push(node.clone());
    //             in_func = true;
    //         }
    //         IRNode::FuncEnd => {
    //             let mut opt = Optimizer::from(func_inner.clone());
    //             opt.main();
    //             res.extend(opt.get_ir());
    //             func_inner.clear();
    //             in_func = false;
    //             res.push(node.clone());
    //         }
    //         _ => {
    //             if in_func {
    //                 func_inner.push(node.clone());
    //             } else {
    //                 res.push(node.clone());
    //             }
    //         }
    //     }
    // }
    // res
}
