use std::collections::{HashMap, HashSet, VecDeque};
use bit_set::BitSet;
use crate::middleend::ir::{IRNode, IRType};

#[derive(Clone)]
pub struct BasicBlock {
    pub name: String,
    pub succ: BitSet,
    pub ir: Vec<IRNode>,
    pub phi: HashMap<String, IRNode>, // var name -> phi inst
    pub phi_names: Vec<String>,
    pub dom: BitSet,
    pub i_dom: BitSet, // 被自己直接支配的节点
    pub pred: BitSet,
    pub df: BitSet,
    pub allocate: Vec<(String, IRType)>,
    pub store: HashSet<String>,
    pub mv: Vec<IRNode>,
}
impl BasicBlock {
    pub fn new() -> Self {
        BasicBlock {
            name: String::from(""),
            succ: BitSet::new(),
            ir: Vec::new(),
            phi: HashMap::new(),
            phi_names: Vec::new(),
            dom: BitSet::new(),
            i_dom: BitSet::new(),
            pred: BitSet::new(),
            df: BitSet::new(),
            allocate: Vec::new(),
            store: HashSet::new(),
            mv: Vec::new(),
        }
    }
}


pub struct CFG {
    pub nodes: Vec<BasicBlock>,
    pub rnk: HashMap<String, usize>,
    pub allocated_vars: HashSet<String>,
}

impl CFG {
    pub fn from(ir: Vec<IRNode>) -> Self {
        let mut cfg = CFG {
            nodes: Vec::new(),
            rnk: HashMap::new(),
            allocated_vars: HashSet::new(),
        };
        let mut bb = BasicBlock::new();
        let mut name = String::from("entry");
        bb.name = name.clone();
        let mut edges = Vec::new();
        for inst in ir.iter() {
            if let IRNode::Label(n) = inst {
                name = n.clone();
                bb.name = name.clone();
            }
            if name == "" {
                continue;
            }
            bb.ir.push(inst.clone());
            match inst {
                IRNode::Allocate(name, ty) => {
                    bb.allocate.push((name.clone(), ty.clone()));
                }
                IRNode::Store(_, _, ptr) => {
                    bb.store.insert(ptr.clone());
                }
                _ => {}
            }
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
                let rnk = cfg.nodes.len();
                cfg.rnk.insert(name.clone(), rnk);
                cfg.nodes.push(bb.clone());
                name = String::from("");
                bb = BasicBlock::new();
            }
        }
        for (u, v) in edges.iter() {
            let (u, v) = (cfg.rnk[u], cfg.rnk[v]);
            cfg.nodes[u].succ.insert(v);
            cfg.nodes[v].pred.insert(u);
        }

        cfg
    }
    pub fn build_dt(&mut self) {
        // step 1: calculate the dominator set(Dom(n))
        let len = self.nodes.len();
        for bb in self.nodes.iter_mut() {
            bb.dom.extend(0..len);
        }
        let mut changed = true;
        // let mut iter = 0;
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
                queue.extend(self.nodes[cur].succ.iter().filter(|x| !visited.contains(*x)));
                let preds = &self.nodes[cur].pred;
                let mut new_dom = BitSet::from_iter(0..self.nodes.len());
                if preds.is_empty() {
                    new_dom.clear();
                } else {
                    for pred in preds.iter() {
                        new_dom.intersect_with(&self.nodes[pred].dom);
                    }
                }

                new_dom.insert(cur);
                if new_dom != self.nodes[cur].dom {
                    changed = true;
                    self.nodes[cur].dom = new_dom;
                }
                // println!("iter: {}, queue size: {}", iter, queue.len());
            }
            // println!("iter: {}", iter);
            // iter += 1;
        }

        // step 2: calculate the immediate dominator(IDom(n).i_dom contains n)
        for n in 0..self.nodes.len() {
            let n_dom_num = self.nodes[n].dom.len();
            let i_dom_of_n = self.nodes[n].dom.iter().find(|&x| self.nodes[x].dom.len() == n_dom_num - 1);
            if let Some(i_dom_of_n) = i_dom_of_n {
                self.nodes[i_dom_of_n].i_dom.insert(n);
            }
        }

        // step 3: calculate the dominance frontier(DF(n))
        for n in 0..self.nodes.len() {
            let mut update_names = BitSet::new();
            let preds = &self.nodes[n].pred;
            for m in preds.iter() {
                update_names.union_with(
                    &self.nodes[m].dom.difference(
                        &self.nodes[n].dom.difference(
                            &BitSet::from_iter([n])
                        ).collect()
                    ).collect()
                );
            }
            for x in update_names.iter() {
                self.nodes[x].df.insert(n);
            }
        }
    }
    pub fn put_phi(&mut self) {
        let mut allocated: Vec<(String, IRType)> = Vec::new();
        for bb in self.nodes.iter() {
            allocated.extend(bb.allocate.iter().cloned());
        }
        let mut phi_cnt = 0;
        let mut phi_name = || {
            let name = format!("%p{}", phi_cnt);
            phi_cnt += 1;
            name
        };
        for (var, ty) in allocated.iter() {
            // for every allocated var, first collect all bbs that contain its def
            let mut phi_queue = VecDeque::new();
            for (i, bb) in self.nodes.iter().enumerate() {
                if bb.store.contains(var) {
                    phi_queue.push_back(i);
                }
            }
            // for every bb, reserve phi inst in its df
            while !phi_queue.is_empty() {
                let cur = phi_queue.pop_front().unwrap();
                let df = &self.nodes[cur].df;
                let updated = df.iter().filter(
                    |&x| !self.nodes[x].phi.contains_key(var)
                ).collect::<Vec<_>>();
                for n in updated.iter() {
                    self.nodes[*n].phi.insert(
                        var.clone(), IRNode::Phi(phi_name(), ty.clone(), vec![]),
                    );
                    self.nodes[*n].phi_names.push(var.clone());
                    phi_queue.push_back(*n);
                }
            }
        }
        // dfs write phi: write back the parameters of phi instructions
        let mut rep = HashMap::new();
        for (var, _) in allocated.iter() {
            let mut bb_stk = Vec::new();
            let mut bb_stat = Vec::new();
            bb_stat.resize(self.nodes.len(), 0usize);
            let mut stk = Vec::new();
            let mut cur_stk_len = Vec::new();
            cur_stk_len.resize(self.nodes.len(), 0usize);

            bb_stk.push(0usize);
            while !bb_stk.is_empty() {
                let cur = bb_stk.last().unwrap().clone();
                let cur_name = &self.nodes[cur].name.clone();
                if bb_stat[cur] == 0 {
                    bb_stat[cur] = 1;
                    cur_stk_len[cur] = stk.len();
                    if let Some(IRNode::Phi(res, _, _)) = self.nodes[cur].phi.get(var) {
                        stk.push(res.clone());
                    }
                    for inst in self.nodes[cur].ir.iter() {
                        match inst {
                            IRNode::Load(res, _, ptr) => {
                                if ptr == var {
                                    rep.insert(res.clone(), stk.last().unwrap_or(&String::from("null")).clone());
                                }
                            }
                            IRNode::Store(_, val, ptr) => {
                                if ptr == var {
                                    stk.push(val.clone());
                                }
                            }
                            _ => {}
                        }
                    }
                    for succ in self.nodes[cur].succ.clone().iter() {
                        if let Some(IRNode::Phi(_, _, args)) = self.nodes[succ].phi.get_mut(var) {
                            if let Some(top) = stk.last() {
                                args.push((top.clone(), cur_name.clone()));
                            }
                        }
                    }
                    for succ in self.nodes[cur].i_dom.iter() {
                        if bb_stat[succ] == 0 {
                            bb_stk.push(succ);
                        }
                    }
                } else {
                    bb_stk.pop();
                    bb_stat[cur] = 2;
                    stk.truncate(cur_stk_len[cur]);
                }
            }
        }
        // rename
        let mut changed = true;
        while changed {
            changed = false;
            for bb in self.nodes.iter_mut() {
                for inst in bb.ir.iter_mut() {
                    match inst {
                        IRNode::Binary(_, _, _, lhs, rhs) => {
                            if let Some(new_val) = rep.get(lhs) {
                                *lhs = new_val.clone();
                                changed = true;
                            }
                            if let Some(new_val) = rep.get(rhs) {
                                *rhs = new_val.clone();
                                changed = true;
                            }
                        }
                        IRNode::BrCond(cond, _, _) => {
                            if let Some(new_val) = rep.get(cond) {
                                *cond = new_val.clone();
                                changed = true;
                            }
                        }
                        IRNode::Ret(_, Some(val)) => {
                            if let Some(new_val) = rep.get(val) {
                                *val = new_val.clone();
                                changed = true;
                            }
                        }
                        IRNode::Load(_, _, ptr) => {
                            if let Some(new_val) = rep.get(ptr) {
                                *ptr = new_val.clone();
                                changed = true;
                            }
                        }
                        IRNode::Store(_, val, ptr) => {
                            if let Some(new_val) = rep.get(ptr) {
                                *ptr = new_val.clone();
                                changed = true;
                            }
                            if let Some(new_val) = rep.get(val) {
                                *val = new_val.clone();
                                changed = true;
                            }
                        }
                        IRNode::GetElementPtr(_, _, ptr, indexes) => {
                            if let Some(new_val) = rep.get(ptr) {
                                *ptr = new_val.clone();
                                changed = true;
                            }
                            for (_, idx) in indexes {
                                if let Some(new_val) = rep.get(idx) {
                                    *idx = new_val.clone();
                                    changed = true;
                                }
                            }
                        }
                        IRNode::ICMP(_, cond, _, op1, op2) => {
                            if let Some(new_val) = rep.get(cond) {
                                *cond = new_val.clone();
                                changed = true;
                            }
                            if let Some(new_val) = rep.get(op1) {
                                *op1 = new_val.clone();
                                changed = true;
                            }
                            if let Some(new_val) = rep.get(op2) {
                                *op2 = new_val.clone();
                                changed = true;
                            }
                        }
                        IRNode::Call(_, _, _, args) => {
                            for (_, arg) in args {
                                if let Some(new_val) = rep.get(arg) {
                                    *arg = new_val.clone();
                                    changed = true;
                                }
                            }
                        }
                        IRNode::Move(_, rd, rs)=>{
                            if let Some(new_val) = rep.get(rs) {
                                *rs = new_val.clone();
                                changed = true;
                            }
                            if let Some(new_val) = rep.get(rd) {
                                *rd = new_val.clone();
                                changed = true;
                            }
                        }
                        _ => {}
                    }
                }
                for inst in bb.phi.values_mut() {
                    if let IRNode::Phi(_, _, args) = inst {
                        for (val, _) in args {
                            if let Some(new_val) = rep.get(val) {
                                *val = new_val.clone();
                                changed = true;
                            }
                        }
                    }
                }
            }
        }
        // supply phi
        let names = self.nodes.iter().map(|bb| bb.name.clone()).collect::<Vec<_>>();
        for bb in self.nodes.iter_mut() {
            let preds = bb.pred.clone();
            for (_, inst) in bb.phi.iter_mut() {
                if let IRNode::Phi(_, ty, args) = inst {
                    let old_args = args.iter().map(
                        |(_, label)| self.rnk[label]
                    ).collect::<BitSet>();
                    for pred in preds.iter() {
                        if !old_args.contains(pred) {
                            match ty {
                                IRType::PTR(_) => {
                                    args.push((String::from("null"), names[pred].clone()));
                                }
                                _ => {
                                    args.push((String::from("0"), names[pred].clone()));
                                }
                            }
                        }
                    }
                } else {
                    unreachable!()
                }
            }
        }
        self.allocated_vars = allocated.iter().map(|(name, _)| name.clone()).collect();
    }

    pub fn eliminate_phi(&mut self, bb_cnt: &mut usize) {
        // put blank bb
        let mut critical_edges = Vec::new();
        for bb in self.nodes.iter() {
            if bb.succ.len() > 1 {
                for succ in bb.succ.iter() {
                    if self.nodes[succ].pred.len() > 1 {
                        critical_edges.push((self.rnk[&bb.name], succ));
                    }
                }
            }
        }
        let mut generate_bb = || {
            let name = format!("CrazyDave..BB{}", *bb_cnt);
            *bb_cnt += 1;
            name
        };
        for (from, to) in critical_edges.iter() {
            let new_label = generate_bb();
            let rnk = self.nodes.len();
            self.nodes[*from].succ.remove(*to);
            self.nodes[*from].succ.insert(rnk);
            self.nodes[*to].pred.remove(*from);
            self.nodes[*to].pred.insert(rnk);
            let from_name = &self.nodes[*from].name.clone();
            let to_name = &self.nodes[*to].name.clone();
            match self.nodes[*from].ir.last_mut().unwrap() {
                IRNode::BrCond(_, label1, label2) => {
                    if label1 == to_name {
                        *label1 = new_label.clone();
                    }
                    if label2 == to_name {
                        *label2 = new_label.clone();
                    }
                }
                _ => unreachable!()
            }
            // insert new_bb
            let new_bb = BasicBlock {
                name: new_label.clone(),
                succ: BitSet::from_iter([*to]),
                ir: vec![
                    IRNode::Label(new_label.clone()),
                    IRNode::Br(to_name.clone()),
                ],
                phi: HashMap::new(),
                phi_names: Vec::new(),
                dom: BitSet::new(),
                i_dom: BitSet::new(),
                pred: BitSet::from_iter([*from]),
                df: BitSet::new(),
                allocate: Vec::new(),
                store: HashSet::new(),
                mv: Vec::new(),
            };

            self.rnk.insert(new_label.clone(), rnk);
            self.nodes.push(new_bb);

            for phi in self.nodes[*to].phi.values_mut() {
                match phi {
                    IRNode::Phi(_, _, args) => {
                        for (_, label) in args {
                            if label == from_name {
                                *label = new_label.clone();
                            }
                        }
                    }
                    _ => unreachable!()
                }
            }
        }

        // insert mv inst
        let mut tmp_cnt = 0usize;
        for i in 0..self.nodes.len() {
            let mut tmp_map: HashMap<String, Box<HashMap<String, String>>> = HashMap::new();
            let mut phi_res_set = HashSet::new();
            let mut mv_nodes = Vec::new();

            // 先解决phi指令的参数和结果重名的情况，这种情况下在该参数的label对应的块中先插入临时变量move，并且将该参数名字改为临时变量
            for phi_name in self.nodes[i].phi_names.clone().iter() {
                let phi = self.nodes[i].phi.get_mut(phi_name).unwrap();
                match phi {
                    IRNode::Phi(res, ty, args) => {
                        for (val, label) in args.iter_mut() {
                            if phi_res_set.contains(val) {
                                if !tmp_map[val].contains_key(label) {
                                    let tmp_name = format!("{}.tmp.{}", val, tmp_cnt);
                                    tmp_cnt += 1;
                                    mv_nodes.push((
                                        label.clone(),
                                        IRNode::Move(
                                            ty.clone(),
                                            tmp_name.clone(),
                                            val.clone(),
                                        )
                                    ));
                                    tmp_map.get_mut(val).unwrap().insert(label.clone(), tmp_name.clone());
                                }
                                *val = tmp_map[val][label].clone();
                            }
                        }
                        phi_res_set.insert(res.clone());
                        tmp_map.insert(res.clone(), Box::new(HashMap::new()));
                    }
                    _ => unreachable!()
                }
            }

            for phi_name in self.nodes[i].phi_names.iter() {
                let phi = &self.nodes[i].phi[phi_name];
                match phi {
                    IRNode::Phi(res, ty, args) => {
                        for (val, label) in args {
                            mv_nodes.push((
                                label.clone(),
                                IRNode::Move(
                                    ty.clone(),
                                    res.clone(),
                                    val.clone(),
                                )
                            ));
                        }
                    }
                    _ => unreachable!()
                }
            }
            for (label, mv) in &mv_nodes {
                self.nodes[self.rnk[label]].mv.push(mv.clone());
            }
            self.nodes[i].phi.clear();
        }
    }
    pub fn get_ir(&self) -> Vec<IRNode> {
        let mut res = Vec::new();
        for bb in self.nodes.iter() {
            for (_, phi) in bb.phi.iter() {
                res.push(phi.clone());
            }
            for (i, inst) in bb.ir.iter().enumerate() {
                if i == bb.ir.len() - 1 {
                    break;
                }
                match inst {
                    IRNode::Allocate(_, _) => {
                        continue;
                    }
                    IRNode::Load(_, _, ptr) | IRNode::Store(_, _, ptr) => {
                        if self.allocated_vars.contains(ptr) {
                            continue;
                        } else {
                            res.push(inst.clone());
                        }
                    }
                    _ => { res.push(inst.clone()); }
                }
            }
            for mv in bb.mv.iter() {
                res.push(mv.clone());
            }
            res.push(bb.ir.last().unwrap().clone());
        }
        res
    }
}