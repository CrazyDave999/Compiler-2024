use bit_set::BitSet;
use std::collections::{HashMap, HashSet, VecDeque};

use super::IRNode;

// 确定是常量，传播它，并从工作表中删除
// 确定不是常量，从工作表中删除
enum ConstState {
    Const(i32),
    Unknown,
}
#[derive(Clone)]
struct BasicBlock {
    name: String,
    ch: Vec<Option<IRNode>>,
    pred: BitSet,
    succ: BitSet,
    ch_cnt: usize,

    dom: BitSet,      // for CDG
    cdg_pred: BitSet, // for CDG
}
impl BasicBlock {
    pub fn new() -> Self {
        BasicBlock {
            name: String::new(),
            ch: Vec::new(),
            pred: BitSet::new(),
            succ: BitSet::new(),
            ch_cnt: 0,
            dom: BitSet::new(),
            cdg_pred: BitSet::new(),
        }
    }
}
struct DataFlow {
    cfg_nodes: Vec<Option<BasicBlock>>,
    cfg_rnk: HashMap<String, usize>,

    work_list: HashSet<String>,

    def_pos: HashMap<String, Box<HashSet<(usize, usize)>>>,
    use_pos: HashMap<String, Box<HashSet<(usize, usize)>>>,

    live: HashSet<(usize, usize)>,
}
impl DataFlow {
    pub fn new() -> Self {
        DataFlow {
            cfg_nodes: Vec::new(),
            cfg_rnk: HashMap::new(),
            work_list: HashSet::new(),
            def_pos: HashMap::new(),
            use_pos: HashMap::new(),
            live: HashSet::new(),
        }
    }
    pub fn from(ir: Vec<IRNode>) -> Self {
        let mut df = DataFlow::new();
        let mut bb = BasicBlock::new();
        let mut name = String::from("entry");
        let mut edges = Vec::new();
        bb.name = name.clone();

        for inst in ir.iter() {
            if let IRNode::Label(n) = inst {
                name = n.clone();
                bb.name = n.clone();
            }
            if name == "" {
                continue;
            }
            let i = df.cfg_nodes.len();
            let j = bb.ch.len();
            for use_ in inst.get_use().iter() {
                df.use_pos
                    .entry(use_.clone())
                    .or_insert(Box::new(HashSet::new()))
                    .insert((i, j));
            }
            for def_ in inst.get_def().iter() {
                df.def_pos
                    .entry(def_.clone())
                    .or_insert(Box::new(HashSet::new()))
                    .insert((i, j));
                df.use_pos
                    .entry(def_.clone())
                    .or_insert(Box::new(HashSet::new()));
            }
            bb.ch.push(Some(inst.clone()));
            bb.ch_cnt += 1;
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
                let rnk = df.cfg_nodes.len();
                df.cfg_rnk.insert(name.clone(), rnk);
                df.cfg_nodes.push(Some(bb.clone()));
                name = String::from("");
                bb = BasicBlock::new();
            }
        }
        for (u, v) in edges.iter() {
            let (u, v) = (df.cfg_rnk[u], df.cfg_rnk[v]);
            df.cfg_nodes[u].as_mut().unwrap().succ.insert(v);
            df.cfg_nodes[v].as_mut().unwrap().pred.insert(u);
        }
        df.work_list = df.def_pos.keys().cloned().collect();
        df
    }
    pub fn main(&mut self) {
        let mut changed = true;
        while changed {
            changed = false;
            changed |= self.constant_propagation();
            changed |= self.constant_branch();
            changed |= self.dead_code_elimination();
            changed |= self.fix_table();
        }
        changed = true;
        while changed {
            changed = false;
            changed |= self.trail_optimization();
        }
        // 重建def_pos, use_pos
        self.def_pos.clear();
        self.use_pos.clear();
        for (i, bb) in self.cfg_nodes.iter().enumerate() {
            if let Some(bb) = bb {
                for (j, inst) in bb.ch.iter().enumerate() {
                    if let Some(inst) = inst {
                        for use_ in inst.get_use().iter() {
                            self.use_pos
                                .entry(use_.clone())
                                .or_insert(Box::new(HashSet::new()))
                                .insert((i, j));
                        }
                        for def_ in inst.get_def().iter() {
                            self.def_pos
                                .entry(def_.clone())
                                .or_insert(Box::new(HashSet::new()))
                                .insert((i, j));
                            self.use_pos
                                .entry(def_.clone())
                                .or_insert(Box::new(HashSet::new()));
                        }
                    }
                }
            }
        }
        self.adce();
    }
    pub fn get_ir(&self) -> Vec<IRNode> {
        let mut res = Vec::new();
        for bb in self.cfg_nodes.iter() {
            if let Some(bb) = bb {
                for inst in bb.ch.iter() {
                    if let Some(inst) = inst {
                        res.push(inst.clone());
                    }
                }
            }
        }
        res
    }
    fn constant_propagation(&mut self) -> bool {
        let mut new_work_list = HashSet::new();
        let mut changed = false;
        for reg in self.work_list.iter() {
            match self.check_const(reg) {
                ConstState::Const(val) => {
                    changed = true;
                    for (i, j) in self.use_pos[reg].iter() {
                        if let Some(inst) = &mut self.cfg_nodes[*i].as_mut().unwrap().ch[*j] {
                            for use_ in inst.get_use_mut() {
                                if *use_ == *reg {
                                    *use_ = format!("{}", val);
                                }
                            }
                        }
                    }
                    for (i, j) in self.def_pos[reg].iter() {
                        self.cfg_nodes[*i].as_mut().unwrap().ch[*j] = None;
                    }
                }
                ConstState::Unknown => {
                    new_work_list.insert(reg.clone());
                }
            }
        }
        if changed {
            self.work_list = new_work_list;
        }
        changed
    }
    fn check_const(&self, name: &str) -> ConstState {
        let mut old_val = None;
        for (i, j) in self.def_pos[name].iter() {
            if let Some(inst) = &self.cfg_nodes[*i].as_ref().unwrap().ch[*j] {
                if let Some(val) = inst.check_const() {
                    match old_val {
                        Some(old_val) => {
                            if old_val != val {
                                return ConstState::Unknown;
                            }
                        }
                        _ => {
                            old_val = Some(val);
                        }
                    }
                } else {
                    return ConstState::Unknown;
                }
            }
        }
        ConstState::Const(old_val.unwrap())
    }
    fn constant_branch(&mut self) -> bool {
        let mut changed = false;
        let mut remove_edges = Vec::new();
        for (i, bb) in self.cfg_nodes.iter_mut().enumerate() {
            match bb {
                Some(bb) => {
                    if let Some(IRNode::BrCond(cond, label1, label2)) = bb.ch.last().unwrap() {
                        match cond.parse::<i32>() {
                            Ok(val) => {
                                let j = self.cfg_rnk[label1];
                                let k = self.cfg_rnk[label2];
                                changed = true;
                                if val == 1 {
                                    *bb.ch.last_mut().unwrap() = Some(IRNode::Br(label1.clone()));
                                    remove_edges.push((i, k));
                                } else {
                                    *bb.ch.last_mut().unwrap() = Some(IRNode::Br(label2.clone()));
                                    remove_edges.push((i, j));
                                }
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
        if changed {
            for (u, v) in remove_edges.iter() {
                self.cfg_nodes[*u].as_mut().unwrap().succ.remove(*v);
                self.cfg_nodes[*v].as_mut().unwrap().pred.remove(*u);
            }
        }
        changed
    }
    fn dead_code_elimination(&mut self) -> bool {
        let mut changed = false;
        let mut remove_edges = Vec::new();
        for (i, bb_op) in self.cfg_nodes.iter_mut().enumerate() {
            if let Some(bb) = bb_op {
                if bb.pred.is_empty() && i != 0 {
                    changed = true;
                    for j in bb.succ.iter() {
                        remove_edges.push((i, j));
                    }
                    *bb_op = None;
                }
            }
        }
        for (u, v) in remove_edges {
            self.cfg_nodes[v].as_mut().unwrap().pred.remove(u);
        }
        for bb in self.cfg_nodes.iter_mut() {
            if let Some(bb) = bb {
                for inst_op in bb.ch.iter_mut() {
                    if let Some(inst) = inst_op {
                        if let Some(def_) = inst.get_def().into_iter().next() {
                            if self.use_pos[&def_].is_empty() {
                                match inst {
                                    IRNode::Call(res, _, _, _) => {
                                        *res = None;
                                        changed = true;
                                    }
                                    _ => {
                                        *inst_op = None;
                                        bb.ch_cnt -= 1;
                                        changed = true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        changed
    }
    fn trail_optimization(&mut self) -> bool {
        let mut changed = false;

        // 对于所有只有一个前驱，同时其前驱只有一个后继的基本块，可以把这个基本块和前驱合并
        let mut move_block = BitSet::new();
        for bb_op in self.cfg_nodes.iter() {
            if let Some(bb) = bb_op {
                if bb.succ.len() == 1 {
                    let succ = bb.succ.iter().next().unwrap();
                    if let Some(succ_bb) = self.cfg_nodes[succ].as_ref() {
                        if succ_bb.pred.len() == 1 {
                            move_block.insert(succ);
                            changed = true;
                        }
                    }
                }
            }
        }

        for n in move_block.iter() {
            let ch = self.cfg_nodes[n].as_ref().unwrap().ch.clone();
            let ch_cnt = self.cfg_nodes[n].as_ref().unwrap().ch_cnt;
            let pred = self.cfg_nodes[n]
                .as_ref()
                .unwrap()
                .pred
                .iter()
                .next()
                .unwrap();
            let succ = self.cfg_nodes[n].as_ref().unwrap().succ.clone();
            let pred_bb = self.cfg_nodes[pred].as_mut().unwrap();
            pred_bb.ch_cnt += ch_cnt - 2;
            pred_bb.ch.pop();
            pred_bb.ch.extend(ch.into_iter().skip(1));

            // 维护pred的succ，succ的pred
            pred_bb.succ.remove(n);
            pred_bb.succ.extend(succ.iter());
            for succ in succ.iter() {
                if let Some(succ_bb) = self.cfg_nodes[succ].as_mut() {
                    succ_bb.pred.remove(n);
                    succ_bb.pred.insert(pred);
                }
            }
            self.cfg_nodes[n] = None;
        }

        // 对于只有跳转语句的基本块，可以把它删除，并修改前驱
        let mut empty_block = BitSet::new();
        for (i, bb_op) in self.cfg_nodes.iter().enumerate() {
            if let Some(bb) = bb_op {
                if i != 0 && bb.ch_cnt == 2 && bb.succ.len() == 1 {
                    empty_block.insert(i);
                    changed = true;
                }
            }
        }

        for n in empty_block.iter() {
            let pred = self.cfg_nodes[n].as_ref().unwrap().pred.clone();
            let succ = self.cfg_nodes[n]
                .as_ref()
                .unwrap()
                .succ
                .iter()
                .next()
                .unwrap();
            let n_name = self.cfg_nodes[n].as_ref().unwrap().name.clone();
            let succ_bb = self.cfg_nodes[succ].as_mut().unwrap();
            succ_bb.pred.remove(n);
            succ_bb.pred.extend(pred.iter());

            let succ_name = succ_bb.name.clone();
            for pred in pred.iter() {
                self.cfg_nodes[pred].as_mut().unwrap().succ.remove(n);
                self.cfg_nodes[pred].as_mut().unwrap().succ.insert(succ);
                match self.cfg_nodes[pred]
                    .as_mut()
                    .unwrap()
                    .ch
                    .last_mut()
                    .unwrap()
                {
                    Some(IRNode::Br(label)) => {
                        *label = succ_name.clone();
                    }
                    Some(IRNode::BrCond(_, label1, label2)) => {
                        if *label1 == n_name {
                            *label1 = succ_name.clone();
                        }
                        if *label2 == n_name {
                            *label2 = succ_name.clone();
                        }
                    }
                    _ => {}
                }
            }
            self.cfg_nodes[n] = None;
        }

        changed
    }
    fn fix_table(&mut self) -> bool {
        let mut changed = false;
        for (_, pos) in self.def_pos.iter_mut() {
            pos.retain(|(i, j)| {
                if let Some(bb) = self.cfg_nodes[*i].as_mut() {
                    if let Some(_) = bb.ch[*j] {
                        true
                    } else {
                        changed = true;
                        false
                    }
                } else {
                    changed = true;
                    false
                }
            });
        }
        for (_, pos) in self.use_pos.iter_mut() {
            pos.retain(|(i, j)| {
                if let Some(bb) = self.cfg_nodes[*i].as_mut() {
                    if let Some(_) = bb.ch[*j] {
                        true
                    } else {
                        changed = true;
                        false
                    }
                } else {
                    changed = true;
                    false
                }
            });
        }
        self.work_list.retain(|reg| {
            if !self.def_pos[reg].is_empty() {
                true
            } else {
                changed = true;
                false
            }
        });
        changed
    }

    fn adce(&mut self) {
        // step 1: 构建CFG反图的支配树，进而构建CDG
        // 虚拟的enter节点
        let mut enter = BasicBlock::new();
        enter.name = "enter".to_string();
        let enter_rnk = self.cfg_nodes.len();
        self.cfg_nodes[0].as_mut().unwrap().pred.insert(enter_rnk);
        enter.succ.insert(0);
        // 虚拟的exit节点，每个return语句都指向它
        let mut exit = BasicBlock::new();
        exit.name = "exit".to_string();
        let exit_rnk = self.cfg_nodes.len() + 1;
        for (i, bb) in self.cfg_nodes.iter_mut().enumerate() {
            if let Some(bb) = bb {
                if let Some(IRNode::Ret(_, _)) = bb.ch.last().unwrap() {
                    bb.succ.insert(exit_rnk);
                    exit.pred.insert(i);
                }
            }
        }
        enter.succ.insert(exit_rnk);
        exit.pred.insert(enter_rnk);
        self.cfg_nodes.push(Some(enter));
        self.cfg_rnk.insert("enter".to_string(), enter_rnk);
        self.cfg_nodes.push(Some(exit));
        self.cfg_rnk.insert("exit".to_string(), exit_rnk);
        self.build_cdg();

        // step 2: 工作表算法
        let mut queue = VecDeque::new();
        for (i, bb) in self.cfg_nodes.iter().enumerate() {
            if let Some(bb) = bb {
                for (j, inst) in bb.ch.iter().enumerate() {
                    if let Some(inst) = inst {
                        if inst.is_effect() {
                            queue.push_back((i, j));
                        }
                    }
                }
            }
        }
        let mut live_bbs = BitSet::new();

        while !queue.is_empty() {
            let (i, j) = queue.pop_front().unwrap();
            if self.live.contains(&(i, j)) {
                continue;
            }
            self.live.insert((i, j));
            live_bbs.insert(i);
            let inst = self.cfg_nodes[i].as_ref().unwrap().ch[j].as_ref().unwrap();
            match inst {
                IRNode::BrCond(_, label1, label2) => {
                    let i1 = self.cfg_rnk[label1];
                    let i2 = self.cfg_rnk[label2];
                    if !live_bbs.contains(i1) {
                        if let Some(bb) = self.cfg_nodes[i1].as_ref() {
                            queue.push_back((i1, bb.ch.len() - 1));
                        }
                    }
                    if !live_bbs.contains(i2) {
                        if let Some(bb) = self.cfg_nodes[i2].as_ref() {
                            queue.push_back((i2, bb.ch.len() - 1));
                        }
                    }
                }
                IRNode::Br(label) => {
                    let i1 = self.cfg_rnk[label];
                    if !live_bbs.contains(i1) {
                        queue.push_back((i1, self.cfg_nodes[i1].as_ref().unwrap().ch.len() - 1));
                    }
                }
                _ => {}
            }
            for use_ in inst.get_use() {
                if let Some(pos) = self.def_pos.get(&use_) {
                    for (u, v) in pos.iter() {
                        if !self.live.contains(&(*u, *v)) {
                            queue.push_back((*u, *v));
                        }
                    }
                }
            }
            for pred in self.cfg_nodes[i].as_ref().unwrap().cdg_pred.iter() {
                if pred == enter_rnk {
                    continue;
                }
                let (u, v) = (pred, self.cfg_nodes[pred].as_ref().unwrap().ch.len() - 1);
                if !self.live.contains(&(u, v)) {
                    queue.push_back((u, v));
                }
            }
        }

        for i in live_bbs.iter() {
            if i == enter_rnk || i == exit_rnk {
                continue;
            }
            if i != 0 {
                self.live.insert((i, 0));
            }
            let l = self.cfg_nodes[i].as_ref().unwrap().ch.len();
            self.live.insert((i, l - 1));
        }

        // 修正所有基本块末尾的branch
        let mut need_fix = Vec::new();
        for (i, bb) in self.cfg_nodes.iter().enumerate() {
            if !live_bbs.contains(i) {
                continue;
            }
            if let Some(bb) = bb {
                match bb.ch.last().unwrap() {
                    Some(IRNode::Br(label)) => {
                        if let Some(j) = self.cfg_rnk.get(label) {
                            if !live_bbs.contains(*j) {
                                need_fix.push(i);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        for i in need_fix.iter() {
            let mut j = self.cfg_nodes[*i].as_ref().unwrap().succ.iter().next().unwrap();
            if self.cfg_nodes[j].is_none(){
                continue;
            }
            let mut visited = BitSet::new();
            while !live_bbs.contains(j) {
                if visited.contains(j) {
                    break;
                }
                if let Some(bb) = self.cfg_nodes[j].as_ref() {
                    for succ in bb.succ.iter() {
                        j = succ;
                        if live_bbs.contains(succ) {
                            break;
                        }
                    }
                }
                visited.insert(j);
            }
            let name = self.cfg_nodes[j].as_ref().unwrap().name.clone();
            match self.cfg_nodes[*i].as_mut().unwrap().ch.last_mut().unwrap() {
                Some(IRNode::Br(label)) => {
                    *label = name;
                }
                _ => {}
            }
        }

        for (i, bb) in self.cfg_nodes.iter_mut().enumerate() {
            if !live_bbs.contains(i) && i != 0 {
                *bb = None;
                continue;
            }
            if let Some(bb) = bb {
                for (j, inst) in bb.ch.iter_mut().enumerate() {
                    if !self.live.contains(&(i, j)) {
                        *inst = None;
                    }
                }
            }
        }


        self.cfg_nodes.pop();
        self.cfg_nodes.pop();
        self.cfg_rnk.remove("enter");
        self.cfg_rnk.remove("exit");
        if !live_bbs.contains(0) {
            let mut first_live = 0;
            for i in 0..self.cfg_nodes.len() {
                if live_bbs.contains(i) {
                    first_live = i;
                    break;
                }
            }
            let ch = self.cfg_nodes[first_live]
                .as_ref()
                .unwrap()
                .ch
                .iter()
                .skip(1)
                .cloned();
            self.cfg_nodes[0].as_mut().unwrap().ch = ch.collect();
            self.cfg_nodes[first_live] = None;
        }
    }
    fn build_cdg(&mut self) {
        let len = self.cfg_nodes.len();
        let mut all_bb = BitSet::new();
        for i in 0..len {
            if self.cfg_nodes[i].is_some() {
                all_bb.insert(i);
            }
        }
        for bb in self.cfg_nodes.iter_mut() {
            if let Some(bb) = bb {
                bb.dom = all_bb.clone();
            }
        }
        let mut changed = true;
        while changed {
            changed = false;
            let mut queue = VecDeque::new();
            let mut visited = BitSet::new();
            queue.push_back(len - 1);
            while !queue.is_empty() {
                let cur = queue.pop_front().unwrap();
                if visited.contains(cur) {
                    continue;
                }
                visited.insert(cur);
                queue.extend(
                    self.cfg_nodes[cur]
                        .as_ref()
                        .unwrap()
                        .pred
                        .iter()
                        .filter(|&x| !visited.contains(x)),
                );
                let succ = &self.cfg_nodes[cur].as_ref().unwrap().succ;
                let mut new_dom = all_bb.clone();
                if succ.is_empty() {
                    new_dom.clear();
                } else {
                    for s in succ.iter() {
                        new_dom.intersect_with(&self.cfg_nodes[s].as_ref().unwrap().dom);
                    }
                }
                new_dom.insert(cur);
                if new_dom != self.cfg_nodes[cur].as_ref().unwrap().dom {
                    changed = true;
                    self.cfg_nodes[cur].as_mut().unwrap().dom = new_dom;
                }
            }
        }

        for n in all_bb.iter() {
            let mut update_names = BitSet::new();
            if let Some(bb) = self.cfg_nodes[n].as_ref() {
                let succ = &bb.succ;
                for m in succ.iter() {
                    if let Some(m_bb) = self.cfg_nodes[m].as_ref() {
                        if let Some(n_bb) = self.cfg_nodes[n].as_ref(){
                            update_names.union_with(
                                &m_bb.dom
                                    .difference(&n_bb.dom.difference(&BitSet::from_iter([n])).collect())
                                    .collect(),
                            );
                        }
                    }
                }
                for x in update_names.iter() {
                    self.cfg_nodes[x].as_mut().unwrap().cdg_pred.insert(n);
                }
            }
        }
    }
}
pub fn pass(ir: Vec<IRNode>) -> Vec<IRNode> {
    let mut res = Vec::new();
    let mut in_func = false;
    let mut func_inner = Vec::new();
    for node in ir.iter() {
        match node {
            IRNode::FuncBegin(_, _, _) => {
                res.push(node.clone());
                in_func = true;
            }
            IRNode::FuncEnd => {
                let mut df = DataFlow::from(func_inner.clone());
                df.main();
                res.extend(df.get_ir());
                func_inner.clear();
                in_func = false;
                res.push(node.clone());
            }
            _ => {
                if in_func {
                    func_inner.push(node.clone());
                } else {
                    res.push(node.clone());
                }
            }
        }
    }
    res
}
