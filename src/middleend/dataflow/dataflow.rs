use std::collections::{HashMap, HashSet};
use bit_set::BitSet;

use super::IRNode;

// 确定是常量，传播它，并从工作表中删除
// 确定不是常量，从工作表中删除
enum ConstState{
    Const(i32),
    Unknown,
}
#[derive(Clone)]
struct BasicBlock{
    ch: Vec<Option<IRNode>>,
    pred: BitSet,
    succ: BitSet,
}
impl BasicBlock{
    pub fn new()->Self {
        BasicBlock{
            ch: Vec::new(),
            pred: BitSet::new(),
            succ: BitSet::new(),
        }
    }
}
struct DataFlow{
    cfg_nodes: Vec<Option<BasicBlock>>,
    cfg_rnk: HashMap<String, usize>,

    work_list: HashSet<String>,

    def_pos: HashMap<String, Box<HashSet<(usize, usize)>>>,
    use_pos: HashMap<String, Box<HashSet<(usize, usize)>>>,
}
impl DataFlow{
    pub fn new()->Self{
        DataFlow{
            cfg_nodes: Vec::new(),
            cfg_rnk: HashMap::new(),
            work_list: HashSet::new(),
            def_pos: HashMap::new(),
            use_pos: HashMap::new(),
        }
    }
    pub fn from(ir: Vec<IRNode>)->Self{
        let mut df = DataFlow::new();
        let mut bb  = BasicBlock::new();
        let mut name = String::from("entry");
        let mut edges = Vec::new();

        for inst in ir.iter(){
            if let IRNode::Label(n) = inst{
                name = n.clone();
            }
            if name == ""{
                continue;
            }
            let i = df.cfg_nodes.len();
            let j = bb.ch.len();
            for use_ in inst.get_use().iter(){
                df.use_pos.entry(use_.clone()).or_insert(Box::new(HashSet::new())).insert((i, j));
            }
            for def_ in inst.get_def().iter(){
                df.def_pos.entry(def_.clone()).or_insert(Box::new(HashSet::new())).insert((i, j));
                df.use_pos.entry(def_.clone()).or_insert(Box::new(HashSet::new()));
            }
            bb.ch.push(Some(inst.clone()));
            if inst.is_terminator(){
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
        for (u, v) in edges.iter(){
            let (u, v) = (df.cfg_rnk[u], df.cfg_rnk[v]);
            df.cfg_nodes[u].as_mut().unwrap().succ.insert(v);
            df.cfg_nodes[v].as_mut().unwrap().pred.insert(u);
        }
        df.work_list = df.def_pos.keys().cloned().collect();
        df
    }
    pub fn main(&mut self){
        let mut changed = true;
        while changed{
            changed = false;
            changed |= self.constant_propagation();
            changed |= self.constant_branch();
            changed |= self.dead_code_elimination();
            changed |= self.fix_table();
        }
    }
    pub fn get_ir(&self) -> Vec<IRNode>{
        let mut res = Vec::new();
        for bb in self.cfg_nodes.iter(){
            if let Some(bb) = bb{
                for inst in bb.ch.iter(){
                    if let Some(inst) = inst{
                        res.push(inst.clone());
                    }
                }
            }
        }
        res
    }
    fn constant_propagation(&mut self) -> bool{
        let mut new_work_list = HashSet::new();
        let mut changed = false;
        for reg in self.work_list.iter(){
            match self.check_const(reg){
                ConstState::Const(val)=> {
                    changed = true;
                    for (i, j) in self.use_pos[reg].iter(){
                        if let Some(inst) = &mut self.cfg_nodes[*i].as_mut().unwrap().ch[*j]{
                            for use_ in inst.get_use_mut(){
                                if *use_ == *reg{
                                    *use_ = format!("{}", val);
                                }
                            }
                        }
                    }
                    for (i, j) in self.def_pos[reg].iter(){
                        self.cfg_nodes[*i].as_mut().unwrap().ch[*j] = None;
                    }
                }
                ConstState::Unknown=>{
                    new_work_list.insert(reg.clone());
                }
            }
        }
        if changed{
            self.work_list = new_work_list;
        }
        changed
    }
    fn check_const(&self, name: &str) -> ConstState{
        let mut old_val = None;
        for (i, j) in self.def_pos[name].iter(){
            if let Some(inst) = &self.cfg_nodes[*i].as_ref().unwrap().ch[*j]{
                if let Some(val) = inst.check_const(){
                    match old_val{
                        Some(old_val)=>{
                            if old_val != val{
                                return ConstState::Unknown;
                            }
                        }
                        _=>{
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
    fn constant_branch(&mut self)->bool{
        let mut changed = false;
        let mut remove_edges = Vec::new();
        for (i, bb) in self.cfg_nodes.iter_mut().enumerate(){
            match bb {
                Some(bb)=>{
                    if let Some(IRNode::BrCond(cond, label1, label2)) = bb.ch.last().unwrap(){
                        match cond.parse::<i32>(){
                            Ok(val)=>{
                                let j = self.cfg_rnk[label1];
                                let k = self.cfg_rnk[label2];
                                changed = true;
                                if val == 1{
                                    *bb.ch.last_mut().unwrap() = Some(IRNode::Br(label1.clone()));
                                    remove_edges.push((i, k));
                                }else{
                                    *bb.ch.last_mut().unwrap() = Some(IRNode::Br(label2.clone()));
                                    remove_edges.push((i, j));
                                }
                            }
                            _=>{}
                        }
                    }
                }
                _=>{}
            }
        }
        if changed{
            for (u, v) in remove_edges.iter(){
                self.cfg_nodes[*u].as_mut().unwrap().succ.remove(*v);
                self.cfg_nodes[*v].as_mut().unwrap().pred.remove(*u);
            }
        }
        changed
    }
    fn dead_code_elimination(&mut self)->bool{
        let mut changed=false;
        let mut remove_edges = Vec::new();
        for (i, bb_op) in self.cfg_nodes.iter_mut().enumerate(){
            if let Some(bb) = bb_op {
                if bb.pred.is_empty() && i != 0{
                    changed = true;
                    for j in bb.succ.iter(){
                        remove_edges.push((i, j));
                    }
                    *bb_op = None;
                }
            }
        }
        for (u, v) in remove_edges{
            self.cfg_nodes[v].as_mut().unwrap().pred.remove(u);
        }
        for bb in self.cfg_nodes.iter_mut(){
            if let Some(bb) = bb{
                for inst_op in bb.ch.iter_mut(){
                    if let Some(inst) = inst_op {
                        if let Some(def_) = inst.get_def().into_iter().next() {
                            if self.use_pos[&def_].is_empty() {
                                match inst {
                                    IRNode::Call(res,_,_,_)=>{
                                        *res = None;
                                        changed = true;
                                    }
                                    _=>{
                                        *inst_op = None;
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
    fn fix_table(&mut self) -> bool{
        let mut changed = false;
        for (_, pos) in self.def_pos.iter_mut(){
            pos.retain(|(i, j)|{
                if let Some(bb) = self.cfg_nodes[*i].as_mut(){
                    if let Some(_) = bb.ch[*j] {
                        true
                    }else{
                        changed = true;
                        false
                    }
                }else{
                    changed = true;
                    false
                }
            });
        }
        for (_, pos) in self.use_pos.iter_mut(){
            pos.retain(|(i, j)|{
                if let Some(bb) = self.cfg_nodes[*i].as_mut(){
                    if let Some(_) = bb.ch[*j] {
                        true
                    }else{
                        changed = true;
                        false
                    }
                }else{
                    changed = true;
                    false
                }
            });
        }
        self.work_list.retain(|reg|{
            if !self.def_pos[reg].is_empty(){
                true
            }else{
                changed = true;
                false
            }
        });
        changed
    }
}
pub fn pass(ir:Vec<IRNode>)->Vec<IRNode>{
    let mut res= Vec::new();
    let mut in_func = false;
    let mut func_inner = Vec::new();
    for node in ir.iter(){
        match node{
            IRNode::FuncBegin(_,_,_)=>{
                res.push(node.clone());
                in_func=true;
            }
            IRNode::FuncEnd=>{
                let mut df = DataFlow::from(func_inner.clone());
                df.main();
                res.extend(df.get_ir());
                func_inner.clear();
                in_func=false;
                res.push(node.clone());
            }
            _=>{
                if in_func{
                    func_inner.push(node.clone());
                }else{
                    res.push(node.clone());
                }
            }
        }
    }
    res
}