use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use super::IRNode;
use super::mem2reg;

#[derive(Eq, PartialEq, Clone)]
pub struct Instruction {
    pub ir_: IRNode,
    pub use_: HashSet<String>,
    pub def_: HashSet<String>,
    pub in_: HashSet<String>,
    pub out_: HashSet<String>,
}
impl Hash for Instruction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ir_.hash(state);
    }
}
impl Instruction {
    pub fn from(ir: &IRNode) -> Self {
        Instruction {
            ir_: ir.clone(),
            use_: ir.get_use(),
            def_: ir.get_def(),
            in_: HashSet::new(),
            out_: HashSet::new(),
        }
    }
    pub fn calc_use_def(&mut self) {
        self.use_ = self.ir_.get_use();
        self.def_ = self.ir_.get_def();
    }
}

#[derive(Clone)]
pub struct BasicBlock {
    pub succ: HashSet<String>,
    pub pred: HashSet<String>,
    pub ch: Vec<Instruction>,
    pub use_: HashSet<String>,
    pub def_: HashSet<String>,
    pub in_: HashSet<String>,
    pub out_: HashSet<String>,
}

impl BasicBlock {
    pub fn from(bb: &mem2reg::BasicBlock) -> Self {
        let mut res = BasicBlock {
            succ: bb.succ.clone(),
            pred: bb.pred.clone(),
            ch: bb.ir.iter().map(|x| Instruction::from(x)).collect(),
            use_: HashSet::new(),
            def_: HashSet::new(),
            in_: HashSet::new(),
            out_: HashSet::new(),
        };
        res.calc_use_def();
        res
    }

    pub fn calc_use_def(&mut self) {
        let mut bb_use_ = HashSet::new();
        let mut bb_def_ = HashSet::new();
        for inst in &self.ch {
            bb_use_ = bb_use_.union(&inst.use_.difference(&bb_def_).cloned().collect()).cloned().collect();
            bb_def_ = bb_def_.union(&inst.def_).cloned().collect();
        }
        self.use_ = bb_use_;
        self.def_ = bb_def_;
    }
}

pub struct ControlFlowGraph {
    nodes: HashMap<String, BasicBlock>,
    exit: String,
}
impl ControlFlowGraph {
    pub fn from(cfg: &HashMap<String, mem2reg::BasicBlock>) -> Self {
        ControlFlowGraph {
            nodes: cfg.iter().map(|(k, v)| (k.clone(), BasicBlock::from(v))).collect(),
            exit: cfg.iter().find_map(|(k, v)| {
                if v.succ.is_empty() {
                    Some(k.clone())
                } else {
                    None
                }
            }).unwrap(),
        }
    }
    fn get_use_(&self, name: &String) -> HashSet<String> {
        self.nodes.get(name).unwrap().use_.clone()
    }
    fn get_def_(&self, name: &String) -> HashSet<String> {
        self.nodes.get(name).unwrap().def_.clone()
    }
    fn get_in_(&self, name: &String) -> HashSet<String> {
        self.nodes.get(name).unwrap().in_.clone()
    }
    fn get_out_(&self, name: &String) -> HashSet<String> {
        self.nodes.get(name).unwrap().out_.clone()
    }

    fn get_succ_(&self, name: &String) -> HashSet<String> {
        self.nodes.get(name).unwrap().succ.clone()
    }

    fn get_pred_(&self, name: &String) -> HashSet<String> {
        self.nodes.get(name).unwrap().pred.clone()
    }

    fn set_in_(&mut self, name: &String, in_: HashSet<String>) {
        self.nodes.get_mut(name).unwrap().in_ = in_;
    }
    fn set_out_(&mut self, name: &String, out_: HashSet<String>) {
        self.nodes.get_mut(name).unwrap().out_ = out_;
    }
    pub fn live_analysis(&mut self) {
        // 逆BFS顺序遍历，计算各个基本块的in_和out_
        let mut changed = true;
        while changed {
            changed = false;
            let mut queue = VecDeque::new();
            let mut visited = HashSet::new();
            queue.push_back(self.exit.clone());
            while !queue.is_empty() {
                let cur = queue.pop_front().unwrap();
                visited.insert(cur.clone());
                let use_ = self.get_use_(&cur);
                let def_ = self.get_def_(&cur);
                let out_ = self.get_out_(&cur);

                let new_in_ = use_.union(&out_.difference(&def_).cloned().collect()).cloned().collect();

                if new_in_ != self.get_in_(&cur) {
                    changed = true;
                    self.set_in_(&cur, new_in_);
                }

                let succ = self.get_succ_(&cur);
                let mut new_out_ = HashSet::new();
                for node in succ {
                    new_out_ = new_out_.union(&self.get_in_(&node)).cloned().collect();
                }
                if new_out_ != self.get_out_(&cur) {
                    changed = true;
                    self.set_out_(&cur, new_out_);
                }
                for node in self.get_pred_(&cur) {
                    if !visited.contains(&node) {
                        queue.push_back(node);
                    }
                }
            }
        }
        // 根据各个基本块的in_和out_计算块内各条指令的in_和out_
        for (_, bb) in &mut self.nodes {
            let mut out_ = bb.out_.clone();
            for inst in bb.ch.iter_mut().rev() {
                inst.out_ = out_.clone();
                inst.in_ = inst.use_.union(&out_.difference(&inst.def_).cloned().collect()).cloned().collect();
                out_ = inst.in_.clone();
            }
        }
    }

    pub fn get_inst(&self) -> Vec<Instruction> {
        let mut res = Vec::new();
        for (_, bb) in &self.nodes {
            for inst in &bb.ch {
                res.push(inst.clone());
            }
        }
        res
    }

    pub fn insert_use_def(&mut self, spill_nodes: HashSet<String>) -> HashSet<String>{
        let mut spill_temps = HashSet::new();
        for node in spill_nodes {
            let mut spill_cnt = 0;
            for (_, bb) in self.nodes.iter_mut() {
                // 对溢出变量，将其use和def都替换成生命周期很短的临时变量
                let mut spill_store = Vec::new();
                for (i, inst) in bb.ch.iter_mut().enumerate() {
                    if inst.def_.contains(&node) {
                        let spill_name = format!("%spill.{}.{}", node, spill_cnt);
                        spill_cnt += 1;
                        spill_temps.insert(spill_name.clone());
                        for def_ in inst.ir_.get_def_mut() {
                            if def_ == &node {
                                *def_ = spill_name.clone();
                            }
                        }
                        spill_store.push((
                            i,
                            Instruction::from(&IRNode::SpillStore(
                                spill_name.clone(),
                                node.clone(),
                            ))
                        ))
                    }
                    inst.calc_use_def();
                }
                for (i, inst) in spill_store.into_iter().rev(){
                    bb.ch.insert(i + 1, inst);
                }

                let mut spill_load = Vec::new();
                for (i, inst) in bb.ch.iter_mut().enumerate(){
                    if inst.use_.contains(&node){
                        let spill_name = format!("%spill.{}.{}", node, spill_cnt);
                        spill_cnt += 1;
                        spill_temps.insert(spill_name.clone());
                        for use_ in inst.ir_.get_use_mut(){
                            if use_ == &node{
                                *use_ = spill_name.clone();
                            }
                        }
                        spill_load.push((
                            i,
                            Instruction::from(&IRNode::SpillLoad(
                                node.clone(),
                                spill_name.clone(),
                            ))
                        ))
                    }
                    inst.calc_use_def();
                }
                for (i, inst) in spill_load.into_iter().rev(){
                    bb.ch.insert(i, inst);
                }
            }
        }
        spill_temps
    }
}