use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use super::IRNode;

#[derive(Eq, PartialEq, Clone)]
pub struct Instruction {
    pub ir_: IRNode,
    pub use_: HashSet<String>,
    pub def_: HashSet<String>,
    // pub in_: HashSet<String>,
    // pub out_: HashSet<String>,
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
            // in_: HashSet::new(),
            // out_: HashSet::new(),
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
    pub fn from(ir: Vec<IRNode>) -> Self {
        let mut res = BasicBlock {
            succ: match ir.last().unwrap() {
                IRNode::Br(label) => {
                    let mut succ = HashSet::new();
                    succ.insert(label.clone());
                    succ
                }
                IRNode::BrCond(_, label1, label2) => {
                    let mut succ = HashSet::new();
                    succ.insert(label1.clone());
                    succ.insert(label2.clone());
                    succ
                }
                _ => HashSet::new(),
            },
            pred: HashSet::new(),
            ch: ir.iter().map(|x| Instruction::from(x)).collect(),
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
    pub nodes: HashMap<String, BasicBlock>,
    names: Vec<String>,
    exit: String,
}
impl ControlFlowGraph {
    pub fn from(mut ir: Vec<IRNode>) -> Self {
        let mut res = ControlFlowGraph {
            nodes: HashMap::new(),
            names: Vec::new(),
            exit: String::new(),
        };
        let mut is_entry = true;
        while let Some(pos) = ir.iter().position(
            |x| x.is_terminator()
        ) {
            let segment = ir.split_off(pos + 1).iter().cloned().collect::<VecDeque<_>>();
            let extracted_segment = std::mem::replace(&mut ir, segment.iter().cloned().collect::<Vec<_>>());
            let name = if is_entry {
                is_entry = false;
                String::from("entry")
            } else {
                match extracted_segment.first().unwrap() {
                    IRNode::Label(name) => name.clone(),
                    _ => unreachable!()
                }
            };
            res.names.push(name.clone());
            res.nodes.insert(name.clone(), BasicBlock::from(extracted_segment));
        }
        if !ir.is_empty() {
            let name = match ir.first().unwrap() {
                IRNode::Label(name) => name.clone(),
                _ => String::from("entry"),
            };
            res.names.push(name.clone());
            res.nodes.insert(name.clone(), BasicBlock::from(ir));
        }
        for name in &res.names {
            if res.nodes[name].succ.is_empty() {
                res.exit = name.clone();
            }
            let succ = res.nodes[name].succ.clone();
            for node in succ {
                res.nodes.get_mut(&node).unwrap().pred.insert(name.clone());
            }
        }

        res
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
    }

    // pub fn get_inst(&self) -> Vec<Instruction> {
    //     let mut res = Vec::new();
    //     for (_, bb) in &self.nodes {
    //         for inst in &bb.ch {
    //             res.push(inst.clone());
    //         }
    //     }
    //     res
    // }

    pub fn insert_use_def(&mut self, spill_nodes: HashSet<String>) -> HashSet<String> {
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
                                inst.ir_.get_ir_type(&node).clone(),
                                spill_name.clone(),
                                node.clone(),
                            ))
                        ))
                    }
                    inst.calc_use_def();
                }
                for (i, inst) in spill_store.into_iter().rev() {
                    bb.ch.insert(i + 1, inst);
                }

                let mut spill_load = Vec::new();
                for (i, inst) in bb.ch.iter_mut().enumerate() {
                    if inst.use_.contains(&node) {
                        let spill_name = format!("%spill.{}.{}", node, spill_cnt);
                        spill_cnt += 1;
                        spill_temps.insert(spill_name.clone());
                        for use_ in inst.ir_.get_use_mut() {
                            if use_ == &node {
                                *use_ = spill_name.clone();
                            }
                        }
                        spill_load.push((
                            i,
                            Instruction::from(&IRNode::SpillLoad(
                                inst.ir_.get_ir_type(&node).clone(),
                                node.clone(),
                                spill_name.clone(),
                            ))
                        ))
                    }
                    inst.calc_use_def();
                }
                for (i, inst) in spill_load.into_iter().rev() {
                    bb.ch.insert(i, inst);
                }
            }
        }
        spill_temps
    }
    pub fn get_ir(&self) -> Vec<IRNode> {
        let mut res = Vec::new();
        for name in &self.names {
            for inst in &self.nodes[name].ch {
                res.push(inst.ir_.clone());
            }
        }
        res
    }
}