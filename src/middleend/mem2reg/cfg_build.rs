use std::collections::{HashMap, HashSet, VecDeque};
use super::IRType;
use super::IRNode;

#[derive(Clone)]
pub struct BasicBlock {
    pub ch: HashSet<String>,
    pub ir: Vec<IRNode>,
    pub phi: HashMap<String, IRNode>, // var name -> phi inst
    pub dom: HashSet<String>,
    pub i_dom: Vec<String>, // 被自己直接支配的节点
    pub pred: HashSet<String>,
    pub df: Vec<String>,
    pub allocate: Vec<(String, IRType)>,
    pub store: HashSet<String>,
    pub mv: Vec<IRNode>,
}
impl BasicBlock {
    pub fn from(ir: Vec<IRNode>) -> Self {
        BasicBlock {
            ch: match ir.last().unwrap() {
                IRNode::Br(label) => {
                    let mut ch = HashSet::new();
                    ch.insert(label.clone());
                    ch
                }
                IRNode::BrCond(_, label1, label2) => {
                    let mut ch = HashSet::new();
                    ch.insert(label1.clone());
                    ch.insert(label2.clone());
                    ch
                }
                _ => HashSet::new(),
            },
            ir: ir.clone(),
            phi: HashMap::new(),
            dom: HashSet::new(),
            i_dom: Vec::new(),
            pred: HashSet::new(),
            df: Vec::new(),
            allocate: ir.iter().filter_map(|x| {
                match x {
                    IRNode::Allocate(name, ty) => Some((name.clone(), ty.clone())),
                    _ => None,
                }
            }).collect(),
            store: ir.iter().filter_map(|x| {
                match x {
                    IRNode::Store(_, _, ptr) => Some(ptr.clone()),
                    _ => None,
                }
            }).collect(),
            mv: Vec::new(),
        }
    }
}

// 接受一个函数内部的ir
pub fn build_cfg(mut ir: Vec<IRNode>) -> (HashMap<String, BasicBlock>, Vec<String>) {
    // 建图
    let mut cfg: HashMap<String, BasicBlock> = HashMap::new();
    let mut names = Vec::new();
    let mut is_entry = true;
    while let Some(pos) = ir.iter().position(
        |x| x.is_terminator()
    ) {
        let mut segment = ir.split_off(pos + 1).iter().cloned().collect::<VecDeque<_>>();
        while let Some(IRNode::Br(_)) = segment.front() {
            segment.pop_front();
        }
        let extracted_segment = std::mem::replace(&mut ir, segment.iter().cloned().collect::<Vec<_>>());
        let name = if is_entry {
            is_entry = false;
            String::from("entry")
        } else {
            match extracted_segment.first().unwrap() {
                IRNode::Label(name) => name.clone(),
                _ => unreachable!(),
            }
        };

        names.push(name.clone());
        cfg.insert(name, BasicBlock::from(extracted_segment.iter().cloned().collect()));
    }
    if !ir.is_empty() {
        let name = match ir.first().unwrap() {
            IRNode::Label(name) => name.clone(),
            _ => String::from("entry"),
        };
        names.push(name.clone());
        cfg.insert(name, BasicBlock::from(ir));
    }
    for name in &names {
        let ch = cfg.get(name).cloned().unwrap().ch;
        for node in ch {
            cfg.get_mut(&node).unwrap().pred.insert(name.clone());
        }
    }
    (cfg, names)
}