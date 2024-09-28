use std::collections::{HashMap, HashSet};
use super::IRType;
use super::IRNode;

#[derive(Clone)]
pub struct BasicBlock {
    pub succ: HashSet<String>,
    pub ir: Vec<IRNode>,
    pub phi: HashMap<String, IRNode>, // var name -> phi inst
    pub phi_names: Vec<String>,
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
            succ: match ir.last().unwrap() {
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
            phi_names: Vec::new(),
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
pub fn build_cfg(ir: Vec<IRNode>) -> (HashMap<String, BasicBlock>, Vec<String>) {
    // 建图
    let mut cfg: HashMap<String, BasicBlock> = HashMap::new();
    let mut names = Vec::new();
    let mut cur_label = Some(String::from("entry"));
    let mut cur_ch = Vec::new();
    for inst in ir.iter() {
        if let IRNode::Label(name) = inst {
            cur_label = Some(name.clone());
        }
        if cur_label.is_none() {
            continue;
        }
        cur_ch.push(inst.clone());
        if inst.is_terminator() {
            names.push(cur_label.clone().unwrap());
            cfg.insert(cur_label.clone().unwrap(), BasicBlock::from(cur_ch.clone()));
            cur_label = None;
            cur_ch.clear();
        }
    }

    for name in &names {
        let ch = cfg.get(name).cloned().unwrap().succ;
        for node in ch {
            cfg.get_mut(&node).unwrap().pred.insert(name.clone());
        }
    }
    (cfg, names)
}