use std::collections::{HashMap, HashSet};
use super::IRNode;
use super::cfg::Instruction;

#[derive(Clone)]
pub struct Node {
    pub neighbors: HashSet<String>,
    pub color: i32,
    pub mv: bool,
}

impl Node {
    pub fn new() -> Self {
        Node {
            neighbors: HashSet::new(),
            color: -1,
            mv: false,
        }
    }
    pub fn get_degree(&self) -> i32 {
        self.neighbors.len() as i32
    }
}

pub struct InterferenceGraph {
    nodes: HashMap<String, Node>,
    mv_pairs: HashSet<(String, String)>,
}
impl InterferenceGraph {
    pub fn new() -> Self {
        InterferenceGraph {
            nodes: HashMap::new(),
            mv_pairs: HashSet::new(),
        }
    }
    pub fn build(&mut self, instructions: Vec<&Instruction>) {
        for inst in instructions {
            for (x, y) in inst.def_.iter().zip(inst.in_.iter()) {
                self.add_edge(x, y);
            }
            match &inst.ir_ {
                IRNode::Move(rd, rs) => {
                    self.set_mv(&rd, &rs);
                }
                _ => {}
            }
        }
    }
    fn set_mv(&mut self, rd: &String, rs: &String) {
        self.nodes.get_mut(rd).unwrap().mv = true;
        self.nodes.get_mut(rs).unwrap().mv = true;
    }
    fn add_edge(&mut self, from: &str, to: &str) {
        self.nodes.entry(from.to_string()).or_insert(Node::new()).neighbors.insert(to.to_string());
        self.nodes.entry(to.to_string()).or_insert(Node::new()).neighbors.insert(from.to_string());
    }
    pub fn remove_node(&mut self, name: &String) {
        let neighbors = self.nodes.get(name).unwrap().neighbors.clone();
        for neighbor in neighbors {
            self.nodes.get_mut(&neighbor).unwrap().neighbors.remove(name);
        }
        self.nodes.remove(name);
    }
    pub fn insert_node(&mut self, name: &String, node: Node) {
        for neighbor in &node.neighbors {
            self.nodes.get_mut(neighbor).unwrap().neighbors.insert(name.clone());
        }
        self.nodes.insert(name.clone(), node);
    }

    pub fn remove_low_degree(&mut self, k: i32) -> HashMap<String, Node> {
        let res = self.nodes.iter().filter_map(
            |(name, node)| {
                if !node.mv && node.get_degree() < k {
                    Some((name.clone(), node.clone()))
                } else {
                    None
                }
            }
        ).collect();
        for (name, _) in &res {
            self.remove_node(name);
        }
        res
    }
}