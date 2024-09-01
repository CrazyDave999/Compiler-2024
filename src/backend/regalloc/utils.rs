use std::collections::{HashMap, HashSet};
use super::IRNode;

pub struct AllocResult {
    pub ir: Vec<IRNode>,
    pub color: HashMap<String, Box<HashMap<String, String>>>,
    pub spill_temps: HashMap<String, Box<HashSet<String>>>,
}
impl AllocResult {
    pub fn new() -> Self {
        AllocResult {
            ir: Vec::new(),
            color: HashMap::new(),
            spill_temps: HashMap::new(),
        }
    }
}