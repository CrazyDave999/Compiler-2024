use super::ig::Node;
use std::collections::{HashMap};
pub struct SelectStack {
    stk: Vec<HashMap<String, Node>>,
}
impl SelectStack {
    pub fn new() -> Self {
        SelectStack {
            stk: Vec::new()
        }
    }
    pub fn push(&mut self, s: HashMap<String, Node>) {
        self.stk.push(s);
    }

    pub fn pop(&mut self) -> HashMap<String, Node> {
        self.stk.pop().unwrap()
    }
}

pub struct SpillWorkList{

}