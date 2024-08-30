use std::collections::HashSet;
use super::allocator::Allocator;
pub fn simplify(alloc: &mut Allocator) {
    loop {
        let new_nodes = alloc.ig.remove_low_degree(alloc.k);
        if new_nodes.is_empty() {
            break;
        }
        alloc.sel_stk.push(new_nodes);
    }
}