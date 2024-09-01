use super::IRNode;
use super::Allocator;
use super::AllocResult;


pub fn pass(ir: Vec<IRNode>) -> AllocResult {
    let mut res = AllocResult::new();
    let mut func_name = None;
    let mut func_inner = Vec::new();
    for node in ir {
        match &node {
            IRNode::FuncBegin(_, name, _) => {
                func_name = Some(name.clone());
                res.ir.push(node);
            }
            IRNode::FuncEnd => {
                let mut alloc = Allocator::from(func_inner.clone());
                alloc.main();
                res.ir.extend(alloc.get_ir());
                res.color.insert(func_name.clone().unwrap(), Box::new(alloc.get_color()));
                res.spill_temps.insert(func_name.clone().unwrap(), Box::new(alloc.get_spill_temps()));

                func_inner.clear();
                func_name = None;
                res.ir.push(node);
            }
            _ => {
                if func_name.is_some() {
                    func_inner.push(node);
                } else {
                    res.ir.push(node);
                }
            }
        }
    }
    res
}