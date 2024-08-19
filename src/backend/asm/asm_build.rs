use std::cmp::min;
use std::collections::HashMap;
use super::ASMNode;
use super::IRNode;


pub fn build_asm(ir: &Vec<IRNode>) -> Vec<ASMNode> {
    let mut res = Vec::new();
    let mut i = 0;
    while i < ir.len() {
        match &ir[i] {
            IRNode::FuncBegin(_, name, args) => {
                res.push(ASMNode::Global(name.clone()));
                res.push(ASMNode::Label(name.clone()));
                // 让我们来算算需要alloc多少空间吧
                let mut alloc_size = 0;
                alloc_size += min(8, args.len()) as i32;
                let mut map: HashMap<String, i32> = HashMap::new(); // name, offset
                let mut tmp = vec![];
                let mut j = i;
                loop {
                    j += 1;
                    match &ir[j] {
                        IRNode::Allocate(res, ty) => {
                            map.insert(res.clone(), alloc_size);
                            alloc_size += 8;
                        }
                        IRNode::Load(res, ty, ptr) => {
                            map.insert(res.clone(), alloc_size);
                            alloc_size += ty.size();
                            tmp.push(ASMNode::LW(
                                "t0".to_string(),
                                map[ptr],
                                "sp".to_string(),
                            ));
                        }
                        IRNode::GetElementPtr(_, _, _, _) => {
                            alloc_size += 1;
                        }
                        IRNode::Binary(res, op, ty, lhs, rhs) => {
                            alloc_size += ty.size();
                        }
                        IRNode::ICMP(_, _, _, _, _) => {
                            alloc_size += 1;
                        }
                        IRNode::Call(_, _, _, _) => {
                            alloc_size += 1;
                        }
                        IRNode::FuncEnd => { break; }
                        _ => {}
                    }
                }

                res.push(ASMNode::ArithI(
                    "addi".to_string(),
                    "sp".to_string(),
                    "sp".to_string(),
                    -alloc_size
                ));
                j = i;
                loop {
                    j += 1;
                    match &ir[j] {
                        IRNode::FuncEnd => { break; }
                        _ => {}
                    }
                }
                i = j;
                res.push(ASMNode::ArithI(
                    "addi".to_string(),
                    "sp".to_string(),
                    "sp".to_string(),
                    alloc_size
                ));
                res.push(ASMNode::Ret);
            }
            _ => {}
        }
        i += 1;
    }
    res
}
