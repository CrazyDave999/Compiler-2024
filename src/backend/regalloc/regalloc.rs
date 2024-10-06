use super::IRNode;
use super::Allocator;
use super::AllocResult;

// caller save 指令必须在算法开始之前就预留好，因此在这里预留caller save指令，其中的寄存器内容在算法结束后填写
// 而callee save指令需要知道使用了哪些寄存器，因此在结束后的get_ir中处理

pub fn pass(ir: Vec<IRNode>) -> AllocResult {
    let a_num: i32 = 8;

    let mut alloc_res = AllocResult::new();
    let mut func_name = None;
    let mut func_inner = Vec::new();

    for node in ir {
        match &node {
            IRNode::FuncBegin(_, name, args) => {
                func_name = Some(name.clone());
                alloc_res.ir.push(node.clone());

                // 从a0-a7中以及内存中把参数值传递给形参虚拟寄存器
                for (i, (ty, name)) in args.iter().enumerate() {
                    if (i as i32) < a_num {
                        func_inner.push(IRNode::Move(ty.clone(), name.clone(), format!("%a{}", i)));
                    } else {
                        func_inner.push(IRNode::ArgLoad(ty.clone(), name.clone(), (i as i32 - a_num) * 4));
                    }
                }
                println!("### regalloc: {} ###", name);
            }
            IRNode::FuncEnd => {
                let mut alloc = Allocator::from(func_inner.clone());
                alloc.main();
                alloc_res.ir.extend(alloc.get_ir());
                alloc_res.color.insert(func_name.clone().unwrap(), Box::new(alloc.get_color()));
                alloc_res.spill_temps.insert(func_name.clone().unwrap(), Box::new(alloc.get_spill_vars()));

                func_inner.clear();
                func_name = None;
                alloc_res.ir.push(node);
            }
            _ => {
                if func_name.is_some() {
                    match &node {
                        IRNode::Call(res, res_ty, name, args) => {
                            func_inner.push(IRNode::CallerProtect(name.clone(), Vec::new())); // 预留保护命令
                            for (i, (ty, arg)) in args.iter().enumerate() {
                                if (i as i32) < a_num {
                                    func_inner.push(IRNode::Move(ty.clone(), format!("%a{}", i), arg.clone()));
                                } else {
                                    func_inner.push(IRNode::ArgStore(ty.clone(), arg.clone(), (i as i32 - a_num) * 4));
                                }
                            }
                            func_inner.push(node.clone());
                            if let Some(res) = res {
                                func_inner.push(IRNode::Move(res_ty.clone(), res.clone(), "%a0".to_string()));
                            }
                            func_inner.push(IRNode::CallerRecover(name.clone(), Vec::new())); // 预留恢复命令
                        }
                        IRNode::Ret(ty, res) => {
                            if let Some(res) = res {
                                func_inner.push(IRNode::Move(ty.clone(), "%a0".to_string(), res.clone()));
                            }
                            func_inner.push(node.clone());
                        }
                        _ => {
                            func_inner.push(node.clone());
                        }
                    }
                } else {
                    alloc_res.ir.push(node);
                }
            }
        }
    }
    alloc_res
}
