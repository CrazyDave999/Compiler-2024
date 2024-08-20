use std::cmp::max;
use std::collections::HashMap;
use super::ASMNode;
use super::IRNode;
use super::IRType;


pub fn build_asm(ir: &Vec<IRNode>) -> Vec<ASMNode> {
    let mut asm = Vec::new();
    let mut data = Vec::new();

    let mut i = 0;
    while i < ir.len() {
        match &ir[i] {
            IRNode::FuncBegin(_, name, args) => {
                asm.push(ASMNode::Global(name.clone()));
                asm.push(ASMNode::Label(name.clone()));
                // 让我们来算算需要alloc多少空间吧
                let mut alloc_size = 0;

                alloc_size += 4; // for ra

                for (j, (ty, _)) in args.iter().enumerate() {
                    alloc_size += ty.size();
                    if j == 7 {
                        break;
                    }
                }
                let mut max_call_arg_spill = 0;
                for j in i + 1.. {
                    match &ir[j] {
                        IRNode::Allocate(_, ty) => {
                            alloc_size += 4 + ty.size();
                        }
                        IRNode::Load(_, ty, _) => {
                            alloc_size += ty.size();
                        }
                        IRNode::GetElementPtr(_, _, _, _) => {
                            alloc_size += 4;
                        }
                        IRNode::Binary(_, _, ty, _, _) => {
                            alloc_size += ty.size();
                        }
                        IRNode::ICMP(_, _, ty, _, _) => {
                            alloc_size += ty.size();
                        }
                        IRNode::Call(res, ret_ty, _, args) => {
                            if let Some(_) = res {
                                alloc_size += ret_ty.size();
                            }
                            let mut call_arg_spill = 0;
                            for (j, (ty, _)) in args.iter().enumerate() {
                                if j > 7 {
                                    call_arg_spill += ty.size();
                                }
                            }
                            max_call_arg_spill = max(max_call_arg_spill, call_arg_spill);
                        }
                        IRNode::FuncEnd => { break; }
                        _ => {}
                    }
                }
                alloc_size += max_call_arg_spill;
                // alloc_size 应是16的倍数
                alloc_size = (alloc_size + 15) / 16 * 16;
                asm.push(ASMNode::ArithI(
                    "addi".to_string(),
                    "sp".to_string(),
                    "sp".to_string(),
                    (-alloc_size).to_string(),
                ));
                let mut offset = alloc_size;
                let mut map: HashMap<String, i32> = HashMap::new(); // name, offset
                offset -= 4;
                asm.push(ASMNode::Store(
                    4,
                    "ra".to_string(),
                    offset.to_string(),
                    "sp".to_string(),
                ));
                map.insert("#ra".to_string(), offset);

                let mut arg_spill = 0;
                for (j, (ty, name)) in args.iter().enumerate() {
                    if j <= 7 {
                        offset -= ty.size();
                        map.insert(name.clone(), offset);
                        asm.push(ASMNode::Store(
                            ty.size(),
                            format!("a{}", j),
                            offset.to_string(),
                            "sp".to_string(),
                        ));
                    } else {
                        map.insert(name.clone(), alloc_size + arg_spill);
                        arg_spill += ty.size();
                    }
                }

                loop {
                    i += 1;
                    match &ir[i] {
                        IRNode::Label(label) => {
                            asm.push(ASMNode::Label(label.clone()));
                        }
                        IRNode::Allocate(res, ty) => {
                            offset -= 4;
                            map.insert(res.clone(), offset);
                            offset -= ty.size();
                            asm.push(ASMNode::ArithI(
                                "addi".to_string(),
                                "t0".to_string(),
                                "sp".to_string(),
                                offset.to_string(),
                            ));
                            asm.push(ASMNode::Store(
                                4,
                                "t0".to_string(),
                                map[res].to_string(),
                                "sp".to_string(),
                            ));
                        }
                        IRNode::Load(res, ty, ptr) => {
                            // 把一个地址处的东西放入虚拟寄存器，在最蠢的想法里，就是把那个地址处的东西拿出来，存到虚拟寄存器对应的栈空间
                            offset -= ty.size();
                            map.insert(res.clone(), offset);
                            match ptr.chars().nth(0).unwrap() {
                                '%' => {
                                    asm.push(ASMNode::Load(
                                        4,
                                        "t0".to_string(),
                                        map[ptr].to_string(),
                                        "sp".to_string(),
                                    ));
                                    asm.push(ASMNode::Load(
                                        ty.size(),
                                        "t1".to_string(),
                                        0.to_string(),
                                        "t0".to_string(),
                                    ));
                                }
                                '@' => {
                                    load_global(&"t1".to_string(), &ptr[1..].to_string(), ty, &mut asm);
                                }
                                _ => unreachable!()
                            }

                            asm.push(ASMNode::Store(
                                ty.size(),
                                "t1".to_string(),
                                map[res].to_string(),
                                "sp".to_string(),
                            ));
                        }
                        IRNode::Store(ty, val, ptr) => {
                            get_val(&"t0".to_string(), val, ty, &map, &mut asm);

                            match ptr.chars().nth(0).unwrap() {
                                '%' => {
                                    asm.push(ASMNode::Load(
                                        4,
                                        "t1".to_string(),
                                        map[ptr].to_string(),
                                        "sp".to_string(),
                                    ));
                                    asm.push(ASMNode::Store(
                                        ty.size(),
                                        "t0".to_string(),
                                        0.to_string(),
                                        "t1".to_string(),
                                    ));
                                }
                                '@' => {
                                    store_global(&"t0".to_string(), &"t1".to_string(), &ptr[1..].to_string(), ty, &mut asm);
                                }
                                _ => unreachable!()
                            }
                        }
                        IRNode::GetElementPtr(_, _, _, _) => {}
                        IRNode::Binary(res, op, ty, lhs, rhs) => {
                            offset -= ty.size();
                            map.insert(res.clone(), offset);

                            get_val(&"t0".to_string(), lhs, ty, &map, &mut asm);
                            get_val(&"t1".to_string(), rhs, ty, &map, &mut asm);

                            asm.push(ASMNode::Arith(
                                op.clone(),
                                "t0".to_string(),
                                "t0".to_string(),
                                "t1".to_string(),
                            ));
                            asm.push(ASMNode::Store(
                                ty.size(),
                                "t0".to_string(),
                                map[res].to_string(),
                                "sp".to_string(),
                            ));
                        }
                        IRNode::ICMP(res, cond, ty, lhs, rhs) => {
                            offset -= ty.size();
                            map.insert(res.clone(), offset);

                            get_val(&"t0".to_string(), lhs, ty, &map, &mut asm);
                            get_val(&"t1".to_string(), rhs, ty, &map, &mut asm);

                            match cond.as_str() {
                                "slt" => {
                                    asm.push(ASMNode::Arith(
                                        "slt".to_string(),
                                        "t0".to_string(),
                                        "t0".to_string(),
                                        "t1".to_string(),
                                    ));
                                }
                                "sge" => {
                                    asm.push(ASMNode::Arith(
                                        "slt".to_string(),
                                        "t0".to_string(),
                                        "t0".to_string(),
                                        "t1".to_string(),
                                    ));
                                    asm.push(ASMNode::ArithI(
                                        "xori".to_string(),
                                        "t0".to_string(),
                                        "t0".to_string(),
                                        "1".to_string(),
                                    ));
                                }
                                "sgt" => {
                                    asm.push(ASMNode::Arith(
                                        "slt".to_string(),
                                        "t0".to_string(),
                                        "t1".to_string(),
                                        "t0".to_string(),
                                    ));
                                }
                                "sle" => {
                                    asm.push(ASMNode::Arith(
                                        "slt".to_string(),
                                        "t0".to_string(),
                                        "t1".to_string(),
                                        "t0".to_string(),
                                    ));
                                    asm.push(ASMNode::ArithI(
                                        "xori".to_string(),
                                        "t0".to_string(),
                                        "t0".to_string(),
                                        "1".to_string(),
                                    ));
                                }
                                "eq" => {
                                    asm.push(ASMNode::Arith(
                                        "xor".to_string(),
                                        "t0".to_string(),
                                        "t0".to_string(),
                                        "t1".to_string(),
                                    ));
                                    asm.push(ASMNode::ArithI(
                                        "sltiu".to_string(),
                                        "t0".to_string(),
                                        "t0".to_string(),
                                        "1".to_string(),
                                    ));
                                }
                                "ne" => {
                                    asm.push(ASMNode::Arith(
                                        "xor".to_string(),
                                        "t0".to_string(),
                                        "t0".to_string(),
                                        "t1".to_string(),
                                    ));
                                    asm.push(ASMNode::Arith(
                                        "sltu".to_string(),
                                        "t0".to_string(),
                                        "zero".to_string(),
                                        "t0".to_string(),
                                    ));
                                }
                                _ => unreachable!()
                            }
                            asm.push(ASMNode::Store(
                                ty.size(),
                                "t0".to_string(),
                                map[res].to_string(),
                                "sp".to_string(),
                            ));
                        }
                        IRNode::BrCond(cond, label1, label2) => {
                            get_val(&"t0".to_string(), cond, &IRType::i1(), &map, &mut asm);
                            asm.push(ASMNode::Branch(
                                "beq".to_string(),
                                "t0".to_string(),
                                "zero".to_string(),
                                label2.clone(),
                            ));
                            asm.push(ASMNode::J(
                                label1.clone(),
                            ));
                        }
                        IRNode::Br(label) => {
                            asm.push(ASMNode::J(
                                label.clone(),
                            ));
                        }
                        IRNode::Call(res, ret_ty, name, args) => {
                            if let Some(res) = res {
                                offset -= ret_ty.size();
                                map.insert(res.clone(), offset);
                            }
                            let mut call_arg_spill = 0;
                            for (j, (ty, arg)) in args.iter().enumerate() {
                                if j <= 7 {
                                    get_val(&format!("a{}", j), arg, ty, &map, &mut asm);
                                } else {
                                    get_val(&"t0".to_string(), arg, ty, &map, &mut asm);
                                    asm.push(ASMNode::Store(
                                        ty.size(),
                                        "t0".to_string(),
                                        call_arg_spill.to_string(),
                                        "sp".to_string(),
                                    ));
                                    call_arg_spill += ty.size();
                                }
                            }
                            asm.push(ASMNode::Call(name.clone()));
                            if let Some(res) = res {
                                // 返回值在a0里面
                                asm.push(ASMNode::Store(
                                    ret_ty.size(),
                                    "a0".to_string(),
                                    map[res].to_string(),
                                    "sp".to_string(),
                                ));
                            }
                        }
                        IRNode::Ret(ty, res) => {
                            if let Some(res) = res {
                                get_val(&"a0".to_string(), res, ty, &map, &mut asm);
                            }
                            asm.push(ASMNode::Load(
                                4,
                                "ra".to_string(),
                                map["#ra"].to_string(),
                                "sp".to_string(),
                            ));
                            asm.push(ASMNode::ArithI(
                                "addi".to_string(),
                                "sp".to_string(),
                                "sp".to_string(),
                                alloc_size.to_string(),
                            ));
                            asm.push(ASMNode::Ret);
                        }
                        IRNode::FuncEnd => { break; }
                        _ => {}
                    }
                }
            }
            IRNode::Global(name, ty, _) => {
                data.push(ASMNode::Section(".sbss".to_string()));
                data.push(ASMNode::Global(name.clone()));
                data.push(ASMNode::Label(name.clone()));
                data.push(ASMNode::Data(ty.size(), 0));
            }
            IRNode::Str(name, _, _, original) => {
                data.push(ASMNode::Section(".rodata".to_string()));
                data.push(ASMNode::Label(name.clone()));
                data.push(ASMNode::Str(original.clone()));
            }
            _ => {}
        }
        i += 1;
    }
    asm.append(&mut data);
    asm
}

fn get_val(reg: &String, arg: &String, ty: &IRType, map: &HashMap<String, i32>, asm: &mut Vec<ASMNode>) {
    match arg.parse::<i32>() {
        Ok(val) => {
            asm.push(ASMNode::ArithI(
                "addi".to_string(),
                reg.clone(),
                "zero".to_string(),
                val.to_string(),
            ));
        }
        Err(_) => {
            match arg.chars().nth(0).unwrap() {
                '%' => {
                    asm.push(ASMNode::Load(
                        ty.size(),
                        reg.clone(),
                        map[arg].to_string(),
                        "sp".to_string(),
                    ));
                }
                '@' => {
                    // 取得地址，而不是值
                    asm.push(ASMNode::Lui(
                        reg.clone(),
                        format!("%hi({})", &arg[1..]),
                    ));
                    asm.push(ASMNode::ArithI(
                        "addi".to_string(),
                        reg.clone(),
                        reg.clone(),
                        format!("%lo({})", &arg[1..]),
                    ));
                }
                _ => unreachable!()
            }
        }
    }
}

fn load_global(res_reg: &String, name: &String, ty: &IRType, asm: &mut Vec<ASMNode>) {
    asm.push(ASMNode::Lui(
        res_reg.clone(),
        format!("%hi({})", name),
    ));
    asm.push(ASMNode::Load(
        ty.size(),
        res_reg.clone(),
        format!("%lo({})", name),
        res_reg.clone(),
    ));
}
fn store_global(src_reg: &String, tmp_reg: &String, name: &String, ty: &IRType, asm: &mut Vec<ASMNode>) {
    asm.push(ASMNode::Lui(
        tmp_reg.clone(),
        format!("%hi({})", name),
    ));
    asm.push(ASMNode::Store(
        ty.size(),
        src_reg.clone(),
        format!("%lo({})", name),
        tmp_reg.clone(),
    ));
}