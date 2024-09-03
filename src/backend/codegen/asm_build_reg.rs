use std::cmp::max;
use std::collections::HashMap;
use crate::middleend::ir::IRType;
use super::ASMNode;
use super::AllocResult;
use super::IRNode;

// 考虑t0,t1用作addi等操作临时变量，虚拟寄存器不能分配到t0，t1
// 前面的mem2reg和regalloc应该保证所有普通指令的操作数是数字，被染色的虚拟寄存器，或全局变量

pub fn build_asm(alloc_res: AllocResult) -> Result<Vec<ASMNode>, String> {
    let mut asm = Vec::new();
    let mut bss = Vec::new();
    let mut rodata = Vec::new();

    let mut beq_cnt = 0;
    let mut i = 0;
    while i < alloc_res.ir.len() {
        match &alloc_res.ir[i] {
            IRNode::FuncBegin(_, name, args) => {
                let color = alloc_res.color[name].as_ref();
                let spill_temps = alloc_res.spill_temps[name].as_ref();

                asm.push(ASMNode::Global(name.clone()));
                asm.push(ASMNode::Label(name.clone()));
                let mut alloc_size = 0;
                alloc_size += 4;
                alloc_size += spill_temps.len() as i32 * 4;

                let mut max_call_arg_spill = 0;
                for j in i + 1.. {
                    match &alloc_res.ir[j] {
                        IRNode::Call(_, _, _, args) => {
                            let mut call_arg_spill = 0;
                            if args.len() > 8 {
                                call_arg_spill = (args.len() - 8) as i32 * 4;
                            }
                            max_call_arg_spill = max(max_call_arg_spill, call_arg_spill);
                        }
                        _ => {}
                    }
                }
                alloc_size += max_call_arg_spill;
                alloc_size = (alloc_size + 15) / 16 * 16;
                addi(&"sp".to_string(), &"sp".to_string(), -alloc_size, &mut asm);
                let mut offset = alloc_size;
                let mut map: HashMap<String, i32> = HashMap::new(); // name, offset

                offset -= 4;
                store(4, &"ra".to_string(), offset, &"sp".to_string(), &mut asm);
                map.insert("#ra".to_string(), offset);
                for tmp in spill_temps.iter() {
                    offset -= 4;
                    map.insert(tmp.clone(), offset);
                }

                let mut arg_spill = 0;
                for (j, (_, name)) in args.iter().enumerate() {
                    if j > 7 {
                        map.insert(name.clone(), alloc_size + arg_spill);
                        arg_spill += 4;
                    }
                }
                loop {
                    i += 1;
                    match &alloc_res.ir[i] {
                        IRNode::Label(label) => {
                            asm.push(ASMNode::Label(label.clone()));
                        }
                        IRNode::Load(res, ty, ptr) => {
                            match ptr.chars().nth(0).unwrap() {
                                '%' => {
                                    load(ty.size(), &color[res], 0, &color[ptr], &mut asm);
                                }
                                '@' => {
                                    load_global(&color[res], &ptr[1..].to_string(), ty, &mut asm);
                                }
                                _ => unreachable!()
                            }
                        }
                        IRNode::Store(ty, val, ptr) => {
                            let val_reg = get_val(val, &"t0".to_string(), &color, &mut asm);
                            match ptr.chars().nth(0).unwrap() {
                                '%' => {
                                    store(ty.size(), &val_reg, 0, &color[ptr], &mut asm);
                                }
                                '@' => {
                                    store_global(&val_reg, &"t0".to_string(), &ptr[1..].to_string(), ty, &mut asm);
                                }
                                _ => unreachable!()
                            }
                        }
                        IRNode::GetElementPtr(res, ty, ptr, indexes) => {
                            let ptr_reg = match ptr.chars().nth(0).unwrap() {
                                '%' => color[ptr].clone(),
                                '@' => {
                                    load_global(&"t0".to_string(), &ptr[1..].to_string(), &IRType::PTR(Box::from(ty.clone())), &mut asm);
                                    "t0".to_string()
                                }
                                _ => unreachable!()
                            };
                            let first_ind = &indexes[0];
                            let first_reg = get_val(&first_ind.1, &"t1".to_string(), &color, &mut asm);
                            asm.push(ASMNode::ArithI(
                                "slli".to_string(),
                                first_reg.clone(),
                                first_reg.clone(),
                                "2".to_string(),
                            ));
                            asm.push(ASMNode::Arith(
                                "add".to_string(),
                                ptr_reg.clone(),
                                ptr_reg.clone(),
                                first_reg.clone(),
                            ));
                            if indexes.len() > 1 {
                                let second_ind = &indexes[1];
                                let second_reg = get_val(&second_ind.1, &"t1".to_string(), &color, &mut asm);
                                asm.push(ASMNode::ArithI(
                                    "slli".to_string(),
                                    second_reg.clone(),
                                    second_reg.clone(),
                                    "2".to_string(),
                                ));
                                asm.push(ASMNode::Arith(
                                    "add".to_string(),
                                    ptr_reg.clone(),
                                    ptr_reg.clone(),
                                    second_reg.clone(),
                                ));
                            }
                            load(4, &color[res], 0, &ptr_reg, &mut asm);
                        }
                        IRNode::Binary(res, op, _, lhs, rhs) => {
                            let lhs_reg = get_val(lhs, &"t0".to_string(), &color, &mut asm);
                            let rhs_reg = get_val(rhs, &"t1".to_string(), &color, &mut asm);
                            asm.push(ASMNode::Arith(
                                match op.as_str() {
                                    "sdiv" => "div".to_string(),
                                    "srem" => "rem".to_string(),
                                    "shl" => "sll".to_string(),
                                    "ashr" => "sra".to_string(),
                                    _ => op.clone()
                                },
                                color[res].clone(),
                                lhs_reg,
                                rhs_reg,
                            ));
                        }
                        IRNode::ICMP(res, cond, _, lhs, rhs) => {
                            let lhs_reg = get_val(lhs, &"t0".to_string(), &color, &mut asm);
                            let rhs_reg = get_val(rhs, &"t1".to_string(), &color, &mut asm);

                            match cond.as_str() {
                                "slt" => {
                                    asm.push(ASMNode::Arith(
                                        "slt".to_string(),
                                        color[res].clone(),
                                        lhs_reg.clone(),
                                        rhs_reg.clone(),
                                    ));
                                }
                                "sge" => {
                                    asm.push(ASMNode::Arith(
                                        "slt".to_string(),
                                        lhs_reg.clone(),
                                        lhs_reg.clone(),
                                        rhs_reg.clone(),
                                    ));
                                    asm.push(ASMNode::ArithI(
                                        "xori".to_string(),
                                        color[res].clone(),
                                        lhs_reg.clone(),
                                        "1".to_string(),
                                    ));
                                }
                                "sgt" => {
                                    asm.push(ASMNode::Arith(
                                        "slt".to_string(),
                                        color[res].clone(),
                                        rhs_reg.clone(),
                                        lhs_reg.clone(),
                                    ));
                                }
                                "sle" => {
                                    asm.push(ASMNode::Arith(
                                        "slt".to_string(),
                                        lhs_reg.clone(),
                                        rhs_reg.clone(),
                                        lhs_reg.clone(),
                                    ));
                                    asm.push(ASMNode::ArithI(
                                        "xori".to_string(),
                                        color[res].clone(),
                                        lhs_reg.clone(),
                                        "1".to_string(),
                                    ));
                                }
                                "eq" => {
                                    asm.push(ASMNode::Arith(
                                        "xor".to_string(),
                                        lhs_reg.clone(),
                                        lhs_reg.clone(),
                                        rhs_reg.clone(),
                                    ));
                                    asm.push(ASMNode::ArithI(
                                        "sltiu".to_string(),
                                        color[res].clone(),
                                        lhs_reg.clone(),
                                        rhs_reg.clone(),
                                    ));
                                }
                                "ne" => {
                                    asm.push(ASMNode::Arith(
                                        "xor".to_string(),
                                        lhs_reg.clone(),
                                        lhs_reg.clone(),
                                        rhs_reg.clone(),
                                    ));
                                    asm.push(ASMNode::Arith(
                                        "sltu".to_string(),
                                        color[res].clone(),
                                        "zero".to_string(),
                                        lhs_reg.clone(),
                                    ));
                                }
                                _ => unreachable!()
                            }
                        }
                        IRNode::BrCond(cond, label1, label2) => {
                            let cond_reg = get_val(cond, &"t0".to_string(), color, &mut asm);
                            let beq_label = format!(".beq.{}", beq_cnt);
                            asm.push(ASMNode::Branch(
                                "beq".to_string(),
                                cond_reg.clone(),
                                "zero".to_string(),
                                beq_label.clone(),
                            ));
                            asm.push(ASMNode::J(
                                label1.clone(),
                            ));
                            asm.push(ASMNode::Label(beq_label.clone()));
                            asm.push(ASMNode::J(
                                label2.clone()
                            ));
                            beq_cnt += 1;
                        }
                        IRNode::Br(label) => {
                            asm.push(ASMNode::J(
                                label.clone(),
                            ));
                        }
                        IRNode::Call(_, _, name, args) => {
                            let mut call_arg_spill = 0;
                            let mut call_arg_cnt = alloc_res.ir[i].get_use().len() as i32;
                            // 参数中，所有的虚拟寄存器已经被分配了物理寄存器，它们占用了a0-a7的前面几个。在前八个参数中，对于立即数，应该将其addi到对应的a0-a7的寄存器
                            for (j, (ty, arg)) in args.iter().enumerate() {
                                if j <= 7 {
                                    match arg.parse::<i32>() {
                                        Ok(val) => {
                                            addi(&format!("a{}", call_arg_cnt), &"zero".to_string(), val, &mut asm);
                                            call_arg_cnt += 1;
                                        }
                                        Err(_) => {}
                                    }
                                } else {
                                    let arg_reg = get_val(arg, &"t0".to_string(), color, &mut asm);
                                    store(
                                        ty.size(),
                                        &arg_reg,
                                        call_arg_spill,
                                        &"sp".to_string(),
                                        &mut asm,
                                    );
                                    call_arg_spill += 4;
                                }
                            }
                            asm.push(ASMNode::Call(name.clone()));
                            // 返回值也应该已经被预着色到a0了
                        }
                        IRNode::Ret(_, res) => {
                            if let Some(res) = res {
                                let res_reg = get_val(res, &"a0".to_string(), color, &mut asm);
                                if res_reg != "a0" {
                                    asm.push(ASMNode::Move(
                                        "a0".to_string(),
                                        res_reg.clone(),
                                    ));
                                }
                            }
                            load(
                                4,
                                &"ra".to_string(),
                                *map.get("#ra").ok_or("Key Not Found")?,
                                &"sp".to_string(),
                                &mut asm,
                            );
                            addi(&"sp".to_string(), &"sp".to_string(), alloc_size, &mut asm);
                            asm.push(ASMNode::Ret);
                        }
                        IRNode::FuncEnd => { break; }
                        IRNode::Move(rd, rs) => {
                            let rs_reg = get_val(rs, &"t0".to_string(), color, &mut asm);
                            if color[rd] != rs_reg {
                                asm.push(ASMNode::Move(
                                    color[rd].clone(),
                                    rs_reg,
                                ));
                            }
                        }
                        IRNode::SpillLoad(ty, tmp, spill_var) => {
                            let offset = *map.get(spill_var).ok_or("Key Not Found")?;
                            load(ty.size(), &color[tmp], offset, &"sp".to_string(), &mut asm);
                        }
                        IRNode::SpillStore(ty, tmp, spill_var) => {
                            let offset = *map.get(spill_var).ok_or("Key Not Found")?;
                            let tmp_reg = get_val(tmp, &"t0".to_string(), color, &mut asm);
                            store(ty.size(), &tmp_reg, offset, &"sp".to_string(), &mut asm);
                        }
                        _ => {}
                    }
                }
            }
            IRNode::Global(name, ty, _) => {
                bss.push(ASMNode::Global(name.clone()));
                bss.push(ASMNode::Align(2));
                bss.push(ASMNode::Label(name.clone()));
                bss.push(ASMNode::Data(ty.size(), 0));
            }
            IRNode::Str(name, _, _, original) => {
                rodata.push(ASMNode::Label(name.clone()));
                rodata.push(ASMNode::Str(original.clone()));
            }
            _ => {}
        }
    }
    asm.push(ASMNode::Segment(".bss".to_string()));
    asm.append(&mut bss);
    asm.push(ASMNode::Segment(".rodata".to_string()));
    asm.append(&mut rodata);
    Ok(asm)
}
fn get_val(val: &String, tmp_reg: &String, color: &HashMap<String, String>, asm: &mut Vec<ASMNode>) -> String {
    if val == "null" {
        "zero".to_string()
    } else {
        match val.parse::<i32>() {
            Ok(val) => {
                addi(&tmp_reg, &"zero".to_string(), val, asm);
                tmp_reg.clone()
            }
            Err(_) => {
                color[val].clone()
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

fn addi(rd: &String, rs: &String, imm: i32, asm: &mut Vec<ASMNode>) {
    if imm >= -2048 && imm <= 2047 {
        asm.push(ASMNode::ArithI(
            "addi".to_string(),
            rd.clone(),
            rs.clone(),
            imm.to_string(),
        ));
    } else {
        let ext = sext_12(imm & 0xfff);
        let mut upper = (imm as u32 >> 12) as i32;
        if ext < 0 {
            upper += 1;
        }
        asm.push(ASMNode::Lui(
            "t0".to_string(),
            upper.to_string(),
        ));
        asm.push(ASMNode::ArithI(
            "addi".to_string(),
            "t0".to_string(),
            "t0".to_string(),
            ext.to_string(),
        ));
        asm.push(ASMNode::Arith(
            "add".to_string(),
            rd.clone(),
            rs.clone(),
            "t0".to_string(),
        ))
    }
}

fn store(size: i32, rs2: &String, imm: i32, rs1: &String, asm: &mut Vec<ASMNode>) {
    if imm >= -2048 && imm <= 2047 {
        asm.push(ASMNode::Store(
            size,
            rs2.clone(),
            imm.to_string(),
            rs1.clone(),
        ));
    } else {
        addi(&"t1".to_string(), rs1, imm, asm);
        asm.push(ASMNode::Store(
            size,
            rs2.clone(),
            "0".to_string(),
            "t1".to_string(),
        ));
    }
}
fn load(size: i32, rd: &String, imm: i32, rs: &String, asm: &mut Vec<ASMNode>) {
    if imm >= -2048 && imm <= 2047 {
        asm.push(ASMNode::Load(
            size,
            rd.clone(),
            imm.to_string(),
            rs.clone(),
        ));
    } else {
        addi(&"t1".to_string(), rs, imm, asm);
        asm.push(ASMNode::Load(
            size,
            rd.clone(),
            "0".to_string(),
            "t1".to_string(),
        ));
    }
}

fn sext_12(imm: i32) -> i32 {
    if imm & 0x800 != 0 {
        (imm as u32 | 0xfffff000u32) as i32
    } else {
        imm
    }
}