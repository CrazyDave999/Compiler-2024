use std::cmp::max;
use std::collections::HashMap;
use crate::middleend::ir::IRType;
use super::ASMNode;
use super::AllocResult;
use super::IRNode;

// 考虑t0,t1用作addi等操作临时变量，虚拟寄存器不能分配到t0，t1
// 前面的mem2reg和regalloc应该保证所有普通指令的操作数是数字，被染色的虚拟寄存器，或全局变量

pub fn build_asm(alloc_res: AllocResult) -> Result<Vec<ASMNode>, String> {
    let a_num = 2;

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
                alloc_size += spill_temps.len() as i32 * 4;

                let mut contains_call = false;
                let mut max_call_arg_spill = 0;

                for j in i + 1.. {
                    match &alloc_res.ir[j] {
                        IRNode::Call(_, _, _, args) => {
                            let mut call_arg_spill = 0;
                            if args.len() > a_num {
                                call_arg_spill = (args.len() - a_num) as i32 * 4;
                            }
                            max_call_arg_spill = max(max_call_arg_spill, call_arg_spill);
                            contains_call = true;
                        }
                        IRNode::CalleeProtect(args) => {
                            alloc_size += args.len() as i32 * 4;
                        }
                        IRNode::CallerProtect(_, args) => {
                            alloc_size += args.len() as i32 * 4;
                        }
                        IRNode::FuncEnd => { break; }
                        _ => {}
                    }
                }
                alloc_size += max_call_arg_spill;

                if contains_call {
                    alloc_size += 4;
                }
                alloc_size = (alloc_size + 15) / 16 * 16;

                if alloc_size > 0 {
                    addi(&"sp".to_string(), &"sp".to_string(), -alloc_size, &mut asm);
                }
                let mut offset = alloc_size;
                let mut map: HashMap<String, i32> = HashMap::new(); // name, offset

                // initialize map begins
                if contains_call {
                    offset -= 4;
                    store(4, &"ra".to_string(), offset, &"sp".to_string(), &mut asm);
                    map.insert("#ra".to_string(), offset);
                }

                for tmp in spill_temps.iter() {
                    offset -= 4;
                    map.insert(tmp.clone(), offset);
                }

                // initialize map ends

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
                            let val_reg = get_val(val, &"gp".to_string(), &color, &mut asm);
                            match ptr.chars().nth(0).unwrap() {
                                '%' => {
                                    store(ty.size(), &val_reg, 0, &color[ptr], &mut asm);
                                }
                                '@' => {
                                    store_global(&val_reg, &"tp".to_string(), &ptr[1..].to_string(), ty, &mut asm);
                                }
                                _ => unreachable!()
                            }
                        }
                        IRNode::GetElementPtr(res, ty, ptr, indexes) => {
                            let ptr_reg = match ptr.chars().nth(0).unwrap() {
                                '%' => color[ptr].clone(),
                                '@' => {
                                    load_global(&"gp".to_string(), &ptr[1..].to_string(), &IRType::PTR(Box::from(ty.clone())), &mut asm);
                                    "gp".to_string()
                                }
                                _ => unreachable!()
                            };
                            let first_ind = &indexes[0];
                            let first_reg = get_val(&first_ind.1, &"tp".to_string(), &color, &mut asm);
                            asm.push(ASMNode::ArithI(
                                "slli".to_string(),
                                "tp".to_string(),
                                first_reg.clone(),
                                "2".to_string(),
                            ));
                            asm.push(ASMNode::Arith(
                                "add".to_string(),
                                color[res].clone(),
                                ptr_reg.clone(),
                                "tp".to_string(),
                            ));
                            if indexes.len() > 1 {
                                let second_ind = &indexes[1];
                                let second_reg = get_val(&second_ind.1, &"tp".to_string(), &color, &mut asm);
                                asm.push(ASMNode::ArithI(
                                    "slli".to_string(),
                                    "tp".to_string(),
                                    second_reg.clone(),
                                    "2".to_string(),
                                ));
                                asm.push(ASMNode::Arith(
                                    "add".to_string(),
                                    color[res].clone(),
                                    color[res].clone(),
                                    "tp".to_string(),
                                ));
                            }
                        }
                        IRNode::Binary(res, op, _, lhs, rhs) => {
                            let lhs_reg = get_val(lhs, &"gp".to_string(), &color, &mut asm);
                            let rhs_reg = get_val(rhs, &"tp".to_string(), &color, &mut asm);
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
                            let lhs_reg = get_val(lhs, &"gp".to_string(), &color, &mut asm);
                            let rhs_reg = get_val(rhs, &"tp".to_string(), &color, &mut asm);

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
                                        "gp".to_string(),
                                        lhs_reg.clone(),
                                        rhs_reg.clone(),
                                    ));
                                    asm.push(ASMNode::ArithI(
                                        "xori".to_string(),
                                        color[res].clone(),
                                        "gp".to_string(),
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
                                        "gp".to_string(),
                                        rhs_reg.clone(),
                                        lhs_reg.clone(),
                                    ));
                                    asm.push(ASMNode::ArithI(
                                        "xori".to_string(),
                                        color[res].clone(),
                                        "gp".to_string(),
                                        "1".to_string(),
                                    ));
                                }
                                "eq" => {
                                    asm.push(ASMNode::Arith(
                                        "xor".to_string(),
                                        "gp".to_string(),
                                        lhs_reg.clone(),
                                        rhs_reg.clone(),
                                    ));
                                    asm.push(ASMNode::ArithI(
                                        "sltiu".to_string(),
                                        color[res].clone(),
                                        "gp".to_string(),
                                        "1".to_string(),
                                    ));
                                }
                                "ne" => {
                                    asm.push(ASMNode::Arith(
                                        "xor".to_string(),
                                        "gp".to_string(),
                                        lhs_reg.clone(),
                                        rhs_reg.clone(),
                                    ));
                                    asm.push(ASMNode::Arith(
                                        "sltu".to_string(),
                                        color[res].clone(),
                                        "zero".to_string(),
                                        "gp".to_string(),
                                    ));
                                }
                                _ => unreachable!()
                            }
                        }
                        IRNode::BrCond(cond, label1, label2) => {
                            let cond_reg = get_val(cond, &"gp".to_string(), color, &mut asm);
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
                        IRNode::Call(_, _, name, _) => {
                            asm.push(ASMNode::Call(name.clone()));
                        }
                        IRNode::Ret(_, _) => {
                            if contains_call {
                                load(
                                    4,
                                    &"ra".to_string(),
                                    map["#ra"],
                                    &"sp".to_string(),
                                    &mut asm,
                                );
                            }
                            if alloc_size > 0 {
                                addi(&"sp".to_string(), &"sp".to_string(), alloc_size, &mut asm);
                            }
                            asm.push(ASMNode::Ret);
                        }
                        IRNode::FuncEnd => { break; }
                        IRNode::Move(_, rd, rs) => {
                            match rs.parse::<i32>() {
                                Ok(val) => {
                                    addi(&color[rd], &"zero".to_string(), val, &mut asm);
                                }
                                Err(_) => {
                                    match rs.chars().nth(0).unwrap() {
                                        '%' => {
                                            if color[rd] != color[rs] {
                                                asm.push(ASMNode::Move(
                                                    color[rd].clone(),
                                                    color[rs].clone(),
                                                ));
                                            }
                                        }
                                        '@' => {
                                            get_val(&rs, &color[rd], color, &mut asm);
                                        }
                                        _ => unreachable!()
                                    }
                                }
                            }
                        }
                        IRNode::SpillLoad(ty, tmp, spill_var) => {
                            let offset = map[spill_var];
                            load(ty.size(), &color[tmp], offset, &"sp".to_string(), &mut asm);
                        }
                        IRNode::SpillStore(ty, tmp, spill_var) => {
                            let offset = map[spill_var];
                            let tmp_reg = get_val(tmp, &"gp".to_string(), color, &mut asm);
                            store(ty.size(), &tmp_reg, offset, &"sp".to_string(), &mut asm);
                        }
                        IRNode::ArgLoad(ty, rd, offset) => {
                            load(ty.size(), &color[rd], *offset + alloc_size, &"sp".to_string(), &mut asm);
                        }
                        IRNode::ArgStore(ty, rs, offset) => {
                            let rs_reg = get_val(rs, &"gp".to_string(), color, &mut asm);
                            store(ty.size(), &rs_reg, *offset, &"sp".to_string(), &mut asm);
                        }
                        IRNode::CalleeProtect(args) => {
                            for reg in args {
                                offset -= 4;
                                map.insert(reg.clone(), offset);
                                store(4, reg, offset, &"sp".to_string(), &mut asm);
                            }
                        }
                        IRNode::CalleeRecover(args) => {
                            for reg in args {
                                load(4, reg, map[reg], &"sp".to_string(), &mut asm);
                            }
                        }
                        IRNode::CallerProtect(_, args) => {
                            for reg in args {
                                offset -= 4;
                                map.insert(reg.clone(), offset);
                                store(4, reg, offset, &"sp".to_string(), &mut asm);
                            }
                        }
                        IRNode::CallerRecover(_, args) => {
                            for reg in args {
                                load(4, reg, map[reg], &"sp".to_string(), &mut asm);
                            }
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
        i += 1;
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
                match val.chars().nth(0).unwrap() {
                    '%' => color[val].clone(),
                    '@' => {
                        asm.push(ASMNode::Lui(
                            tmp_reg.clone(),
                            format!("%hi({})", &val[1..]),
                        ));
                        asm.push(ASMNode::ArithI(
                            "addi".to_string(),
                            tmp_reg.clone(),
                            tmp_reg.clone(),
                            format!("%lo({})", &val[1..]),
                        ));
                        tmp_reg.clone()
                    }
                    _ => unreachable!()
                }
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
            "gp".to_string(),
            upper.to_string(),
        ));
        asm.push(ASMNode::ArithI(
            "addi".to_string(),
            "gp".to_string(),
            "gp".to_string(),
            ext.to_string(),
        ));
        asm.push(ASMNode::Arith(
            "add".to_string(),
            rd.clone(),
            rs.clone(),
            "gp".to_string(),
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
        addi(&"tp".to_string(), rs1, imm, asm);
        asm.push(ASMNode::Store(
            size,
            rs2.clone(),
            "0".to_string(),
            "tp".to_string(),
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
        addi(&"tp".to_string(), rs, imm, asm);
        asm.push(ASMNode::Load(
            size,
            rd.clone(),
            "0".to_string(),
            "tp".to_string(),
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