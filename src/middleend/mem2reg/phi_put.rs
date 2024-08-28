use std::collections::{HashMap, HashSet, VecDeque};
use super::{IRType, IRNode};
use super::BasicBlock;
pub fn put_phi(cfg: &mut HashMap<String, BasicBlock>, names: &Vec<String>) -> HashSet<String> {
    // collect all allocated var
    let mut allocated: Vec<(String, IRType)> = Vec::new();
    for (_, bb) in cfg.iter() {
        allocated.extend(bb.allocate.iter().cloned());
    }

    let mut phi_cnt = 0;
    let mut phi_name = || {
        let name = format!("%p{}", phi_cnt);
        phi_cnt += 1;
        name
    };

    for (var, ty) in &allocated {
        // for every allocated var, first collect all bbs that contain its def
        let mut phi_queue = VecDeque::new();
        phi_queue.extend(names.iter().cloned().filter(
            |name| cfg[name].store.contains(var)
        ));

        // for every bb, reserve phi inst in its df
        while !phi_queue.is_empty() {
            let bb_name = phi_queue.pop_front().unwrap();
            let df = &cfg.get(&bb_name).unwrap().df;
            let df_names: Vec<_> = cfg.iter().filter_map(
                |(name, bb)| {
                    if df.contains(name) && !bb.phi.contains_key(var) {
                        Some(name.clone())
                    } else {
                        None
                    }
                }
            ).collect();

            df_names.iter().for_each(|df_name| {
                cfg.get_mut(df_name).unwrap().phi.insert(
                    var.clone(), IRNode::Phi(phi_name(), ty.clone(), vec![]),
                );
                phi_queue.push_back(df_name.clone());
            });
        }
    }

    let mut rep = HashMap::new();
    for (var, _) in &allocated {
        dfs_write_phi(var, &String::from("entry"), cfg, names, &mut Vec::new(), &mut rep);
    }
    // let _rep = rep.clone();
    rename(cfg, &rep);
    supply_phi(cfg);
    allocated.iter().map(|(var, _)| var.clone()).collect()
}

// write back the parameters of phi instructions
fn dfs_write_phi(alloc_var: &String, cur: &String, cfg: &mut HashMap<String, BasicBlock>, names: &Vec<String>, stk: &mut Vec<String>, rep: &mut HashMap<String, String>) {
    let cur_stk_len = stk.len();

    // let _f1 = *alloc_var == "%p.29";
    let bb = cfg.get_mut(cur).unwrap();
    if let Some(IRNode::Phi(res, _, _)) = bb.phi.get_mut(alloc_var) {
        stk.push(res.clone());
    }
    for ir in &mut bb.ir {
        match ir {
            IRNode::Load(res, _, ptr) => {
                if ptr == alloc_var {
                    rep.insert(res.clone(), stk.last().unwrap().clone());
                }
            }
            IRNode::Store(_, val, ptr) => {
                if ptr == alloc_var {
                    stk.push(val.clone());
                }
            }
            _ => {}
        }
    }


    for name in &cfg.get(cur).unwrap().succ.clone() {
        if let Some(IRNode::Phi(_, _, args)) = cfg.get_mut(name).unwrap().phi.get_mut(alloc_var) {
            if let Some(top) = stk.last() {
                args.push((top.clone(), cur.clone()));
            }
        }
    }

    for name in &cfg.get(cur).unwrap().i_dom.clone() {
        dfs_write_phi(alloc_var, name, cfg, names, stk, rep);
    }
    stk.truncate(cur_stk_len);
}

fn rename(cfg: &mut HashMap<String, BasicBlock>, rep: &HashMap<String, String>) {
    let mut changed = true;
    while changed {
        changed = false;
        for (_, bb) in cfg.iter_mut() {
            for ir in &mut bb.ir {
                match ir {
                    IRNode::Binary(_, _, _, lhs, rhs) => {
                        if let Some(new_val) = rep.get(lhs) {
                            *lhs = new_val.clone();
                            changed = true;
                        }
                        if let Some(new_val) = rep.get(rhs) {
                            *rhs = new_val.clone();
                            changed = true;
                        }
                    }
                    IRNode::BrCond(cond, _, _) => {
                        if let Some(new_val) = rep.get(cond) {
                            *cond = new_val.clone();
                            changed = true;
                        }
                    }
                    IRNode::Ret(_, Some(val)) => {
                        if let Some(new_val) = rep.get(val) {
                            *val = new_val.clone();
                            changed = true;
                        }
                    }
                    IRNode::Load(_, _, ptr) => {
                        if let Some(new_val) = rep.get(ptr) {
                            *ptr = new_val.clone();
                            changed = true;
                        }
                    }
                    IRNode::Store(_, val, ptr) => {
                        if let Some(new_val) = rep.get(val) {
                            *val = new_val.clone();
                            changed = true;
                        }
                        if let Some(new_val) = rep.get(ptr) {
                            *ptr = new_val.clone();
                            changed = true;
                        }
                    }
                    IRNode::GetElementPtr(_, _, ptr, indexes) => {
                        if let Some(new_val) = rep.get(ptr) {
                            *ptr = new_val.clone();
                            changed = true;
                        }
                        for (_, idx) in indexes {
                            if let Some(new_val) = rep.get(idx) {
                                *idx = new_val.clone();
                                changed = true;
                            }
                        }
                    }
                    IRNode::ICMP(_, cond, _, op1, op2) => {
                        if let Some(new_val) = rep.get(cond) {
                            *cond = new_val.clone();
                            changed = true;
                        }
                        if let Some(new_val) = rep.get(op1) {
                            *op1 = new_val.clone();
                            changed = true;
                        }
                        if let Some(new_val) = rep.get(op2) {
                            *op2 = new_val.clone();
                            changed = true;
                        }
                    }
                    IRNode::Call(_, _, _, args) => {
                        for (_, arg) in args {
                            if let Some(new_val) = rep.get(arg) {
                                *arg = new_val.clone();
                                changed = true;
                            }
                        }
                    }
                    _ => {}
                }
            }
            for phi in &mut bb.phi {
                let (_, inst) = phi;
                if let IRNode::Phi(_, _, args) = inst {
                    for (val, _) in args {
                        if let Some(new_val) = rep.get(val) {
                            *val = new_val.clone();
                            changed = true;
                        }
                    }
                }
            }
        }
    }
}

fn supply_phi(cfg: &mut HashMap<String, BasicBlock>) {
    for (_, bb) in &mut *cfg {
        let preds = bb.pred.clone();
        for (_, ir_node) in &mut bb.phi {
            if let IRNode::Phi(_, ty, args) = ir_node {
                let old_args = args.iter().map(
                    |(_, label)| label.clone()
                ).collect::<HashSet<_>>();
                args.extend(
                    preds.iter().filter(
                        |pred| !old_args.contains(*pred)
                    ).map(
                        |pred| {
                            match ty {
                                IRType::PTR(_)=> {
                                    (String::from("null"), pred.clone())
                                }
                                _=>{
                                    (String::from("0"), pred.clone())
                                }
                            }
                        }
                    )
                );
            } else {
                unreachable!()
            }
        }
    }
}

