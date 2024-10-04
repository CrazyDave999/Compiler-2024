use std::collections::{HashMap, HashSet};
use super::IRNode;
use super::cfg_build::BasicBlock;

// to eliminate critical edges
fn put_blank_bb(cfg: &mut HashMap<String, BasicBlock>, names: &mut Vec<String>, bb_cnt: &mut i32) {
    let mut critical_edges = Vec::new();
    for (label, bb) in &*cfg {
        if bb.succ.len() > 1 {
            for node in &bb.succ {
                if cfg.get(node).unwrap().pred.len() > 1 {
                    critical_edges.push((label.clone(), node.clone()));
                }
            }
        }
    }
    let mut generate_bb = || {
        let name = format!("CrazyDave..BB{}", *bb_cnt);
        *bb_cnt += 1;
        name
    };

    for (from, to) in &critical_edges {
        // modify from_bb
        let new_label = generate_bb();
        cfg.get_mut(from).unwrap().succ.remove(to);
        cfg.get_mut(from).unwrap().succ.insert(new_label.clone());

        match cfg.get_mut(from).unwrap().ir.last_mut().unwrap() {
            IRNode::BrCond(_, label1, label2) => {
                if label1 == to {
                    *label1 = new_label.clone();
                }
                if label2 == to {
                    *label2 = new_label.clone();
                }
            }
            _ => unreachable!()
        }

        // insert new_bb
        cfg.insert(
            new_label.clone(),
            BasicBlock::from(
                vec![
                    IRNode::Label(new_label.clone()),
                    IRNode::Br(to.clone()),
                ]
            ),
        );

        // modify to_bb
        cfg.get_mut(to).unwrap().pred.remove(from);
        cfg.get_mut(to).unwrap().pred.insert(new_label.clone());
        for phi in cfg.get_mut(to).unwrap().phi.values_mut() {
            match phi {
                IRNode::Phi(_, _, args) => {
                    for (_, label) in args {
                        if label == from {
                            *label = new_label.clone();
                        }
                    }
                }
                _ => unreachable!()
            }
        }
        let l = names.len();
        names.insert(l - 1, new_label.clone());
    }
}

// eliminate phi and insert move
pub fn eliminate_phi(cfg: &mut HashMap<String, BasicBlock>, names: &mut Vec<String>, bb_cnt: &mut i32) {
    put_blank_bb(cfg, names, bb_cnt);
    let mut tmp_cnt = 0;
    for name in names.iter() {
        let mut tmp_map:HashMap<String, Box<HashMap<String, String>>>=HashMap::new();
        let mut phi_res_set = HashSet::new();
        let mut mv_nodes = Vec::new();

        // 先解决phi指令的参数和结果重名的情况，这种情况下在该参数的label对应的块中先插入临时变量move，并且将该参数名字改为临时变量
        for phi_name in cfg[name].phi_names.clone().iter(){
            let phi = cfg.get_mut(name).unwrap().phi.get_mut(phi_name).unwrap();
            match phi {
                IRNode::Phi(res, ty, args) => {
                    for (val, label) in args.iter_mut() {
                        if phi_res_set.contains(val) {
                            if !tmp_map[val].contains_key(label){
                                let tmp_name = format!("{}.tmp.{}", val, tmp_cnt);
                                tmp_cnt += 1;
                                mv_nodes.push((
                                    label.clone(),
                                    IRNode::Move(
                                        ty.clone(),
                                        tmp_name.clone(),
                                        val.clone(),
                                    )
                                ));
                                tmp_map.get_mut(val).unwrap().insert(label.clone(), tmp_name.clone());
                            }
                            *val = tmp_map[val][label].clone();
                        }
                    }
                    phi_res_set.insert(res.clone());
                    tmp_map.insert(res.clone(), Box::new(HashMap::new()));
                }
                _ => unreachable!()
            }
        }

        for phi_name in cfg[name].phi_names.iter() {
            let phi = &cfg[name].phi[phi_name];
            match phi {
                IRNode::Phi(res, ty, args) => {
                    for (val, label) in args {
                        mv_nodes.push((
                            label.clone(),
                            IRNode::Move(
                                ty.clone(),
                                res.clone(),
                                val.clone(),
                            )
                        ));
                    }
                }
                _ => unreachable!()
            }
        }
        for (label, mv) in &mv_nodes {
            cfg.get_mut(label).unwrap().mv.push(mv.clone());
        }
        cfg.get_mut(name).unwrap().phi.clear();
    }
}