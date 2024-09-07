use std::collections::HashMap;
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
    for name in &*names {
        let mut mv_nodes = Vec::new();
        for (_, phi) in &cfg.get(name).unwrap().phi.clone() {
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