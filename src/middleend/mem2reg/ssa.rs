use std::collections::{HashMap, HashSet};
use super::{IRNode, IRType};
use super::cfg_build::BasicBlock;

// to eliminate critical edges
fn put_blank_bb(cfg: &mut HashMap<String, BasicBlock>, names: &mut Vec<String>) {
    let mut critical_edges = Vec::new();
    for (label, bb) in &*cfg {
        if bb.ch.len() > 1 {
            for node in &bb.ch {
                if cfg.get(node).unwrap().pred.len() > 1 {
                    critical_edges.push((label.clone(), node.clone()));
                }
            }
        }
    }
    let mut bb_cnt = 0;
    let mut generate_bb = || {
        let name = format!("CrazyDave..BB{}", bb_cnt);
        bb_cnt += 1;
        name
    };

    for (from, to) in &critical_edges {
        // modify from_bb
        let new_label = generate_bb();
        cfg.get_mut(from).unwrap().ch.remove(to);
        cfg.get_mut(from).unwrap().ch.insert(new_label.clone());

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
        names.push(new_label.clone());
    }
}

pub fn eliminate_phi(cfg: &mut HashMap<String, BasicBlock>, names: &mut Vec<String>) {
    put_blank_bb(cfg, names);
}