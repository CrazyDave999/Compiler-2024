use std::collections::{HashMap, HashSet};
use super::IRNode;
use super::cfg_build::{BasicBlock, build_cfg};
use super::dt_build::build_dt;
use super::phi_put::put_phi;
use super::ssa::eliminate_phi;
pub fn pass(ir: Vec<IRNode>) -> Vec<IRNode> {
    let mut res = Vec::new();
    let mut in_func = false;
    let mut func_inner = Vec::new();
    let mut bb_cnt = 0;
    for node in ir {
        match node {
            IRNode::FuncBegin(_, _, _) => {
                res.push(node);
                in_func = true;
            }
            IRNode::FuncEnd => {
                let (mut cfg, mut names) = build_cfg(func_inner.clone());
                build_dt(&mut cfg, &names);
                let allocated_vars = put_phi(&mut cfg, &names);
                eliminate_phi(&mut cfg, &mut names, &mut bb_cnt);
                res.extend(get_ir(&cfg, &names, &allocated_vars));

                func_inner.clear();
                in_func = false;
                res.push(node);
            }
            _ => {
                if in_func {
                    func_inner.push(node);
                } else {
                    res.push(node);
                }
            }
        }
    }
    res
}

fn get_ir(cfg: &HashMap<String, BasicBlock>, names: &Vec<String>, allocated_vars: &HashSet<String>) -> Vec<IRNode> {
    let mut res = Vec::new();
    for name in names {
        let bb = cfg.get(name).unwrap();
        if name != "entry" {
            res.push(IRNode::Label(name.clone()));
        }
        for (_, phi) in &bb.phi {
            res.push(phi.clone());
        }
        let start = if name == "entry" {
            0
        } else {
            1
        };

        for ir in &bb.ir[start..bb.ir.len() - 1] {
            match ir {
                IRNode::Allocate(_, _) => {
                    continue;
                }
                IRNode::Load(_, _, ptr) | IRNode::Store(_, _, ptr) => {
                    if allocated_vars.contains(ptr) {
                        continue;
                    } else {
                        res.push(ir.clone());
                    }
                }
                _ => { res.push(ir.clone()); }
            }
        }

        for mv in &bb.mv {
            res.push(mv.clone());
        }

        res.push(bb.ir.last().unwrap().clone());
    }
    res
}