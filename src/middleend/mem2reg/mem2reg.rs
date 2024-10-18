use super::IRNode;
use super::utils::CFG;

pub fn pass(ir: Vec<IRNode>) -> Vec<IRNode> {
    let mut res = Vec::new();
    let mut in_func = false;
    let mut func_inner = Vec::new();
    let mut bb_cnt = 0;
    let mut func_name = String::new();
    for node in ir.iter() {
        match node {
            IRNode::FuncBegin(_, name, _) => {
                res.push(node.clone());
                in_func = true;
                func_name = name.clone();
            }
            IRNode::FuncEnd => {
                let mut cfg = CFG::from(func_inner.clone());
                // println!("{} cfg build ok", func_name);
                cfg.build_dt();
                // println!("{} build dt ok", func_name);
                cfg.put_phi();
                // println!("{} put phi ok", func_name);
                cfg.eliminate_phi(&mut bb_cnt);
                // println!("{} eliminate phi ok", func_name);
                res.extend(cfg.get_ir());

                func_inner.clear();
                in_func = false;
                res.push(node.clone());
            }
            _ => {
                if in_func {
                    func_inner.push(node.clone());
                } else {
                    res.push(node.clone());
                }
            }
        }
    }
    res
}
