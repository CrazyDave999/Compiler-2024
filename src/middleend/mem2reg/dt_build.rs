use std::collections::{HashMap, HashSet, VecDeque};
use super::BasicBlock;

// step 1: calculate thw dominator set(Dom(n))
fn bfs_calc_dom(cfg: &mut HashMap<String, BasicBlock>, names: &Vec<String>) {
    for (_, bb) in cfg.iter_mut() {
        bb.dom = names.iter().cloned().collect(); // 初始所有dom都是全集
    }

    let mut changed = true;
    while changed {
        changed = false;
        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();
        queue.push_back("entry".to_string());
        while !queue.is_empty() {
            let name = &queue.pop_front().unwrap();
            visited.insert(name.clone());
            queue.extend(cfg[name].ch.iter().cloned().filter(|x| !visited.contains(x)));
            let preds = &cfg[name].pred;
            let mut new_dom: HashSet<String> = names.iter().cloned().collect();

            if preds.is_empty() {
                new_dom.clear();
            } else {
                for pred in preds {
                    new_dom = new_dom.intersection(&cfg[pred].dom).cloned().collect();
                }
            }

            new_dom.insert(name.clone());
            if new_dom != cfg.get(name).unwrap().dom {
                changed = true;
                cfg.get_mut(name).unwrap().dom = new_dom;
            }
        }
    }
}

// step 2: calculate the immediate dominator(IDom(n).i_dom contains n)
fn calc_i_dom(cfg: &mut HashMap<String, BasicBlock>, names: &Vec<String>) {
    for n in names {
        let n_dom_num = cfg[n].dom.len();
        let i_dom_name = cfg[n].dom.iter().find(|&x| {
            cfg[x].dom.len() == n_dom_num - 1
        }).cloned(); // 直接支配n的节点
        if let Some(i_dom_name) = i_dom_name {
            cfg.get_mut(&i_dom_name).unwrap().i_dom.push(n.clone());
        }
    }
}

// step 3: calculate the dominance frontier(DF(n))
fn calc_df(cfg: &mut HashMap<String, BasicBlock>, names: &Vec<String>) {
    for n in names {
        let mut update_names: HashSet<String> = HashSet::new();
        let preds = &cfg[n].pred;
        for m in preds {
            update_names = update_names.union(
                &cfg[m].dom.difference(
                    &cfg[n].dom.difference(
                        &HashSet::from([n.clone()])
                    ).cloned().collect()
                ).cloned().collect()
            ).cloned().collect();
        }
        for name in &update_names {
            cfg.get_mut(name).unwrap().df.push(n.clone());
        }
    }
}

pub fn build_dt(cfg: &mut HashMap<String, BasicBlock>, names: &Vec<String>) {
    bfs_calc_dom(cfg, names);
    calc_i_dom(cfg, names);
    calc_df(cfg, names);
}

