use std::collections::{HashMap, HashSet};
use super::{IRNode, IRType};
struct Function {
    pub ty: IRType,
    pub name: String,
    pub args: Vec<(IRType, String)>,
    pub ch: Vec<IRNode>,
    pub call: HashMap<String, usize>, // for dfs calculation of size
}
impl Function {
    pub fn new(ty: IRType, name: String, args: Vec<(IRType, String)>) -> Self {
        Function {
            ty,
            name,
            args,
            ch: Vec::new(),
            call: HashMap::new(),
        }
    }
    pub fn get_ir(&self) -> Vec<IRNode> {
        let mut res = Vec::new();
        res.push(IRNode::FuncBegin(self.ty.clone(), self.name.clone(), self.args.clone()));
        res.extend(self.ch.iter().cloned());
        res.push(IRNode::FuncEnd);
        res
    }
    pub fn get_inline_ir(&self, res: &Option<String>, params: &Vec<(IRType, String)>, cnt: &mut usize) -> Vec<IRNode> {
        let mut ir = Vec::new();
        let mut param_map = HashMap::new();
        for i in 0..params.len() {
            param_map.insert(self.args[i].1.clone(), params[i].1.clone());
        }
        let mut rename_map: HashMap<String, String> = HashMap::new();
        for mut inst in self.ch.clone().into_iter() {
            for use_ in (&mut inst).inline_get_use_mut() {
                if let Some(val) = param_map.get(use_) {
                    *use_ = val.clone();
                } else {
                    *use_ = rename_map[use_].clone();
                }
            }
            let mut keep = true; // 如果返回值没有被使用，丢弃move指令
            for def_ in (&mut inst).inline_get_def_mut() {
                if *def_ == "%#ret" {
                    if let Some(res) = res{
                        *def_ = res.clone();
                    }else{
                        keep = false;
                        break;
                    }
                } else {
                    let new_def_ = format!("%inl.{}", *cnt);
                    *cnt += 1;
                    *def_ = rename_map.entry(def_.clone()).or_insert(new_def_).clone();
                }
            }
            if !keep {
                continue;
            }
            match &mut inst {
                IRNode::Label(label) => {
                    let new_label = format!("inl.{}", *cnt);
                    *cnt += 1;
                    *label = rename_map.entry(label.clone()).or_insert(new_label.clone()).clone();
                }
                IRNode::Br(label) => {
                    let new_label = format!("inl.{}", *cnt);
                    *cnt += 1;
                    *label = rename_map.entry(label.clone()).or_insert(new_label.clone()).clone();
                }
                IRNode::BrCond(_, label1, label2) => {
                    let new_label1 = format!("inl.{}", *cnt);
                    *cnt += 1;
                    let new_label2 = format!("inl.{}", *cnt);
                    *cnt += 1;

                    *label1 = rename_map.entry(label1.clone()).or_insert(new_label1.clone()).clone();
                    *label2 = rename_map.entry(label2.clone()).or_insert(new_label2.clone()).clone();
                }
                _ => {}
            }
            ir.push(inst);
        }
        ir
    }
}
pub fn pass(ir: Vec<IRNode>) -> Vec<IRNode> {
    let mut pre = Vec::new();
    let mut funcs = Vec::new();
    let mut func_rnk = HashMap::new(); // name -> rnk
    let mut in_func = false;

    // 内建函数是肯定不能inline的
    let builtin = HashSet::from(
        ["print", "println", "printInt", "printlnInt", "getString", "getInt", "toString", "CrazyDave..boolToString", "malloc", "CrazyDave..AllocArray", "CrazyDave..GetArraySize", "string.length", "string.substring", "string.parseInt", "string.ord", "string.add", "string.eq", "string.ne", "string.lt", "string.gt", "string.le", "string.ge"]
    );

    // 切分成各个函数
    for inst in ir.iter() {
        match inst {
            IRNode::FuncBegin(ty, name, args) => {
                in_func = true;
                funcs.push(Function::new(
                    ty.clone(),
                    name.clone(),
                    args.clone(),
                ));
                func_rnk.insert(name.clone(), funcs.len() - 1);
            }
            IRNode::FuncEnd => {
                in_func = false;
            }
            _ => {
                if in_func {
                    funcs.last_mut().unwrap().ch.push(inst.clone());
                    match inst {
                        IRNode::Call(_, _, name, _) => {
                            if builtin.contains(name.as_str()) {
                                continue;
                            }
                            funcs.last_mut().unwrap().call.entry(name.clone()).and_modify(|x| *x += 1).or_insert(1);
                        }
                        _ => {}
                    }
                } else {
                    pre.push(inst.clone());
                }
            }
        }
    }

    // println!("split");

    // 栈模拟递归，找出环，排除递归函数
    let mut stk = Vec::new();
    let mut stk_set = HashSet::new();
    let mut has_ring: HashSet<String> = HashSet::new();
    let mut stat = HashMap::new();
    for func in funcs.iter() {
        stat.insert(func.name.clone(), 0);
    }

    for func in funcs.iter() {
        if stat[&func.name] != 0 {
            continue;
        }
        stk.push(func.name.clone());
        stk_set.insert(func.name.clone());
        while !stk.is_empty() {
            let cur = &stk.last().cloned().unwrap();
            let rnk = func_rnk[cur];

            if stat[cur] == 0 {
                *stat.get_mut(cur).unwrap() = 1;
                for succ in funcs[rnk].call.keys() {
                    if stk_set.contains(succ) || has_ring.contains(succ) {
                        // stk中stat为1的函数是环上的函数
                        has_ring.extend(stk_set.iter().filter(|&x| stat[x] == 1).cloned());
                        continue;
                    }
                    stk.push(succ.clone());
                    stk_set.insert(succ.clone());
                }
            } else {
                *stat.get_mut(cur).unwrap() = 2;
                stk.pop();
                stk_set.remove(cur);
            }
        }
    }

    // println!("check ring");

    let mut inline_func = HashSet::new();
    let max_size: usize = 200;
    let mut size = HashMap::new();
    stk.clear();
    for func in funcs.iter() {
        stat.insert(func.name.clone(), 0);
    }
    for func in funcs.iter() {
        if stat[&func.name] != 0 || has_ring.contains(&func.name) {
            continue;
        }
        stk.push(func.name.clone());
        while !stk.is_empty() {
            let cur = &stk.last().cloned().unwrap();
            let rnk = func_rnk[cur];
            if stat[cur] == 0 {
                *stat.get_mut(cur).unwrap() = 1;
                for succ in funcs[rnk].call.keys() {
                    if stat[succ] != 0 || has_ring.contains(succ) {
                        continue;
                    }
                    stk.push(succ.clone());
                }
            } else {
                *stat.get_mut(cur).unwrap() = 2;
                stk.pop();
                stk_set.remove(cur);

                let mut sz = funcs[rnk].ch.len();
                for (succ, cnt) in funcs[rnk].call.iter() {
                    if has_ring.contains(succ) {
                        continue;
                    }
                    sz += (size[succ] - 1) * cnt;
                }
                size.insert(cur.clone(), sz);
                if sz <= max_size && cur != "main" {
                    inline_func.insert(cur.clone());
                }
            }
        }
    }

    // println!("calc size");

    // 对于inline函数，递归内联，使得每个函数的ch中没有call，并可以根据参数和返回值的名字生成ir
    let mut func_cnt = 0;
    stk.clear();
    for name in inline_func.iter() {
        stat.insert(name.clone(), 0);
    }
    for name in inline_func.iter() {
        if stat[name] != 0 {
            continue;
        }
        stk.push(name.clone());
        while !stk.is_empty() {
            let cur = &stk.last().cloned().unwrap();
            let rnk = func_rnk[cur];
            if stat[cur] == 0 {
                *stat.get_mut(cur).unwrap() = 1;
                for succ in funcs[rnk].call.keys() {
                    if stat[succ] != 0 || !inline_func.contains(succ) {
                        continue;
                    }
                    stk.push(succ.clone());
                }
            } else {
                *stat.get_mut(cur).unwrap() = 2;
                stk.pop();
                // 子树中的函数已经准备好inline，现在消除当前函数中的call
                let label = format!("%inl.{}", func_cnt);
                func_cnt += 1;
                let mut ch = Vec::new();
                let mut need_label = false;
                for (i, inst) in funcs[rnk].ch.iter().enumerate() {
                    match inst {
                        IRNode::Call(res, _, name, params) => {
                            if inline_func.contains(name) {
                                ch.extend(funcs[func_rnk[name]].get_inline_ir(res, params, &mut func_cnt));
                            } else {
                                ch.push(inst.clone());
                            }
                        }
                        IRNode::Ret(ty, val) => {
                            if let Some(val) = val {
                                ch.push(IRNode::Move(ty.clone(), "%#ret".to_string(), val.clone()));
                            }
                            if i < funcs[rnk].ch.len() - 1 {
                                need_label = true;
                            }
                            if need_label {
                                ch.push(IRNode::Br(label.clone()));
                            }
                        }
                        _ => {
                            ch.push(inst.clone());
                        }
                    }
                }
                if need_label {
                    ch.push(IRNode::Label(label.clone()));
                }
                let _ = std::mem::replace(&mut funcs[rnk].ch, ch);
                // println!("{} finished", cur);
            }
        }
    }
    // println!("inline prepare");
    for (name, rnk) in func_rnk.iter() {
        if inline_func.contains(name) {
            continue;
        }
        let mut ch = Vec::new();
        for inst in funcs[*rnk].ch.iter() {
            match inst {
                IRNode::Call(res, _, name, params) => {
                    if inline_func.contains(name) {
                        ch.extend(funcs[func_rnk[name]].get_inline_ir(res, params, &mut func_cnt));
                    } else {
                        ch.push(inst.clone());
                    }
                }
                _ => {
                    ch.push(inst.clone());
                }
            }
        }
        let _ = std::mem::replace(&mut funcs[*rnk].ch, ch);
    }

    // println!("inline");
    let mut res = Vec::new();
    res.extend(pre);
    for func in funcs.iter() {
        // println!("iter {}", func.name);
        if !inline_func.contains(&func.name) {
            // println!("{} not inline {}", func.name, func.ch.len());
            res.extend(func.get_ir());
        }
    }
    res
}