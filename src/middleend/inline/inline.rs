use super::{IRNode, IRType};
use crate::middleend::ir::IRNode::Global;
use std::collections::{HashMap, HashSet, VecDeque};
struct Function {
    pub ty: IRType,
    pub name: String,
    pub args: Vec<(IRType, String)>,
    pub ch: VecDeque<IRNode>,
    pub call: HashMap<String, usize>, // for dfs calculation of size
}
struct GlobalInfo {
    pub ty: IRType,
    pub in_func: HashSet<String>,
}
impl Function {
    pub fn new(ty: IRType, name: String, args: Vec<(IRType, String)>) -> Self {
        Function {
            ty,
            name,
            args,
            ch: VecDeque::new(),
            call: HashMap::new(),
        }
    }
    pub fn get_ir(&self) -> Vec<IRNode> {
        let mut res = Vec::new();
        res.push(IRNode::FuncBegin(
            self.ty.clone(),
            self.name.clone(),
            self.args.clone(),
        ));
        res.extend(self.ch.iter().cloned());
        res.push(IRNode::FuncEnd);
        res
    }
    pub fn get_inline_ir(
        &self,
        res: &Option<String>,
        params: &Vec<(IRType, String)>,
        cnt: &mut usize,
    ) -> Vec<IRNode> {
        let mut ir = Vec::new();
        let mut param_map = HashMap::new();
        for i in 0..params.len() {
            param_map.insert(self.args[i].1.clone(), params[i].1.clone());
        }
        let mut rename_map: HashMap<String, String> = HashMap::new();
        for mut inst in self.ch.clone().into_iter() {
            for use_ in (&mut inst).get_use_mut() {
                if let Some(val) = param_map.get(use_) {
                    *use_ = val.clone();
                } else {
                    *use_ = rename_map.get(use_).unwrap_or(&"0".to_string()).clone();
                }
            }
            let mut keep = true; // 如果返回值没有被使用，丢弃move指令
            for def_ in (&mut inst).get_def_mut() {
                if *def_ == "%#ret" {
                    if let Some(res) = res {
                        *def_ = res.clone();
                    } else {
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
                    *label = rename_map
                        .entry(label.clone())
                        .or_insert(new_label.clone())
                        .clone();
                }
                IRNode::Br(label) => {
                    let new_label = format!("inl.{}", *cnt);
                    *cnt += 1;
                    *label = rename_map
                        .entry(label.clone())
                        .or_insert(new_label.clone())
                        .clone();
                }
                IRNode::BrCond(_, label1, label2) => {
                    let new_label1 = format!("inl.{}", *cnt);
                    *cnt += 1;
                    let new_label2 = format!("inl.{}", *cnt);
                    *cnt += 1;

                    *label1 = rename_map
                        .entry(label1.clone())
                        .or_insert(new_label1.clone())
                        .clone();
                    *label2 = rename_map
                        .entry(label2.clone())
                        .or_insert(new_label2.clone())
                        .clone();
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
    let builtin = HashSet::from([
        "print",
        "println",
        "printInt",
        "printlnInt",
        "getString",
        "getInt",
        "toString",
        "CrazyDave..boolToString",
        "malloc",
        "CrazyDave..AllocArray",
        "CrazyDave..GetArraySize",
        "string.length",
        "string.substring",
        "string.parseInt",
        "string.ord",
        "string.add",
        "string.eq",
        "string.ne",
        "string.lt",
        "string.gt",
        "string.le",
        "string.ge",
    ]);

    // 切分成各个函数
    for inst in ir.iter() {
        match inst {
            IRNode::FuncBegin(ty, name, args) => {
                in_func = true;
                funcs.push(Function::new(ty.clone(), name.clone(), args.clone()));
                func_rnk.insert(name.clone(), funcs.len() - 1);
            }
            IRNode::FuncEnd => {
                in_func = false;
            }
            _ => {
                if in_func {
                    funcs.last_mut().unwrap().ch.push_back(inst.clone());
                    match inst {
                        IRNode::Call(_, _, name, _) => {
                            if builtin.contains(name.as_str()) {
                                continue;
                            }
                            funcs
                                .last_mut()
                                .unwrap()
                                .call
                                .entry(name.clone())
                                .and_modify(|x| *x += 1)
                                .or_insert(1);
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
    let max_size: usize = 2000;
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
                let mut ch = VecDeque::new();
                let mut need_label = false;
                for (i, inst) in funcs[rnk].ch.iter().enumerate() {
                    match inst {
                        IRNode::Call(res, _, name, params) => {
                            if inline_func.contains(name) {
                                ch.extend(funcs[func_rnk[name]].get_inline_ir(
                                    res,
                                    params,
                                    &mut func_cnt,
                                ));
                            } else {
                                ch.push_back(inst.clone());
                            }
                        }
                        IRNode::Ret(ty, val) => {
                            if let Some(val) = val {
                                ch.push_back(IRNode::Move(
                                    ty.clone(),
                                    "%#ret".to_string(),
                                    val.clone(),
                                ));
                            }
                            if i < funcs[rnk].ch.len() - 1 {
                                need_label = true;
                            }
                            if need_label {
                                ch.push_back(IRNode::Br(label.clone()));
                            }
                        }
                        _ => {
                            ch.push_back(inst.clone());
                        }
                    }
                }
                if need_label {
                    ch.push_back(IRNode::Label(label.clone()));
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
        let mut ch = VecDeque::new();
        for inst in funcs[*rnk].ch.iter() {
            match inst {
                IRNode::Call(res, _, name, params) => {
                    if inline_func.contains(name) {
                        ch.extend(funcs[func_rnk[name]].get_inline_ir(res, params, &mut func_cnt));
                    } else {
                        ch.push_back(inst.clone());
                    }
                }
                _ => {
                    ch.push_back(inst.clone());
                }
            }
        }
        let _ = std::mem::replace(&mut funcs[*rnk].ch, ch);
    }

    // 函数的数量变少，这时检查全局变量，如果一个全局变量只在一个函数中使用，则将其转换为局部变量，从而参与mem2reg
    let mut infos = Vec::new();
    let mut info_rnk = HashMap::new();
    for inst in pre.iter() {
        match inst {
            Global(name, ty, _) => {
                let rnk = infos.len();
                let name = format!("@{}", name);
                infos.push(GlobalInfo {
                    ty: ty.clone(),
                    in_func: HashSet::new(),
                });
                info_rnk.insert(name.clone(), rnk);
            }
            _ => {}
        }
    }
    for func in funcs.iter() {
        if !inline_func.contains(&func.name) {
            for inst in func.ch.iter() {
                match inst {
                    IRNode::Load(_, _, ptr) => {
                        if let Some(rnk) = info_rnk.get(ptr) {
                            infos[*rnk].in_func.insert(func.name.clone());
                        }
                    }
                    IRNode::Store(_, _, ptr) => {
                        if let Some(rnk) = info_rnk.get(ptr) {
                            infos[*rnk].in_func.insert(func.name.clone());
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    let mut changed_names = HashSet::new();
    for (name, rnk) in info_rnk.iter() {
        if infos[*rnk].in_func.len() == 1 {
            changed_names.insert(name.clone());
        }
    }
    let global_name = |name: &String| -> String {
        format!("%global.{}", name.chars().skip(1).collect::<String>())
    };
    for func in funcs.iter_mut() {
        if !inline_func.contains(&func.name) {
            for inst in func.ch.iter_mut() {
                match inst {
                    IRNode::Store(_, _, ptr) => {
                        if changed_names.contains(ptr) {
                            *ptr = global_name(ptr);
                        }
                    }
                    IRNode::Load(_, _, ptr) => {
                        if changed_names.contains(ptr) {
                            *ptr = global_name(ptr);
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    // 插入alloca
    for (name, rnk) in info_rnk.iter() {
        if changed_names.contains(name) {
            for func in infos[*rnk].in_func.iter() {
                funcs[func_rnk[func]]
                    .ch
                    .push_front(IRNode::Allocate(global_name(name), infos[*rnk].ty.clone()));
            }
        }
    }
    pre.retain(|x| match x {
        Global(name, _, _) => !changed_names.contains(&format!("@{}", name)),
        _ => true,
    });

    let mut res = Vec::new();
    res.extend(pre);
    for func in funcs.iter() {
        if !inline_func.contains(&func.name) {
            res.extend(func.get_ir());
        }
    }
    res
}
