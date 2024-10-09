use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use super::IRType;
#[derive(Eq, Clone, PartialEq, Hash)]
pub enum IRNode {
    Class(String, Vec<IRType>), // name, fields
    Global(String, IRType, String), // name, ty, val
    Str(String, IRType, String, String), // name, ty, val, original

    FuncBegin(IRType, String, Vec<(IRType, String)>), // ret_ty, name, args
    FuncEnd,
    Binary(String, String, IRType, String, String), // res, op, ty, lhs, rhs
    BrCond(String, String, String), // cond, label1, label2
    Br(String), // label
    Ret(IRType, Option<String>), // ty, val
    Allocate(String, IRType), // res, ty
    Load(String, IRType, String), // res, ty, ptr
    Store(IRType, String, String), // ty, val, ptr
    GetElementPtr(String, IRType, String, Vec<(IRType, String)>), // res, ty, ptr, indexes
    ICMP(String, String, IRType, String, String), // res, cond, ty, op1, op2
    Call(Option<String>, IRType, String, Vec<(IRType, String)>), // res, res_ty, func_name, args
    Phi(String, IRType, Vec<(String, String)>), // res, ty, vars and labels
    Select(String, String, IRType, String, String), // res, cond, ty, val1, val2
    Label(String),

    // internal instructions for SSA implementation and register allocation
    Move(IRType, String, String), // ty, rd, rs
    SpillLoad(IRType, String, String), // ty, tmp, spill_var
    SpillStore(IRType, String, String), // ty, tmp, spill_var
    ArgLoad(IRType, String, i32), // ty, rd, offset
    ArgStore(IRType, String, i32), // ty, rs, offset
    CalleeProtect(Vec<String>), // protected args
    CalleeRecover(Vec<String>), // protected args
    CallerProtect(String, Vec<String>), // func_name, protected args
    CallerRecover(String, Vec<String>), // func_name, protected args
}

impl IRNode {
    pub fn is_terminator(&self) -> bool {
        match self {
            IRNode::Br(_) | IRNode::BrCond(_, _, _) | IRNode::Ret(_, _) => true,
            _ => false
        }
    }
    pub fn alloc_get_use_mut(&mut self) -> Vec<&mut String> {
        match self {
            IRNode::Binary(_, _, _, lhs, rhs) => {
                let mut vec = Vec::new();
                vec.push(lhs);
                vec.push(rhs);
                vec
            }
            IRNode::BrCond(cond, _, _) => {
                let mut vec = Vec::new();
                vec.push(cond);
                vec
            }
            IRNode::Load(_, _, ptr) => {
                let mut vec = Vec::new();
                vec.push(ptr);
                vec
            }
            IRNode::Store(_, val, ptr) => {
                let mut vec = Vec::new();
                vec.push(val);
                vec.push(ptr);
                vec
            }
            IRNode::GetElementPtr(_, _, ptr, indexes) => {
                let mut vec = Vec::new();
                vec.push(ptr);
                for (_, idx) in indexes {
                    vec.push(idx);
                }
                vec
            }
            IRNode::ICMP(_, cond, _, op1, op2) => {
                let mut vec = Vec::new();
                vec.push(cond);
                vec.push(op1);
                vec.push(op2);
                vec
            }
            IRNode::Move(_, _, rs) => {
                let mut vec = Vec::new();
                vec.push(rs);
                vec
            }
            IRNode::SpillStore(_, tmp, _) => {
                let mut vec = Vec::new();
                vec.push(tmp);
                vec
            }
            IRNode::ArgStore(_, rs, _) => {
                let mut vec = Vec::new();
                vec.push(rs);
                vec
            }
            _ => Vec::new()
        }.into_iter().filter(|x| {
            x.chars().next().unwrap() == '%'
        }).collect()
    }
    pub fn alloc_get_use(&self) -> HashSet<String> {
        match self {
            IRNode::Binary(_, _, _, lhs, rhs) => {
                let mut set = HashSet::new();
                set.insert(lhs.clone());
                set.insert(rhs.clone());
                set
            }
            IRNode::BrCond(cond, _, _) => {
                let mut set = HashSet::new();
                set.insert(cond.clone());
                set
            }
            IRNode::Load(_, _, ptr) => {
                let mut set = HashSet::new();
                set.insert(ptr.clone());
                set
            }
            IRNode::Store(_, val, ptr) => {
                let mut set = HashSet::new();
                set.insert(val.clone());
                set.insert(ptr.clone());
                set
            }
            IRNode::GetElementPtr(_, _, ptr, indexes) => {
                let mut set = HashSet::new();
                set.insert(ptr.clone());
                for (_, idx) in indexes {
                    set.insert(idx.clone());
                }
                set
            }
            IRNode::ICMP(_, cond, _, op1, op2) => {
                let mut set = HashSet::new();
                set.insert(cond.clone());
                set.insert(op1.clone());
                set.insert(op2.clone());
                set
            }
            IRNode::Call(_, _, _, args) => {
                let mut set = HashSet::new();
                for (i, _) in args.iter().enumerate() {
                    if i >= 8 {
                        break;
                    }
                    set.insert(format!("%a{}", i));
                }
                set
            }
            IRNode::Move(_, _, rs) => {
                let mut set = HashSet::new();
                set.insert(rs.clone());
                set
            }
            IRNode::SpillStore(_, tmp, _) => {
                let mut set = HashSet::new();
                set.insert(tmp.clone());
                set
            }
            IRNode::ArgStore(_, rs, _) => {
                let mut set = HashSet::new();
                set.insert(rs.clone());
                set
            }
            _ => HashSet::new()
        }.into_iter().filter(|x| {
            x.chars().next().unwrap() == '%'
        }).collect()
    }
    pub fn alloc_get_def_mut(&mut self) -> HashSet<&mut String> {
        match self {
            IRNode::Binary(res, _, _, _, _) => {
                let mut set = HashSet::new();
                set.insert(res);
                set
            }
            IRNode::Load(res, _, _) => {
                let mut set = HashSet::new();
                set.insert(res);
                set
            }
            IRNode::GetElementPtr(res, _, _, _) => {
                let mut set = HashSet::new();
                set.insert(res);
                set
            }
            IRNode::ICMP(res, _, _, _, _) => {
                let mut set = HashSet::new();
                set.insert(res);
                set
            }
            IRNode::Move(_, rd, _) => {
                let mut set = HashSet::new();
                set.insert(rd);
                set
            }
            IRNode::SpillLoad(_, tmp, _) => {
                let mut set = HashSet::new();
                set.insert(tmp);
                set
            }
            IRNode::ArgLoad(_, rd, _) => {
                let mut set = HashSet::new();
                set.insert(rd);
                set
            }
            _ => HashSet::new()
        }
    }
    pub fn alloc_get_def(&self) -> HashSet<String> {
        match self {
            IRNode::Binary(res, _, _, _, _) => {
                let mut set = HashSet::new();
                set.insert(res.clone());
                set
            }
            IRNode::Load(res, _, _) => {
                let mut set = HashSet::new();
                set.insert(res.clone());
                set
            }
            IRNode::GetElementPtr(res, _, _, _) => {
                let mut set = HashSet::new();
                set.insert(res.clone());
                set
            }
            IRNode::ICMP(res, _, _, _, _) => {
                let mut set = HashSet::new();
                set.insert(res.clone());
                set
            }
            IRNode::Call(Some(_), _, _, _) => {
                let mut set = HashSet::new();
                set.insert("%a0".to_string());
                set
            }
            IRNode::Move(_, rd, _) => {
                let mut set = HashSet::new();
                set.insert(rd.clone());
                set
            }
            IRNode::SpillLoad(_, tmp, _) => {
                let mut set = HashSet::new();
                set.insert(tmp.clone());
                set
            }
            IRNode::ArgLoad(_, rd, _) => {
                let mut set = HashSet::new();
                set.insert(rd.clone());
                set
            }
            _ => HashSet::new()
        }.into_iter().filter(|x| {
            x.chars().next().unwrap() == '%'
        }).collect()
    }

    pub fn get_use_mut(&mut self) -> Vec<&mut String> {
        match self {
            IRNode::Binary(_, _, _, lhs, rhs) => {
                let mut vec = Vec::new();
                vec.push(lhs);
                vec.push(rhs);
                vec
            }
            IRNode::BrCond(cond, _, _) => {
                let mut vec = Vec::new();
                vec.push(cond);
                vec
            }
            IRNode::Load(_, _, ptr) => {
                let mut vec = Vec::new();
                vec.push(ptr);
                vec
            }
            IRNode::Store(_, val, ptr) => {
                let mut vec = Vec::new();
                vec.push(val);
                vec.push(ptr);
                vec
            }
            IRNode::GetElementPtr(_, _, ptr, indexes) => {
                let mut vec = Vec::new();
                vec.push(ptr);
                for (_, idx) in indexes {
                    vec.push(idx);
                }
                vec
            }
            IRNode::ICMP(_, cond, _, op1, op2) => {
                let mut vec = Vec::new();
                vec.push(cond);
                vec.push(op1);
                vec.push(op2);
                vec
            }
            IRNode::Ret(_, Some(val)) => {
                let mut vec = Vec::new();
                vec.push(val);
                vec
            }
            IRNode::Call(_, _, _, args) => {
                let mut vec = Vec::new();
                vec.extend(args.iter_mut().map(|(_, arg)| arg));
                vec
            }
            IRNode::Move(_, _, rs) => {
                let mut vec = Vec::new();
                vec.push(rs);
                vec
            }
            _ => Vec::new()
        }.into_iter().filter(|x| {
            x.chars().next().unwrap() == '%'
        }).collect()
    }
    pub fn get_def_mut(&mut self) -> HashSet<&mut String> {
        match self {
            IRNode::Binary(res, _, _, _, _) => {
                let mut set = HashSet::new();
                set.insert(res);
                set
            }
            IRNode::Load(res, _, _) => {
                let mut set = HashSet::new();
                set.insert(res);
                set
            }
            IRNode::GetElementPtr(res, _, _, _) => {
                let mut set = HashSet::new();
                set.insert(res);
                set
            }
            IRNode::ICMP(res, _, _, _, _) => {
                let mut set = HashSet::new();
                set.insert(res);
                set
            }
            IRNode::Call(Some(res), _, _, _) => {
                let mut set = HashSet::new();
                set.insert(res);
                set
            }
            IRNode::Allocate(res, _) => {
                let mut set = HashSet::new();
                set.insert(res);
                set
            }
            IRNode::Move(_, rd, _) => {
                let mut set = HashSet::new();
                set.insert(rd);
                set
            }
            _ => HashSet::new()
        }.into_iter().filter(|x| {
            x.chars().next().unwrap() == '%'
        }).collect()
    }
    pub fn get_use(&self) -> HashSet<String>{
        match self {
            IRNode::Binary(_, _, _, lhs, rhs) => {
                let mut set = HashSet::new();
                set.insert(lhs.clone());
                set.insert(rhs.clone());
                set
            }
            IRNode::BrCond(cond, _, _) => {
                let mut set = HashSet::new();
                set.insert(cond.clone());
                set
            }
            IRNode::Load(_, _, ptr) => {
                let mut set = HashSet::new();
                set.insert(ptr.clone());
                set
            }
            IRNode::Store(_, val, ptr) => {
                let mut set = HashSet::new();
                set.insert(val.clone());
                set.insert(ptr.clone());
                set
            }
            IRNode::GetElementPtr(_, _, ptr, indexes) => {
                let mut set = HashSet::new();
                set.insert(ptr.clone());
                for (_, idx) in indexes {
                    set.insert(idx.clone());
                }
                set
            }
            IRNode::ICMP(_, cond, _, op1, op2) => {
                let mut set = HashSet::new();
                set.insert(cond.clone());
                set.insert(op1.clone());
                set.insert(op2.clone());
                set
            }
            IRNode::Ret(_, Some(val)) => {
                let mut set = HashSet::new();
                set.insert(val.clone());
                set
            }
            IRNode::Call(_, _, _, args) => {
                let mut set = HashSet::new();
                set.extend(args.iter().map(|(_, arg)| arg.clone()));
                set
            }
            IRNode::Move(_, _, rs) => {
                let mut set = HashSet::new();
                set.insert(rs.clone());
                set
            }
            _ => HashSet::new()
        }.into_iter().filter(|x| {
            x.chars().next().unwrap() == '%'
        }).collect()
    }
    pub fn get_def(&self) -> HashSet<String>{
        match self {
            IRNode::Binary(res, _, _, _, _) => {
                let mut set = HashSet::new();
                set.insert(res.clone());
                set
            }
            IRNode::Load(res, _, _) => {
                let mut set = HashSet::new();
                set.insert(res.clone());
                set
            }
            IRNode::GetElementPtr(res, _, _, _) => {
                let mut set = HashSet::new();
                set.insert(res.clone());
                set
            }
            IRNode::ICMP(res, _, _, _, _) => {
                let mut set = HashSet::new();
                set.insert(res.clone());
                set
            }
            IRNode::Call(Some(res), _, _, _) => {
                let mut set = HashSet::new();
                set.insert(res.clone());
                set
            }
            IRNode::Allocate(res, _) => {
                let mut set = HashSet::new();
                set.insert(res.clone());
                set
            }
            IRNode::Move(_, rd, _) => {
                let mut set = HashSet::new();
                set.insert(rd.clone());
                set
            }
            _ => HashSet::new()
        }.into_iter().filter(|x| {
            x.chars().next().unwrap() == '%'
        }).collect()
    }
    pub fn check_const(&self) -> Option<i32> {
        match self {
            IRNode::Binary(_, op, _, lhs, rhs) => {
                if let (Ok(lhs), Ok(rhs)) = (lhs.parse::<i32>(), rhs.parse::<i32>()) {
                    match op.as_str() {
                        "add" => lhs.checked_add(rhs),
                        "sub" => lhs.checked_sub(rhs),
                        "mul" => lhs.checked_mul(rhs),
                        "sdiv" =>lhs.checked_div(rhs),
                        "srem" => Some(lhs % rhs),
                        "shl" => Some(lhs << rhs),
                        "ashr" => Some(lhs >> rhs),
                        "and" => Some(lhs & rhs),
                        "or" => Some(lhs | rhs),
                        "xor" => Some(lhs ^ rhs),
                        _ => None
                    }
                } else {
                    if lhs == rhs{
                        match op.as_str(){
                            "sub" => Some(0),
                            "xor" => Some(0),
                            _ => None
                        }
                    }else{
                        None
                    }
                }
            },
            IRNode::ICMP(_, op, _, lhs, rhs)=>{
                if let (Ok(lhs), Ok(rhs)) = (lhs.parse::<i32>(), rhs.parse::<i32>()) {
                    match op.as_str() {
                        "eq" => Some((lhs == rhs) as i32),
                        "ne" => Some((lhs != rhs) as i32),
                        "slt" => Some((lhs < rhs) as i32),
                        "sle" => Some((lhs <= rhs) as i32),
                        "sgt" => Some((lhs > rhs) as i32),
                        "sge" => Some((lhs >= rhs) as i32),
                        _ => None
                    }
                } else {
                    if lhs == rhs{
                        match op.as_str(){
                            "eq" => Some(1),
                            "ne" => Some(0),
                            "slt" => Some(0),
                            "sle" => Some(1),
                            "sgt" => Some(0),
                            "sge" => Some(1),
                            _ => None
                        }
                    }else{
                        None
                    }
                }
            },
            IRNode::Move(_, _, rs) => {
                if let Ok(val) = rs.parse::<i32>() {
                    Some(val)
                } else {
                    None
                }
            },
            _ => None
        }
    }
}

impl Display for IRNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IRNode::Class(name, ch) => {
                write!(f, "{} = type {{{}}}\n", name, ch.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(", "))
            }
            IRNode::Global(name, ty, val) => {
                write!(f, "@{} = global {} {}\n", name, ty, val)
            }
            IRNode::Str(name, ty, val, _) => {
                write!(f, "@{} = private unnamed_addr constant {} c\"{}\\00\"\n", name, ty, val)
            }
            IRNode::FuncBegin(ret_ty, name, args) => {
                write!(
                    f,
                    "define dso_local {} @{}({}) {{\nentry:\n",
                    ret_ty,
                    name,
                    args.iter().map(
                        |(ty, name)| format!("{} {}", ty, name)
                    ).collect::<Vec<String>>().join(", ")
                )
            }
            IRNode::FuncEnd => {
                write!(f, "}}\n")
            }
            IRNode::Binary(res, op, ty, lhs, rhs) => {
                write!(f, "{} = {} {} {}, {}\n", res, op, ty, lhs, rhs)
            }
            IRNode::BrCond(cond, label1, label2) => {
                write!(f, "br i1 {}, label %{}, label %{}\n", cond, label1, label2)
            }
            IRNode::Br(label) => {
                write!(f, "br label %{}\n", label)
            }
            IRNode::Ret(ty, val) => {
                write!(f, "ret {} {}\n", ty, val.clone().unwrap_or("".to_string()))
            }
            IRNode::Allocate(res, ty) => {
                write!(f, "{} = alloca {}\n", res, ty)
            }
            IRNode::Load(res, ty, ptr) => {
                write!(f, "{} = load {}, ptr {}\n", res, ty, ptr)
            }
            IRNode::Store(ty, val, ptr) => {
                write!(f, "store {} {}, ptr {}\n", ty, val, ptr)
            }
            IRNode::GetElementPtr(res, ty, ptr, indexes) => {
                write!(
                    f,
                    "{} = getelementptr {}, ptr {}, {}\n",
                    res,
                    ty,
                    ptr,
                    indexes.iter().map(
                        |(ty, idx)| format!("{} {}", ty, idx)
                    ).collect::<Vec<String>>().join(", ")
                )
            }
            IRNode::ICMP(res, cond, ty, op1, op2) => {
                write!(f, "{} = icmp {} {} {}, {}\n", res, cond, ty, op1, op2)
            }
            IRNode::Call(res, res_ty, name, args) => {
                write!(
                    f,
                    "{}call {} @{}({})\n",
                    match res {
                        Some(res) => format!("{} = ", res),
                        None => "".to_string()
                    },
                    res_ty,
                    name,
                    args.iter().map(
                        |(ty, arg)| format!("{} {}", ty, arg)
                    ).collect::<Vec<String>>().join(", ")
                )
            }
            IRNode::Phi(res, ty, labels) => {
                write!(
                    f,
                    "{} = phi {} {}\n",
                    res,
                    ty,
                    labels.iter().map(
                        |(val, label)| format!("[{}, %{}]", val, label)
                    ).collect::<Vec<String>>().join(", ")
                )
            }
            IRNode::Select(res, cond, ty, val1, val2) => {
                write!(
                    f,
                    "{} = select i1 {}, {} {}, {} {}\n",
                    res,
                    cond,
                    ty,
                    val1,
                    ty,
                    val2
                )
            }
            IRNode::Label(label) => {
                write!(f, "{}:\n", label)
            }
            IRNode::Move(_, rd, rs) => {
                write!(f, "; ### Move {} <- {} ###\n", rd, rs)
            }
            IRNode::SpillLoad(_, tmp, spill_var) => {
                write!(f, "; ### Spill Load {} {} ###\n", tmp, spill_var)
            }
            IRNode::SpillStore(_, tmp, spill_var) => {
                write!(f, "; ### Spill Store {} {} ###\n", tmp, spill_var)
            }
            IRNode::ArgLoad(_, rd, _) => {
                write!(f, "; ### Arg Load {} ###\n", rd)
            }
            IRNode::ArgStore(_, rs, _) => {
                write!(f, "; ### Arg Store {} ###\n", rs)
            }
            IRNode::CalleeProtect(args) => {
                write!(f, "; ### CalleeProtect {:?} ###\n", args)
            }
            IRNode::CalleeRecover(args) => {
                write!(f, "; ### CalleeRecover {:?} ###\n", args)
            }
            IRNode::CallerProtect(name, args) => {
                write!(f, "; ### CallerProtect {} {:?} ###\n", name, args)
            }
            IRNode::CallerRecover(name, args) => {
                write!(f, "; ### CallerRecover {} {:?} ###\n", name, args)
            }
        }
    }
}