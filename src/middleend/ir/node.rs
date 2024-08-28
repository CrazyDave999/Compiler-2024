use std::fmt::{Display, Formatter};
use super::IRType;
#[derive(Clone)]
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
    Move(String, String), // rd, rs
}

impl IRNode {
    pub fn is_terminator(&self) -> bool {
        match self {
            IRNode::Br(_) | IRNode::BrCond(_, _, _) => true,
            _ => false
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
            IRNode::Move(rd, rs) => {
                write!(f, "; ### Move {} <- {} ###\n", rd, rs)
            }
        }
    }
}