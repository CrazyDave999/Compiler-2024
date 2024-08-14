use std::fmt::Display;
use super::ast::Type;

#[derive(Clone)]
pub enum IRType {
    PTR(Box<IRType>),
    Var(String, Vec<i32>),
}

impl IRType {
    pub fn from(ty: &Type) -> Self {
        if ty.dim > 0 {
            IRType::PTR(Box::from(IRType::from(&Type {
                name: ty.name,
                dim: ty.dim - 1,
            })))
        } else {
            match ty.name {
                "int" => IRType::i32(),
                "bool" => IRType::i1(),
                "void" => IRType::void(),
                _ => IRType::PTR(Box::from(IRType::class(&ty.name))),
            }
        }
    }
    pub fn i32() -> Self {
        IRType::Var("i32".to_string(), vec![])
    }
    pub fn i1() -> Self {
        IRType::Var("i1".to_string(), vec![])
    }

    pub fn void() -> Self {
        IRType::Var("void".to_string(), vec![])
    }

    pub fn class(name: &str) -> Self {
        IRType::Var(format!("%class.{}", name.to_string()), vec![])
    }

    pub fn get_class_name(&self) -> String {
        match self {
            IRType::PTR(ty) => ty.get_class_name(),
            IRType::Var(name, _) => name.clone(),
        }
    }

    // 提供基本类型的大小
    pub fn size(&self) -> i32 {
        match self {
            IRType::PTR(_) => 4,
            IRType::Var(name, sizes) => {
                let mut size: i32 = match name.as_str() {
                    "i32" => 4,
                    "i1" => 1,
                    _ => unreachable!()
                };
                for s in sizes {
                    size *= s;
                }
                size
            }
        }
    }
}

impl Display for IRType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IRType::PTR(_) => write!(f, "ptr"),
            IRType::Var(name, sizes) => {
                if sizes.is_empty() {
                    write!(f, "{}", name)
                } else {
                    write!(f, "[{} x {}]", sizes[0], IRType::Var(name.clone(), sizes[1..].to_vec()))
                }
            }
        }
    }
}
