use std::fmt::Display;
use super::ast::Type;

#[derive(Clone)]
pub enum IRType {
    PTR(String),
    Var(String, Vec<i32>),
}

impl IRType {
    pub fn from(ty: &Type) -> Self {
        if ty.dim > 0 {
            IRType::PTR(String::from(""))
        } else {
            match ty.name {
                "int" => IRType::i32(),
                "bool" => IRType::i1(),
                "void" => IRType::void(),
                _ => IRType::PTR(ty.name.to_string()),
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
