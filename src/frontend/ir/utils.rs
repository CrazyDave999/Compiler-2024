use std::fmt::Display;
use super::ast::Type;

pub enum IRType {
    PTR,
    Var(String, Vec<i32>),
}

impl IRType {
    pub fn from(ty: &Type) -> Self {
        let mut sizes = vec![];
        for _ in 0..ty.dim {
            sizes.push(-1);
        }
        match ty.name {
            "int" => IRType::Var("i32".to_string(), sizes),
            "bool" => IRType::Var("i1".to_string(), sizes),
            "void" => IRType::Var("void".to_string(), sizes),
            _ => IRType::Var(format!("%class.{}", ty.name), sizes)
        }
    }
}

impl Display for IRType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IRType::PTR => write!(f, "ptr"),
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

pub struct Counter {
    cnt: i32,
}