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

    pub fn from_str(name: &str) -> Self {
        match name {
            "int" => IRType::i32(),
            "bool" => IRType::i1(),
            "void" => IRType::void(),
            _ => IRType::PTR(Box::from(IRType::class(name))),
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
        IRType::Var(name.to_string(), vec![])
    }

    pub fn get_class_name(&self) -> String {
        match self {
            IRType::PTR(ty) => ty.get_class_name(),
            IRType::Var(name, _) => name.clone(),
        }
    }
    pub fn get_ir_class_name(&self) -> String {
        match self {
            IRType::PTR(ty) => ty.get_ir_class_name(),
            IRType::Var(name, _) => format!("%class.{}", name),
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
                    _ => 0
                };
                for s in sizes {
                    size *= s;
                }
                size
            }
        }
    }

    pub fn default_value(&self) -> Option<String> {
        match self {
            IRType::PTR(_) => Some("null".to_string()),
            IRType::Var(name, _) => {
                match name.as_str() {
                    "i32" => Some("0".to_string()),
                    "i1" => Some("0".to_string()),
                    "void" => None,
                    _ => unreachable!()
                }
            }
        }
    }
    pub fn is_void(&self) -> bool {
        match self {
            IRType::Var(name, _) => name == "void",
            _ => false,
        }
    }

    pub fn is_i32(&self) -> bool {
        match self {
            IRType::Var(name, sh) => name == "i32" && sh.is_empty(),
            _ => false,
        }
    }

    pub fn is_i1(&self) -> bool {
        match self {
            IRType::Var(name, sh) => name == "i1" && sh.is_empty(),
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            IRType::PTR(ty) => {
                match &**ty {
                    IRType::Var(name, sh) => name == "string" && sh.is_empty(),
                    _ => false,
                }
            }
            _ => false,
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

pub fn escape_string(input: &str) -> (String, i32) {
    let c1 = input.matches("\\\"").count() as i32;
    let c2 = input.matches("\\n").count() as i32;
    let c3 = input.matches("\\\\").count() as i32;
    let result = input
        .replace("\\\"", "\\22")
        .replace("\\n", "\\0A");

    (result, input.len() as i32 - c1 - c2 - c3)
}