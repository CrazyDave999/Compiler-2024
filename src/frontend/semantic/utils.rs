use super::Type;

pub struct Context<'a> {
    pub ret_types: Vec<Type<'a>>,
}

impl Context<'_> {
    pub fn new() -> Self {
        Context {
            ret_types: vec![],
        }
    }
}

#[derive(PartialEq)]
pub struct ExprInfo<'a> {
    pub ty: Type<'a>,
    pub is_left: bool,
    pub func: Option<(Type<'a>, Vec<Type<'a>>)>, // ret type, args
    pub idx: i32, // for IR, 不是-1说明是成员
    pub cnt: i32, // for IR
    pub is_global: bool, // for IR, 是否是全局变量
}

impl<'a> ExprInfo<'a> {
    pub fn void() -> Self {
        ExprInfo {
            ty: Type::void(),
            is_left: false,
            func: None,
            idx: -1,
            cnt: -1,
            is_global: false,
        }
    }

    pub fn normal_var(name: &'a str) -> Self {
        ExprInfo {
            ty: Type { name, dim: 0 },
            is_left: false,
            func: None,
            idx: -1,
            cnt: -1,
            is_global: false,
        }
    }
    pub fn normal(ty: Type<'a>) -> Self {
        ExprInfo {
            ty,
            is_left: false,
            func: None,
            idx: -1,
            cnt: -1,
            is_global: false,
        }
    }

    pub fn left(ty: Type<'a>) -> Self {
        ExprInfo {
            ty,
            is_left: true,
            func: None,
            idx: -1,
            cnt: -1,
            is_global: false,
        }
    }
}
