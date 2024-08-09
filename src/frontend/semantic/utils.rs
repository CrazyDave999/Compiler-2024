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
    pub mem: Option<(&'a str, &'a str)>, // class name, member name
    pub gb_func: Option<&'a str>, // function name
}

impl<'a> ExprInfo<'a> {
    pub fn void() -> Self {
        ExprInfo {
            ty: Type::void(),
            is_left: false,
            mem: None,
            gb_func: None,
        }
    }

    pub fn normal_var(name: &'a str) -> Self {
        ExprInfo {
            ty: Type { name, dim: 0 },
            is_left: false,
            mem: None,
            gb_func: None,
        }
    }
    pub fn normal(ty: Type<'a>) -> Self {
        ExprInfo {
            ty,
            is_left: false,
            mem: None,
            gb_func: None,
        }
    }


}
