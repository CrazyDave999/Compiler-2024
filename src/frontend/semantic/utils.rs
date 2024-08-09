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
    pub is_const: bool,
    pub mem: Option<(Option<&'a str>, Option<&'a str>)>, // class name, member name
    pub gb_func: Option<&'a str>, // function name
}

impl<'a> ExprInfo<'a> {
    pub fn void() -> Self {
        ExprInfo {
            ty: Type { name: "void", dim: 0 },
            is_left: false,
            is_const: false,
            mem: None,
            gb_func: None,
        }
    }
}
