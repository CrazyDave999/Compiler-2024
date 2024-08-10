#[derive(Debug)]
#[derive(Clone, Copy)]
pub struct Type<'a> {
    pub name: &'a str,
    pub dim: i32,
}


impl<'a> PartialEq for Type<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.dim == other.dim
    }
}

impl<'a> Type<'a> {
    pub fn is_primitive(&self) -> bool {
        self.dim == 0 && (self.name == "int" || self.name == "bool" || self.name == "string")
    }
    pub fn int() -> Self {
        Type { name: "int", dim: 0 }
    }

    pub fn is_int(&self) -> bool {
        self.name == "int" && self.dim == 0
    }
    pub fn bool() -> Self {
        Type { name: "bool", dim: 0 }
    }

    pub fn is_bool(&self) -> bool {
        self.name == "bool" && self.dim == 0
    }
    pub fn string() -> Self {
        Type { name: "string", dim: 0 }
    }
    pub fn is_string(&self) -> bool {
        self.name == "string" && self.dim == 0
    }
    pub fn void() -> Self {
        Type { name: "void", dim: 0 }
    }

    pub fn is_void(&self) -> bool {
        self.name == "void"
    }

    pub fn null() -> Self {
        Type { name: "null", dim: 0 }
    }

    pub fn is_null(&self) -> bool {
        self.name == "null"
    }

    pub fn func() -> Self {
        Type { name: "#FUNC#", dim: 0 }
    }
}