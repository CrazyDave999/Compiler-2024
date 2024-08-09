use std::collections::HashMap;
use super::Type;
use super::utils::ExprInfo;

#[derive(Clone)]
pub(crate) enum Member<'a> {
    Var(Type<'a>),
    Func(Type<'a>, Vec<Type<'a>>),
}
pub(crate) enum ScopeType<'a> {
    Global,
    Func,
    Class(&'a str),
    Block,
    Loop,
}
struct ScopeLayer<'a> {
    ty: ScopeType<'a>,
    var: HashMap<&'a str, Type<'a>>, // type
    func: HashMap<&'a str, (Type<'a>, Vec<Type<'a>>)>, // (return type, args)
    class: HashMap<&'a str, HashMap<&'a str, Member<'a>>>, // members
}

impl<'a> ScopeLayer<'a> {
    pub fn new(scope_type: ScopeType<'a>) -> Self {
        ScopeLayer {
            ty: scope_type,
            var: HashMap::new(),
            func: HashMap::new(),
            class: HashMap::new(),
        }
    }
}

pub(crate) struct Scope<'a> {
    layers: Vec<ScopeLayer<'a>>,
}
// 找一个变量，先找类成员，再找当前作用域
impl<'a> Scope<'a> {
    pub fn new() -> Self {
        let mut scope = Scope {
            layers: vec![ScopeLayer::new(ScopeType::Global)],
        };

        // builtin types
        scope.insert_class("int", HashMap::new());
        scope.insert_class("bool", HashMap::new());

        let mut str_hash_map = HashMap::new();
        str_hash_map.insert("length", Member::Func(Type::int(), vec![]));
        str_hash_map.insert("substring", Member::Func(Type::string(), vec![Type::int(), Type::int()]));
        str_hash_map.insert("parseInt", Member::Func(Type::int(), vec![]));
        str_hash_map.insert("ord", Member::Func(Type::int(), vec![Type::int()]));

        scope.insert_class("string", str_hash_map);

        // builtin functions
        scope.insert_func("print", &Type::void(), &vec![Type::string()]);
        scope.insert_func("println", &Type::void(), &vec![Type::string()]);
        scope.insert_func("printInt", &Type::void(), &vec![Type::int()]);
        scope.insert_func("printlnInt", &Type::void(), &vec![Type::int()]);
        scope.insert_func("getString", &Type::string(), &vec![]);
        scope.insert_func("getInt", &Type::int(), &vec![]);
        scope.insert_func("toString", &Type::string(), &vec![Type::int()]);
        scope
    }
    pub fn push(&mut self, ty: ScopeType<'a>) {
        self.layers.push(ScopeLayer::new(ty));
    }
    pub fn pop(&mut self) {
        self.layers.pop();
    }

    fn top(&mut self) -> &mut ScopeLayer<'a> {
        self.layers.last_mut().unwrap()
    }

    pub fn insert_var(&mut self, name: &'a str, ty: Type<'a>) {
        self.top().var.insert(name, ty);
    }

    pub fn insert_func(&mut self, name: &'a str, ty: &Type<'a>, args: &Vec<Type<'a>>) {
        self.top().func.insert(name, (ty.clone(), args.clone()));
    }

    pub fn insert_class(&mut self, name: &'a str, members: HashMap<&'a str, Member<'a>>) {
        self.top().class.insert(name, members.clone());
    }

    pub fn find_ident(&self, name: &'a str) -> Option<ExprInfo<'a>> {
        for layer in self.layers.iter().rev() {
            if let Some(ty) = layer.var.get(name) {
                return Some(ExprInfo {
                    ty: ty.clone(),
                    is_left: true,
                    is_const: false,
                    mem: if let ScopeType::Class(class_name) = layer.ty {
                        Some((Some(class_name), Some(name)))
                    } else {
                        None
                    },
                    gb_func: None,
                });
            }
            if let Some(_) = layer.func.get(name) {
                return Some(ExprInfo {
                    ty: Type::func(),
                    is_left: false,
                    is_const: false,
                    mem: if let ScopeType::Class(class_name) = layer.ty {
                        Some((Some(class_name), Some(name)))
                    } else {
                        None
                    },
                    gb_func: if let ScopeType::Global = layer.ty {
                        Some(name)
                    } else {
                        None
                    },
                });
            }
        }
        None
    }

    pub fn find_var_top(&mut self, name: &'a str) -> Option<Type<'a>> {
        self.top().var.get(name).cloned()
    }

    pub fn find_gb_func(&self, name: &'a str) -> Option<(Type<'a>, Vec<Type<'a>>)> {
        self.layers[0].func.get(name).cloned()
    }

    pub fn find_class(&self, name: &'a str) -> Option<&HashMap<&'a str, Member<'a>>> {
        self.layers[0].class.get(name)
    }
    pub fn get_member(&self, class_name: &'a str, mem_name: &'a str) -> Option<Member<'a>> {
        if let Some(members) = self.find_class(class_name) {
            members.get(mem_name).cloned()
        } else {
            None
        }
    }

    pub fn get_class_name(&self) -> Option<&'a str> {
        if self.layers.len() < 1 {
            return None;
        }
        match self.layers[1].ty {
            ScopeType::Class(name) => Some(name),
            _ => None
        }
    }

    pub fn is_in_func(&self) -> bool {
        for layer in self.layers.iter().rev() {
            if let ScopeType::Func = layer.ty {
                return true;
            }
        }
        false
    }
    pub fn is_in_loop(&self) -> bool {
        for layer in self.layers.iter().rev() {
            if let ScopeType::Loop = layer.ty {
                return true;
            }
        }
        false
    }
}