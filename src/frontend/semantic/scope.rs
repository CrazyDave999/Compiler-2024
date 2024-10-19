use super::utils::ExprInfo;
use super::Type;
use std::collections::HashMap;

#[derive(Clone)]
pub(crate) enum Member<'a> {
    Var(Type<'a>, i32, i32),
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
    map: HashMap<&'a str, Member<'a>>,
}

impl<'a> ScopeLayer<'a> {
    pub fn new(scope_type: ScopeType<'a>) -> Self {
        ScopeLayer {
            ty: scope_type,
            map: HashMap::new(),
        }
    }
}

pub(crate) struct Scope<'a> {
    layers: Vec<ScopeLayer<'a>>,
    class: HashMap<&'a str, HashMap<&'a str, Member<'a>>>, // members
    pub cnt: i32,
}
// 找一个变量，先找类成员，再找当前作用域
impl<'a> Scope<'a> {
    pub fn new() -> Self {
        let mut scope = Scope {
            layers: vec![ScopeLayer::new(ScopeType::Global)],
            class: HashMap::new(),
            cnt: 0,
        };

        // builtin types
        scope.insert_class("int", HashMap::new());
        scope.insert_class("bool", HashMap::new());

        let mut str_hash_map = HashMap::new();
        str_hash_map.insert("length", Member::Func(Type::int(), vec![]));
        str_hash_map.insert(
            "substring",
            Member::Func(Type::string(), vec![Type::int(), Type::int()]),
        );
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

    pub fn insert_var(&mut self, name: &'a str, ty: Type<'a>, idx: i32) -> i32 {
        let cnt = self.cnt;
        self.top().map.insert(name, Member::Var(ty, idx, cnt));
        self.cnt += 1;
        cnt
    }

    pub fn insert_func(&mut self, name: &'a str, ty: &Type<'a>, args: &Vec<Type<'a>>) {
        self.top()
            .map
            .insert(name, Member::Func(ty.clone(), args.clone()));
    }

    pub fn insert_class(&mut self, name: &'a str, members: HashMap<&'a str, Member<'a>>) {
        self.class.insert(name, members.clone());
    }

    pub fn find_ident(&self, name: &'a str) -> Option<ExprInfo<'a>> {
        for layer in self.layers.iter().rev() {
            if let Some(member) = layer.map.get(name) {
                return match member {
                    Member::Var(ty, idx, cnt) => Some(ExprInfo {
                        ty: ty.clone(),
                        is_left: true,
                        func: None,
                        idx: *idx,
                        cnt: *cnt,
                        is_global: match layer.ty {
                            ScopeType::Global => true,
                            _ => false,
                        },
                    }),
                    Member::Func(ty, args) => Some(ExprInfo {
                        ty: match layer.ty {
                            ScopeType::Class(_) => Type::method(),
                            ScopeType::Global => Type::func(),
                            _ => unreachable!(),
                        },
                        is_left: false,
                        func: Some((ty.clone(), args.clone())),
                        idx: -1,
                        cnt: -1,
                        is_global: false,
                    }),
                };
            }
        }
        None
    }
    pub fn find_ident_top(&mut self, name: &'a str) -> Option<Member<'a>> {
        self.top().map.get(name).cloned()
    }

    pub fn find_class(&self, name: &'a str) -> Option<&HashMap<&'a str, Member<'a>>> {
        self.class.get(name)
    }

    pub fn get_class_name(&self) -> Option<&'a str> {
        if self.layers.len() < 1 {
            return None;
        }
        match self.layers[1].ty {
            ScopeType::Class(name) => Some(name),
            _ => None,
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
