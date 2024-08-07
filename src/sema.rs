use std::collections::HashMap;
use crate::ast::{ASTNode, Type};
use crate::sema::ScopeType::{Class, Func, Global};

struct Context<'a> {
    ty: Type<'a>, // 当前表达式的类型
    is_left: bool, // 当前表达式是否左值
    class_name: &'a mut str, // 当前类名
}
#[derive(Clone)]
enum Member<'a> {
    Var(Type<'a>),
    Func(Type<'a>, Vec<Type<'a>>),
}
enum ScopeType {
    Global,
    Func,
    Class,
    Block,
}
struct ScopeLayer<'a> {
    ty: ScopeType,
    var: HashMap<&'a str, Type<'a>>, // type
    func: HashMap<&'a str, (Type<'a>, Vec<Type<'a>>)>, // (return type, args)
    class: HashMap<&'a str, HashMap<&'a str, Member<'a>>>, // members
}

impl<'a> ScopeLayer<'a> {
    pub fn new(scope_type: ScopeType) -> Self {
        ScopeLayer {
            ty: scope_type,
            var: HashMap::new(),
            func: HashMap::new(),
            class: HashMap::new(),
        }
    }
}

struct Scope<'a> {
    layers: Vec<ScopeLayer<'a>>,
}
// 找一个变量，先找类成员，再找当前作用域
impl<'a> Scope<'a> {
    pub fn new() -> Self {
        let mut scope = Scope {
            layers: vec![ScopeLayer::new(Global)],
        };
        // builtin types
        scope.insert_class("int", HashMap::new());
        scope.insert_class("bool", HashMap::new());
        scope.insert_class("string", HashMap::new());

        // builtin functions
        scope.insert_func("print", &Type { name: "void", dim: 0 }, &vec![Type { name: "string", dim: 0 }]);
        scope.insert_func("println", &Type { name: "void", dim: 0 }, &vec![Type { name: "string", dim: 0 }]);
        scope.insert_func("printInt", &Type { name: "void", dim: 0 }, &vec![Type { name: "int", dim: 0 }]);
        scope.insert_func("printlnInt", &Type { name: "void", dim: 0 }, &vec![Type { name: "int", dim: 0 }]);
        scope.insert_func("getString", &Type { name: "string", dim: 0 }, &vec![]);
        scope.insert_func("getInt", &Type { name: "int", dim: 0 }, &vec![]);
        scope.insert_func("toString", &Type { name: "string", dim: 0 }, &vec![Type { name: "int", dim: 0 }]);
        scope
    }
    pub fn push(&mut self, ty: ScopeType) {
        self.layers.push(ScopeLayer::new(ty));
    }
    pub fn pop(&mut self) {
        self.layers.pop();
    }
    pub fn index(&self) -> usize {
        self.layers.len() - 1
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

    pub fn find_var(&self, name: &'a str) -> Option<&Type<'a>> {
        for layer in self.layers.iter().rev() {
            if let Some(ty) = layer.var.get(name) {
                return Some(ty);
            }
        }
        None
    }

    pub fn find_func(&self, name: &'a str) -> Option<&(Type<'a>, Vec<Type<'a>>)> {
        self.layers[0].func.get(name)
    }

    pub fn find_class(&self, name: &'a str) -> Option<&HashMap<&'a str, Member<'a>>> {
        self.layers[0].class.get(name)
    }
}

pub fn check(ast: &ASTNode) -> bool {
    true
}

fn dfs<'a>(ast: &ASTNode<'a>, ctx: &mut Context<'a>, scope: &mut Scope<'a>) -> bool {
    match ast {
        ASTNode::Root(ch) => {
            // 先收集所有的函数定义及类定义，加入作用域
            for node in ch {
                match node {
                    ASTNode::FuncDef(ty, name, args, block) => {
                        if let Some(_) = scope.find_func(name) {
                            return false;
                        }
                        if let Some(_) = scope.find_class(name) {
                            return false;
                        }
                        let args: Vec<Type> = args.iter().map(|(ty, _)| ty.clone()).collect();
                        scope.insert_func(name, ty, &args);
                    }
                    ASTNode::ClassDef(name, ch) => {
                        if let Some(_) = scope.find_func(name) {
                            return false;
                        }
                        if let Some(_) = scope.find_class(name) {
                            return false;
                        }
                        let mut members = HashMap::new();
                        let mut constr = false;
                        for mem in ch {
                            match mem {
                                ASTNode::ConstrDef(name, block) => {
                                    if constr {
                                        // Two construct function
                                        return false;
                                    }
                                    constr = true;
                                }
                                ASTNode::VarDecl(ty, ch) => {
                                    for (name, op) in ch {
                                        if members.contains_key(name) {
                                            // 重复定义
                                            return false;
                                        }
                                        if let Some(_) = op {
                                            // 类成员默认初始化表达式为非法
                                            return false;
                                        }
                                        members.insert(name.clone(), Member::Var(ty.clone()));
                                    }
                                }
                                ASTNode::FuncDef(ty, name, args, _) => {
                                    if members.contains_key(name) {
                                        // 重复定义
                                        return false;
                                    }
                                    let args = args.iter().map(|(ty, name)| ty.clone()).collect();
                                    members.insert(name, Member::Func(ty.clone(), args));
                                }
                                _ => { unreachable!() }
                            }
                        }
                        scope.insert_class(name, members);
                    }
                    _ => {}
                }
            }
            for node in ch {
                if !dfs(node, ctx, scope) {
                    return false;
                }
            }
            true
        }
        ASTNode::VarDecl(ty, ch) => {
            for (name, op) in ch {
                if let Some(_) = scope.find_func(name) {
                    // 变量名和函数名不能重复
                    return false;
                }
                if let Some(expr) = op {
                    if !dfs(expr, ctx, scope) {
                        return false;
                    }
                    if *ty != ctx.ty {
                        // 声明类型和初始化表达式类型不匹配
                        return false;
                    }
                }
                scope.insert_var(name, ty.clone());
            }
            true
        }
        ASTNode::FuncDef(ty, name, args, block) => {
            // 考虑形参可以与函数重名
            scope.push(Func);
            for (ty, name) in args {
                scope.insert_var(name, ty.clone());
            }
            if !dfs(block, ctx, scope) {
                return false;
            }
            scope.pop();
            true
        }
        ASTNode::ConstrDef(name, block) => {
            scope.push(Func);
            if !dfs(block, ctx, scope) {
                return false;
            }
            scope.pop();
            true
        }
        ASTNode::ClassDef(name, block) => {
            scope.push(Class);
            ctx.class_name = &mut name.clone();
            for mem in block {
                match mem {
                    ASTNode::FuncDef(_, _, _, _) => {
                        if !dfs(mem, ctx, scope) {
                            return false;
                        }
                    }
                    _ => unreachable!()
                }
            }
            ctx.class_name = &mut "";
            scope.pop();
            true
        }
        ASTNode::Block(ch) => {
            scope.push(ScopeType::Block);
            for node in ch {
                if !dfs(node, ctx, scope) {
                    return false;
                }
            }
            scope.pop();
            true
        }
        ASTNode::ThisExpr => {
            true
        }
        ASTNode::ArrayInit(name, sizes, op) => {
            if let Some(_) = scope.find_class(name) {
                let mut my_type = Type { name, dim: sizes.len() as i32 };
                if let Some(expr) = op {
                    if !dfs(expr, ctx, scope) {
                        return false;
                    }
                    if my_type != ctx.ty {
                        return false;
                    }
                }
                ctx.ty = my_type;
                true
            } else {
                false
            }
        }
        ASTNode::ClassInit(name) => {
            if let Some(_) = scope.find_class(name) {
                ctx.ty = Type { name, dim: 0 };
                true
            } else {
                false
            }
        }
        ASTNode::BinaryExpr(name, lhs, rhs) => {
            if !dfs(lhs, ctx, scope) {
                return false;
            }
            let lhs_ty = ctx.ty.clone();
            let lhs_left = ctx.is_left;
            if !dfs(rhs, ctx, scope) {
                return false;
            }
            let rhs_ty = ctx.ty.clone();
            if lhs_ty != rhs_ty {
                // 先解决类型不等的情况
                if *name == "=" {
                    return if lhs_left && rhs_ty.name == "null" {
                        ctx.ty = lhs_ty;
                        true
                    } else {
                        false
                    };
                }

                if *name == "==" || *name == "!=" {
                    return if !((lhs_ty.dim > 0 && rhs_ty.name == "null") || (rhs_ty.dim > 0 && lhs_ty.name == "null")) {
                        false
                    } else {
                        ctx.ty = Type { name: "bool", dim: 0 };
                        true
                    };
                }
            }
            if lhs_ty.name == "null" || rhs_ty.name == "null" {
                return false;
            }
            // 类型相等
            match *name {
                "+" => {
                    if lhs_ty.dim == 0 && (lhs_ty.name == "int" || lhs_ty.name == "string") {
                        ctx.ty = lhs_ty;
                        true
                    } else {
                        false
                    }
                }
                "-" | "*" | "/" | "%" => {
                    if lhs_ty.dim == 0 && lhs_ty.name == "int" {
                        ctx.ty = lhs_ty;
                        true
                    } else {
                        false
                    }
                }
                "<" | ">" | "<=" | ">=" => {
                    if lhs_ty.dim == 0 && (lhs_ty.name == "int" || lhs_ty.name == "string") {
                        ctx.ty = Type { name: "bool", dim: 0 };
                        true
                    } else {
                        false
                    }
                }
                "==" | "!=" => {
                    ctx.ty = Type { name: "bool", dim: 0 };
                    true
                }
                "&&" | "||" => {
                    if lhs_ty.dim == 0 && lhs_ty.name == "bool" {
                        ctx.ty = lhs_ty;
                        true
                    } else {
                        false
                    }
                }
                "<<" | ">>" | "&" | "|" | "^" => {
                    if lhs_ty.dim == 0 && lhs_ty.name == "int" {
                        ctx.ty = lhs_ty;
                        true
                    } else {
                        false
                    }
                }
                "=" => {
                    if lhs_left {
                        ctx.ty = lhs_ty;
                        true
                    } else {
                        false
                    }
                }
                _ => { false }
            }
        }
        ASTNode::UnitaryExpr(name, rhs) => {
            if !dfs(rhs, ctx, scope) {
                return false;
            }
            let rhs_ty = ctx.ty.clone();
            let rhs_left = ctx.is_left;
            if rhs_left && rhs_ty.name == "int" && rhs_ty.dim == 0 {
                true
            } else {
                false
            }
        }
        ASTNode::TernaryExpr(cond, expr1, expr2) => {
            if !dfs(cond, ctx, scope) {
                return false;
            }
            let cond_ty = ctx.ty.clone();
            if cond_ty.name == "bool" && cond_ty.dim == 0 {
                if !dfs(expr1, ctx, scope) {
                    return false;
                }
                let expr1_ty = ctx.ty.clone();
                if !dfs(expr2, ctx, scope) {
                    return false;
                }
                let expr2_ty = ctx.ty.clone();
                if expr1_ty == expr2_ty {
                    ctx.ty = expr1_ty;
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }
        ASTNode::Increment(lhs, op) => {
            if !dfs(lhs, ctx, scope) {
                return false;
            }
            let lhs_ty = ctx.ty.clone();
            let lhs_left = ctx.is_left;
            if lhs_left && lhs_ty.name == "int" && lhs_ty.dim == 0 {
                ctx.is_left = false;
                true
            } else {
                false
            }
        }
        ASTNode::ArrayAccess(lhs, inner) => {
            if !dfs(lhs, ctx, scope) {
                return false;
            }
            let lhs_ty = ctx.ty.clone();
            if lhs_ty.dim > 0 {
                if !dfs(inner, ctx, scope) {
                    return false;
                }
                if ctx.ty.name == "int" && ctx.ty.dim == 0 {
                    ctx.ty = Type { name: lhs_ty.name, dim: lhs_ty.dim - 1 };
                    ctx.is_left = true;
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }
        ASTNode::MemberAccess(lhs, name) => { true }
        ASTNode::FuncCall(lhs, params) => { true }
        ASTNode::IfStmt(cond, if_block, else_block) => { true }
        ASTNode::ForStmt(expr1, expr2, expr3, block) => { true }
        ASTNode::WhileStmt(cond, block) => { true }
        ASTNode::ReturnStmt(ret) => { true }
        ASTNode::BreakStmt => { true }
        ASTNode::ContinueStmt => { true }
        ASTNode::NULL => { true }
        ASTNode::Int(val) => { true }
        ASTNode::Str(s) => { true }
        ASTNode::Bool(b) => { true }
        ASTNode::ArrConst(ch) => { true }
        ASTNode::FmtStr(ch) => { true }
        ASTNode::Ident(name) => { true }
    }
}