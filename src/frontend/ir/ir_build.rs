use std::collections::HashMap;
use super::IRType;
use super::IRNode;
use super::ast::ASTNode;

struct Context {
    index: i32,
    cnt: i32, // 中间变量
    class_name: Option<String>,
    size_map: HashMap<String, i32>,
}
impl Context {
    pub fn new() -> Self {
        Context {
            index: 0,
            cnt: 0,
            class_name: None,
            size_map: HashMap::new(),
        }
    }
    pub fn push(&mut self) {
        self.index += 1;
    }
    pub fn pop(&mut self) {
        self.index -= 1;
    }
    pub fn global_var(&self, name: &str) -> String {
        format!("@{}", name)
    }

    pub fn local_var_def(&self, name: &str) -> String {
        format!("%{}.{}", name, self.index)
    }
    pub fn local_var_use(&self, name: &str, layer: i32) -> String {
        format!("%{}.{}", name, layer)
    }

    pub fn param(&self, name: &str) -> String {
        format!("%{}.{}.param", name, self.index)
    }

    pub fn func_def(&self, name: &str) -> String {
        if let Some(class_name) = &self.class_name {
            format!("@{}.{}", class_name, name)
        } else {
            format!("@{}", name)
        }
    }
    pub fn func_use(&self, name: &str, class_name: Option<String>) -> String {
        if let Some(class_name) = class_name {
            format!("@{}.{}", class_name, name)
        } else {
            format!("@{}", name)
        }
    }
    pub fn class_use(&self, name: &str) -> String {
        format!("%class.{}", name)
    }
    pub fn class_use_this(&self) -> String {
        format!("%class.{}", self.class_name.as_ref().unwrap())
    }
    pub fn is_global(&self) -> bool {
        self.index == 0
    }
    pub fn generate(&mut self) -> String {
        let res = self.cnt;
        self.cnt += 1;
        format!("%v{}", res)
    }

    pub fn insert_class_size(&mut self, name: &str, size: i32) {
        self.size_map.insert(name.to_string(), size);
    }
    pub fn size_of(&self, name: &str) -> i32 {
        *self.size_map.get(name).unwrap()
    }
}

struct IRInfo {
    pub ty: IRType,
    pub left_ir_name: String,
    pub right_ir_name: Option<String>, // 不一定每次都要load
    pub lhs_ir_name: Option<String>,
    pub lhs_ty: IRType,
}

impl IRInfo {
    pub fn void() -> Self {
        IRInfo {
            ty: IRType::Var("void".to_string(), vec![]),
            left_ir_name: String::from(""),
            right_ir_name: None,
            lhs_ir_name: None,
            lhs_ty: IRType::Var("void".to_string(), vec![]),
        }
    }
    pub fn get_right_ir_name(&self, ctx: &mut Context, func_defs: &mut Vec<IRNode>) -> String {
        match &self.right_ir_name {
            Some(s) => s.clone(),
            None => {
                let res_name = ctx.generate();
                func_defs.push(IRNode::Load(
                    res_name.clone(),
                    self.ty.clone(),
                    self.left_ir_name.clone(),
                ));
                res_name
            }
        }
    }
}

pub fn build_ir<'a>(ast: &'a ASTNode<'a>) -> Vec<IRNode> {
    let mut ctx = Context::new();
    let mut class_defs = Vec::new();
    let mut var_decls = Vec::new();
    let mut func_defs = Vec::new();
    let mut res = Vec::new();
    dfs(ast, &mut ctx, &mut class_defs, &mut var_decls, &mut func_defs);
    for ch in class_defs {
        res.push(ch);
    }
    for ch in var_decls {
        res.push(ch);
    }
    for ch in func_defs {
        res.push(ch);
    }
    res
}

fn dfs<'a>(ast: &ASTNode<'a>, ctx: &mut Context, class_defs: &mut Vec<IRNode>, var_decls: &mut Vec<IRNode>, func_defs: &mut Vec<IRNode>) -> IRInfo {
    match ast {
        ASTNode::Root(ch, _) => {
            for node in ch {
                dfs(node, ctx, class_defs, var_decls, func_defs);
            }
            IRInfo::void()
        }
        ASTNode::ClassDef(name, ch, _) => {
            ctx.class_name = Some(name.to_string());
            ctx.push();
            let mut fields = Vec::new();
            let mut size = 0;
            for node in ch {
                match node {
                    ASTNode::VarDecl(ty, vars, _) => {
                        for _ in 0..vars.len() {
                            let ir_ty = IRType::from(ty);
                            fields.push(ir_ty.clone());
                            size += ir_ty.size();
                        }
                    }
                    _ => {}
                }
            }
            ctx.insert_class_size(name, size);
            for node in ch {
                match node {
                    ASTNode::FuncDef(_, _, _, _, _) => { dfs(node, ctx, class_defs, var_decls, func_defs); }
                    ASTNode::ConstrDef(_, _, _) => { dfs(node, ctx, class_defs, var_decls, func_defs); }
                    _ => {}
                }
            }
            ctx.pop();
            ctx.class_name = None;
            class_defs.push(IRNode::Class(format!("%class.{}", name), fields));
            IRInfo::void()
        }
        ASTNode::FuncDef(ret_ty, name, args, block, _) => {
            ctx.push();

            let mut args_ = Vec::new();
            if let Some(class_name) = ctx.class_name.as_ref() {
                args_.push((IRType::PTR(Box::from(IRType::class(class_name))), String::from("%this")));
            }
            for (ty, name) in args {
                args_.push((IRType::from(ty), ctx.param(name)));
            }

            func_defs.push(IRNode::FuncBegin(IRType::from(ret_ty), ctx.func_def(*name), args_));

            for (ty, name) in args {
                func_defs.push(IRNode::Allocate(ctx.local_var_def(name), IRType::from(ty)));
                func_defs.push(IRNode::Store(IRType::from(ty), ctx.param(name), ctx.local_var_def(name)));
            }

            dfs(block, ctx, class_defs, var_decls, func_defs);

            func_defs.push(IRNode::FuncEnd);
            ctx.pop();
            IRInfo::void()
        }
        ASTNode::ConstrDef(name, block, _) => {
            IRInfo::void()
        }
        ASTNode::VarDecl(ty, vars, _) => {
            if ctx.is_global() {
                for (name, _) in vars {
                    var_decls.push(IRNode::Global(ctx.global_var(name), IRType::from(ty), "0".to_string()));
                }
            } else {
                for (name, expr) in vars {
                    func_defs.push(IRNode::Allocate(ctx.local_var_def(name), IRType::from(ty)));
                    if let Some(expr) = expr {
                        let expr_info = dfs(expr, ctx, class_defs, var_decls, func_defs);
                        let right_ir_name = expr_info.get_right_ir_name(ctx, func_defs);
                        func_defs.push(IRNode::Store(
                            IRType::from(ty),
                            right_ir_name,
                            ctx.local_var_def(name),
                        ));
                    }
                }
            }
            IRInfo::void()
        }
        ASTNode::Block(ch, _) => {
            ctx.push();
            for node in ch {
                dfs(node, ctx, class_defs, var_decls, func_defs);
            }
            ctx.pop();
            IRInfo::void()
        }
        ASTNode::ThisExpr(_) => {
            IRInfo {
                ty: IRType::PTR(Box::from(IRType::class(ctx.class_name.as_ref().unwrap()))),
                left_ir_name: String::from(""),
                right_ir_name: Some(String::from("%this")),
                lhs_ir_name: None,
                lhs_ty: IRType::void(),
            }
        }
        ASTNode::ArrayInit(name, sizes, op, _) => {
            if let Some(arr_const) = op {
                unreachable!()
            } else {
                alloc_arr_by_sizes(name, sizes, ctx, class_defs, var_decls, func_defs)
            }
        }
        ASTNode::ClassInit(name, _) => {
            let res_name = ctx.generate();
            let res_ty = IRType::PTR(Box::from(IRType::class(name)));
            func_defs.push(IRNode::Call(
                Some(res_name.clone()),
                res_ty.clone(),
                String::from("@malloc"),
                vec![(IRType::i32(), ctx.size_of(name).to_string())],
            ));
            IRInfo {
                ty: res_ty.clone(),
                left_ir_name: res_name.clone(),
                right_ir_name: Some(res_name),
                lhs_ir_name: None,
                lhs_ty: IRType::void(),
            }
        }
        ASTNode::BinaryExpr(op, lhs, rhs, _) => {
            let lhs_info = dfs(lhs, ctx, class_defs, var_decls, func_defs);
            let rhs_info = dfs(rhs, ctx, class_defs, var_decls, func_defs);
            match *op {
                "+" => {
                    let res_ty = lhs_info.ty.clone();
                    let lhs_ir_name = lhs_info.get_right_ir_name(ctx, func_defs);
                    let rhs_ir_name = rhs_info.get_right_ir_name(ctx, func_defs);
                    let res_name = ctx.generate();
                    func_defs.push(IRNode::Binary(
                        res_name.clone(),
                        "add".to_string(),
                        res_ty.clone(),
                        lhs_ir_name,
                        rhs_ir_name,
                    ));
                    IRInfo {
                        ty: res_ty,
                        left_ir_name: String::from(""),
                        right_ir_name: Some(res_name),
                        lhs_ir_name: None,
                        lhs_ty: IRType::void(),
                    }
                }
                "-" | "*" | "/" | "%" | "<<" | ">>" | "&" | "|" | "^" => {
                    let res_ty = lhs_info.ty.clone();
                    let lhs_ir_name = lhs_info.get_right_ir_name(ctx, func_defs);
                    let rhs_ir_name = rhs_info.get_right_ir_name(ctx, func_defs);
                    let res_name = ctx.generate();
                    func_defs.push(IRNode::Binary(
                        res_name.clone(),
                        match *op {
                            "-" => "sub",
                            "*" => "mul",
                            "/" => "sdiv",
                            "%" => "srem",
                            "<<" => "shl",
                            ">>" => "ashr",
                            "&" => "and",
                            "|" => "or",
                            "^" => "xor",
                            _ => unreachable!(),
                        }.to_string(),
                        res_ty.clone(),
                        lhs_ir_name,
                        rhs_ir_name,
                    ));
                    IRInfo {
                        ty: res_ty,
                        left_ir_name: String::from(""),
                        right_ir_name: Some(res_name),
                        lhs_ir_name: None,
                        lhs_ty: IRType::void(),
                    }
                }
                "=" => {
                    let rhs_ir_name = rhs_info.get_right_ir_name(ctx, func_defs);
                    func_defs.push(IRNode::Store(
                        lhs_info.ty.clone(),
                        rhs_ir_name,
                        lhs_info.left_ir_name,
                    ));
                    IRInfo::void()
                }
                _ => {
                    IRInfo::void()
                }
            }
        }
        ASTNode::ArrayAccess(lhs, expr, _, ty) => {
            let lhs_info = dfs(lhs, ctx, class_defs, var_decls, func_defs);
            let expr_info = dfs(expr, ctx, class_defs, var_decls, func_defs);
            let res_ty = IRType::from(ty);
            let lhs_ir_name = lhs_info.get_right_ir_name(ctx, func_defs);
            let expr_ir_name = expr_info.get_right_ir_name(ctx, func_defs);
            let left_ir_name_ = ctx.generate();
            func_defs.push(IRNode::GetElementPtr(
                left_ir_name_.clone(),
                res_ty.clone(),
                lhs_ir_name,
                vec![(IRType::i32(), expr_ir_name)],
            ));

            IRInfo {
                ty: res_ty,
                left_ir_name: left_ir_name_.clone(),
                right_ir_name: None,
                lhs_ir_name: None,
                lhs_ty: IRType::void(),
            }
        }
        ASTNode::MemberAccess(lhs, name, _, idx, ty) => {
            let lhs_info = dfs(lhs, ctx, class_defs, var_decls, func_defs);
            match ty.name {
                "#METHOD#" => {
                    IRInfo {
                        ty: IRType::from(ty),
                        left_ir_name: String::from(""),
                        right_ir_name: Some(ctx.func_use(*name, Some(lhs_info.ty.get_class_name()))),
                        lhs_ir_name: Some(lhs_info.get_right_ir_name(ctx, func_defs)),
                        lhs_ty: lhs_info.ty,
                    }
                }
                _ => {
                    let res_ty = IRType::from(ty);
                    let lhs_ir_name = lhs_info.get_right_ir_name(ctx, func_defs);
                    let ptr_name = ctx.generate();
                    func_defs.push(IRNode::GetElementPtr(
                        ptr_name.clone(),
                        IRType::Var(lhs_info.ty.get_ir_class_name(), vec![]),
                        lhs_ir_name,
                        vec![(IRType::i32(), String::from("0")), (IRType::i32(), (*idx).to_string())],
                    ));

                    IRInfo {
                        ty: res_ty,
                        left_ir_name: ptr_name.clone(),
                        right_ir_name: None,
                        lhs_ir_name: None,
                        lhs_ty: lhs_info.ty,
                    }
                }
            }
        }
        ASTNode::FuncCall(lhs, params, _, ret_ty) => {
            let lhs_info = dfs(lhs, ctx, class_defs, var_decls, func_defs);
            let mut args = Vec::new();
            let lhs_ir_name = lhs_info.get_right_ir_name(ctx, func_defs);
            if let Some(class_name) = lhs_info.lhs_ir_name {
                args.push((lhs_info.lhs_ty, class_name));
            }
            for param in params {
                let param_info = dfs(param, ctx, class_defs, var_decls, func_defs);
                let param_ir_name = param_info.get_right_ir_name(ctx, func_defs);
                args.push((param_info.ty, param_ir_name));
            }
            let res_name = ctx.generate();
            let res_ty = IRType::from(ret_ty);

            if ret_ty.is_void() {
                func_defs.push(IRNode::Call(
                    None,
                    res_ty.clone(),
                    lhs_ir_name,
                    args,
                ));
            } else {
                func_defs.push(IRNode::Call(
                    Some(res_name.clone()),
                    res_ty.clone(),
                    lhs_ir_name,
                    args,
                ));
            }
            IRInfo {
                ty: res_ty,
                left_ir_name: res_name.clone(),
                right_ir_name: Some(res_name),
                lhs_ir_name: None,
                lhs_ty: IRType::void(),
            }
        }
        ASTNode::ReturnStmt(ret, _) => {
            if let Some(expr) = ret {
                let expr_info = dfs(expr, ctx, class_defs, var_decls, func_defs);
                let expr_ir_name = expr_info.get_right_ir_name(ctx, func_defs);
                func_defs.push(IRNode::Ret(
                    expr_info.ty,
                    Some(expr_ir_name),
                ));
                IRInfo::void()
            } else {
                func_defs.push(IRNode::Ret(IRType::Var(String::from("void"), vec![]), None));
                IRInfo::void()
            }
        }
        ASTNode::Int(val, _) => {
            IRInfo {
                ty: IRType::i32(),
                left_ir_name: String::from(""),
                right_ir_name: Some(val.to_string()),
                lhs_ir_name: None,
                lhs_ty: IRType::void(),
            }
        }
        ASTNode::Bool(b, _) => {
            IRInfo {
                ty: IRType::i1(),
                left_ir_name: String::from(""),
                right_ir_name: match b {
                    true => Some("1".to_string()),
                    false => Some("0".to_string()),
                },
                lhs_ir_name: None,
                lhs_ty: IRType::void(),
            }
        }

        ASTNode::Ident(name, _, idx, ty, layer) => {
            if *idx != -1 {
                // 成员
                let ptr_name = ctx.generate();
                let res_ty = IRType::from(ty);
                func_defs.push(IRNode::GetElementPtr(
                    ptr_name.clone(),
                    IRType::Var(ctx.class_use_this(), vec![]),
                    String::from("%this"),
                    vec![(IRType::i32(), String::from("0")), (IRType::i32(), (*idx).to_string())],
                ));

                IRInfo {
                    ty: res_ty,
                    left_ir_name: ptr_name.clone(),
                    right_ir_name: None,
                    lhs_ir_name: None,
                    lhs_ty: IRType::void(),
                }
            } else {
                match ty.name {
                    "#FUNC#" => {
                        IRInfo {
                            ty: IRType::from(ty),
                            left_ir_name: String::from(""),
                            right_ir_name: Some(ctx.func_use(name, None)),
                            lhs_ir_name: None,
                            lhs_ty: IRType::void(),
                        }
                    }
                    "#METHOD#" => {
                        IRInfo {
                            ty: IRType::from(ty),
                            left_ir_name: String::from(""),
                            right_ir_name: Some(ctx.func_use(name, Some(ctx.class_name.as_ref().unwrap().clone()))),
                            lhs_ir_name: Some(String::from("%this")),
                            lhs_ty: IRType::void(),
                        }
                    }
                    _ => {
                        IRInfo {
                            ty: IRType::from(ty),
                            left_ir_name: ctx.local_var_use(name, *layer),
                            right_ir_name: None,
                            lhs_ir_name: None,
                            lhs_ty: IRType::void(),
                        }
                    }
                }
            }
        }
        _ => {
            IRInfo::void()
        }
    }
}

fn alloc_arr_by_sizes<'a>(name: &str, sizes: &[Option<ASTNode>], ctx: &mut Context, class_defs: &mut Vec<IRNode>, var_decls: &mut Vec<IRNode>, func_defs: &mut Vec<IRNode>) -> IRInfo {
    let expr_info = dfs(&sizes[0].as_ref().unwrap(), ctx, class_defs, var_decls, func_defs);
    if sizes.len() == 1 {
        let res_ty = IRType::from_str(name);
        let expr_ir_name = expr_info.get_right_ir_name(ctx, func_defs);
        let mul_res_name = ctx.generate();

        func_defs.push(IRNode::Binary(
            mul_res_name.clone(),
            String::from("mul"),
            IRType::i32(),
            expr_ir_name,
            res_ty.size().to_string(),
        ));
        let res_name = ctx.generate();
        func_defs.push(IRNode::Call(
            Some(res_name.clone()),
            IRType::PTR(Box::from(IRType::from_str(name))),
            String::from("@malloc"),
            vec![(IRType::i32(), mul_res_name)],
        ));
        IRInfo {
            ty: IRType::PTR(Box::from(res_ty)),
            left_ir_name: res_name.clone(),
            right_ir_name: None,
            lhs_ir_name: None,
            lhs_ty: IRType::void(),
        }
    } else {
        unreachable!()
    }
}