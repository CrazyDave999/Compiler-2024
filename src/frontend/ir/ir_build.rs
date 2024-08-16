use std::collections::HashMap;
use super::IRType;
use super::IRNode;
use super::ast::ASTNode;

struct Context {
    index: i32,
    cnt: i32, // 中间变量
    if_cnt: i32,
    for_cnt: i32,
    while_cnt: i32,
    str_cnt: i32,
    land_cnt: i32,
    class_name: Option<String>,
    size_map: HashMap<String, i32>,
    last_label: String,
    class_defs:  Vec<IRNode>,
    var_decls:  Vec<IRNode>,
    func_defs: Vec<IRNode>
}
impl Context {
    pub fn new() -> Self {
        Context {
            index: 0,
            cnt: 0,
            if_cnt: 0,
            for_cnt: 0,
            while_cnt: 0,
            str_cnt: 0,
            land_cnt: 0,
            class_name: None,
            size_map: HashMap::new(),
            last_label: String::from(""),
            class_defs: Vec::new(),
            var_decls: Vec::new(),
            func_defs: Vec::new(),
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
        if layer == 0 {
            format!("@{}", name)
        } else {
            format!("%{}.{}", name, layer)
        }
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

    pub fn generate_if(&mut self) -> String {
        let res = self.if_cnt;
        self.if_cnt += 1;
        res.to_string()
    }

    pub fn generate_for(&mut self) -> String {
        let res = self.for_cnt;
        self.for_cnt += 1;
        res.to_string()
    }
    pub fn generate_while(&mut self) -> String {
        let res = self.while_cnt;
        self.while_cnt += 1;
        res.to_string()
    }

    pub fn generate_str(&mut self) -> String {
        let res = self.str_cnt;
        self.str_cnt += 1;
        res.to_string()
    }

    pub fn generate_land(&mut self) -> String {
        let res = self.land_cnt;
        self.land_cnt += 1;
        res.to_string()
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
    pub fn get_right_ir_name(&self, ctx: &mut Context) -> String {
        match &self.right_ir_name {
            Some(s) => s.clone(),
            None => {
                let res_name = ctx.generate();
                ctx.func_defs.push(IRNode::Load(
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
    let mut res = Vec::new();
    dfs(ast, &mut ctx);
    for ch in ctx.class_defs {
        res.push(ch);
    }
    for ch in ctx.var_decls {
        res.push(ch);
    }
    for ch in ctx.func_defs {
        res.push(ch);
    }
    res
}

fn dfs<'a>(ast: &ASTNode<'a>, ctx: &mut Context) -> IRInfo {
    match ast {
        ASTNode::Root(ch, _) => {
            for node in ch {
                dfs(node, ctx);
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
                    ASTNode::FuncDef(_, _, _, _, _) => { dfs(node, ctx); }
                    ASTNode::ConstrDef(_, _, _) => { dfs(node, ctx); }
                    _ => {}
                }
            }
            ctx.pop();
            ctx.class_name = None;
            ctx.class_defs.push(IRNode::Class(format!("%class.{}", name), fields));
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

            ctx.func_defs.push(IRNode::FuncBegin(IRType::from(ret_ty), ctx.func_def(*name), args_));
            ctx.last_label = String::from("entry");
            for (ty, name) in args {
                ctx.func_defs.push(IRNode::Allocate(ctx.local_var_def(name), IRType::from(ty)));
                ctx.func_defs.push(IRNode::Store(IRType::from(ty), ctx.param(name), ctx.local_var_def(name)));
            }

            dfs(block, ctx);

            ctx.func_defs.push(IRNode::FuncEnd);
            ctx.pop();
            IRInfo::void()
        }
        ASTNode::ConstrDef(name, block, _) => {
            IRInfo::void()
        }
        ASTNode::VarDecl(ty, vars, _) => {
            if ctx.is_global() {
                let ir_type = IRType::from(ty);
                for (name, _) in vars {
                    ctx.var_decls.push(IRNode::Global(
                        ctx.global_var(name),
                        ir_type.clone(),
                        match &ir_type {
                            IRType::Var(name, _) => match name.as_str() {
                                "i32" => String::from("0"),
                                "i1" => String::from("0"),
                                _ => unreachable!()
                            },
                            IRType::PTR(_) => String::from("null")
                        },
                    ));
                }
            } else {
                for (name, expr) in vars {
                    ctx.func_defs.push(IRNode::Allocate(ctx.local_var_def(name), IRType::from(ty)));
                    if let Some(expr) = expr {
                        let expr_info = dfs(expr, ctx);
                        let right_ir_name = expr_info.get_right_ir_name(ctx);
                        ctx.func_defs.push(IRNode::Store(
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
                dfs(node, ctx);
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
                alloc_arr_by_sizes(name, sizes, ctx)
            }
        }
        ASTNode::ClassInit(name, _) => {
            let res_name = ctx.generate();
            let res_ty = IRType::PTR(Box::from(IRType::class(name)));
            ctx.func_defs.push(IRNode::Call(
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
            let lhs_info = dfs(lhs, ctx);
            match *op {
                "+" => {
                    let rhs_info = dfs(rhs, ctx);
                    let res_ty = lhs_info.ty.clone();
                    let lhs_ir_name = lhs_info.get_right_ir_name(ctx);
                    let rhs_ir_name = rhs_info.get_right_ir_name(ctx);
                    let res_name = ctx.generate();

                    match &res_ty {
                        IRType::Var(_, _) => {
                            // i32
                            ctx.func_defs.push(IRNode::Binary(
                                res_name.clone(),
                                "add".to_string(),
                                res_ty.clone(),
                                lhs_ir_name,
                                rhs_ir_name,
                            ));
                        }
                        IRType::PTR(_) => {
                            // string
                            ctx.func_defs.push(IRNode::Call(
                                Some(res_name.clone()),
                                res_ty.clone(),
                                String::from("@string.add"),
                                vec![
                                    (IRType::PTR(Box::from(IRType::class("string"))), lhs_ir_name),
                                    (IRType::PTR(Box::from(IRType::class("string"))), rhs_ir_name),
                                ],
                            ))
                        }
                    }

                    IRInfo {
                        ty: res_ty,
                        left_ir_name: String::from(""),
                        right_ir_name: Some(res_name),
                        lhs_ir_name: None,
                        lhs_ty: IRType::void(),
                    }
                }
                "-" | "*" | "/" | "%" | "<<" | ">>" | "&" | "|" | "^" => {
                    let rhs_info = dfs(rhs, ctx);
                    let res_ty = lhs_info.ty.clone();
                    let lhs_ir_name = lhs_info.get_right_ir_name(ctx);
                    let rhs_ir_name = rhs_info.get_right_ir_name(ctx);
                    let res_name = ctx.generate();
                    ctx.func_defs.push(IRNode::Binary(
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
                "<" | "<=" | ">" | ">=" | "==" | "!=" => {
                    let rhs_info = dfs(rhs, ctx);
                    let res_ty = IRType::i1();
                    let lhs_ir_name = lhs_info.get_right_ir_name(ctx);
                    let rhs_ir_name = rhs_info.get_right_ir_name(ctx);
                    let res_name = ctx.generate();
                    match &lhs_info.ty {
                        IRType::Var(_, _) => {
                            // i32
                            ctx.func_defs.push(IRNode::ICMP(
                                res_name.clone(),
                                match *op {
                                    "<" => "slt",
                                    "<=" => "sle",
                                    ">" => "sgt",
                                    ">=" => "sge",
                                    "==" => "eq",
                                    "!=" => "ne",
                                    _ => unreachable!(),
                                }.to_string(),
                                lhs_info.ty.clone(),
                                lhs_ir_name,
                                rhs_ir_name,
                            ));
                        }
                        IRType::PTR(_) => {
                            // string
                            ctx.func_defs.push(IRNode::Call(
                                Some(res_name.clone()),
                                res_ty.clone(),
                                match *op {
                                    "==" => "@string.eq",
                                    "!=" => "@string.ne",
                                    "<" => "@string.lt",
                                    "<=" => "@string.le",
                                    ">" => "@string.gt",
                                    ">=" => "@string.ge",
                                    _ => unreachable!(),
                                }.to_string(),
                                vec![
                                    (IRType::PTR(Box::from(IRType::class("string"))), lhs_ir_name),
                                    (IRType::PTR(Box::from(IRType::class("string"))), rhs_ir_name),
                                ],
                            ));
                        }
                    }

                    IRInfo {
                        ty: res_ty,
                        left_ir_name: String::from(""),
                        right_ir_name: Some(res_name),
                        lhs_ir_name: None,
                        lhs_ty: IRType::void(),
                    }
                }
                "&&" => {
                    let last_label = ctx.last_label.clone();
                    let land_cnt = ctx.generate_land();
                    let land_true_label = format!("land.true.{}", land_cnt);
                    let land_end_label = format!("land.end.{}", land_cnt);
                    let lhs_ir_name = lhs_info.get_right_ir_name(ctx);
                    ctx.func_defs.push(IRNode::BrCond(
                        lhs_ir_name.clone(),
                        land_true_label.clone(),
                        land_end_label.clone(),
                    ));
                    ctx.func_defs.push(IRNode::Label(land_true_label.clone()));
                    ctx.last_label = land_true_label.clone();
                    let rhs_info = dfs(rhs, ctx);
                    let rhs_ir_name = rhs_info.get_right_ir_name(ctx);
                    ctx.func_defs.push(IRNode::Br(
                        land_end_label.clone(),
                    ));
                    ctx.func_defs.push(IRNode::Label(land_end_label.clone()));
                    let res_name = ctx.generate();
                    ctx.func_defs.push(IRNode::Phi(
                        res_name.clone(),
                        IRType::i1(),
                        vec![
                            (String::from("false"), last_label),
                            (rhs_ir_name, ctx.last_label.clone()),
                        ],
                    ));
                    ctx.last_label = land_end_label;
                    IRInfo {
                        ty: IRType::i1(),
                        left_ir_name: String::from(""),
                        right_ir_name: Some(res_name),
                        lhs_ir_name: None,
                        lhs_ty: IRType::void(),
                    }
                }
                "=" => {
                    let rhs_info = dfs(rhs, ctx);
                    let rhs_ir_name = rhs_info.get_right_ir_name(ctx);
                    ctx.func_defs.push(IRNode::Store(
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
        ASTNode::UnitaryExpr(op, rhs, _) => {
            let rhs_info = dfs(rhs, ctx);
            let res_ty = rhs_info.ty.clone();
            let rhs_ir_name = rhs_info.get_right_ir_name(ctx);
            let res_name = ctx.generate();
            match *op {
                "++" | "--" => {
                    ctx.func_defs.push(IRNode::Binary(
                        res_name.clone(),
                        match *op {
                            "++" => "add",
                            "--" => "sub",
                            _ => unreachable!(),
                        }.to_string(),
                        res_ty.clone(),
                        rhs_ir_name,
                        String::from("1"),
                    ));
                    ctx.func_defs.push(IRNode::Store(
                        res_ty.clone(),
                        res_name.clone(),
                        rhs_info.left_ir_name.clone(),
                    ));
                }
                "!" => {
                    ctx.func_defs.push(IRNode::Binary(
                        res_name.clone(),
                        "xor".to_string(),
                        res_ty.clone(),
                        rhs_ir_name,
                        String::from("1"),
                    ));
                }
                "-" => {
                    ctx.func_defs.push(IRNode::Binary(
                        res_name.clone(),
                        "sub".to_string(),
                        res_ty.clone(),
                        String::from("0"),
                        rhs_ir_name,
                    ));
                }
                "~" => {
                    ctx.func_defs.push(IRNode::Binary(
                        res_name.clone(),
                        "xor".to_string(),
                        res_ty.clone(),
                        rhs_ir_name,
                        String::from("-1"),
                    ));
                }
                _ => unreachable!()
            }
            IRInfo {
                ty: res_ty,
                left_ir_name: rhs_info.left_ir_name,
                right_ir_name: Some(res_name),
                lhs_ir_name: None,
                lhs_ty: IRType::void(),
            }
        }
        ASTNode::Increment(lhs, op, _) => {
            let lhs_info = dfs(lhs, ctx);
            let lhs_ir_name = lhs_info.get_right_ir_name(ctx);
            let res_ty = lhs_info.ty.clone();
            let res_name = ctx.generate();
            ctx.func_defs.push(IRNode::Binary(
                res_name.clone(),
                match *op {
                    "++" => "add",
                    "--" => "sub",
                    _ => unreachable!(),
                }.to_string(),
                res_ty.clone(),
                lhs_ir_name.clone(),
                String::from("1"),
            ));
            ctx.func_defs.push(IRNode::Store(
                res_ty.clone(),
                res_name.clone(),
                lhs_info.left_ir_name.clone(),
            ));
            IRInfo {
                ty: res_ty,
                left_ir_name: lhs_info.left_ir_name.clone(),
                right_ir_name: Some(lhs_ir_name),
                lhs_ir_name: None,
                lhs_ty: IRType::void(),
            }
        }
        ASTNode::ArrayAccess(lhs, expr, _, ty) => {
            let lhs_info = dfs(lhs, ctx);
            let expr_info = dfs(expr, ctx);
            let res_ty = IRType::from(ty);
            let lhs_ir_name = lhs_info.get_right_ir_name(ctx);
            let expr_ir_name = expr_info.get_right_ir_name(ctx);
            let left_ir_name_ = ctx.generate();
            ctx.func_defs.push(IRNode::GetElementPtr(
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
            let lhs_info = dfs(lhs, ctx);
            match ty.name {
                "#METHOD#" => {
                    IRInfo {
                        ty: IRType::from(ty),
                        left_ir_name: String::from(""),
                        right_ir_name: Some(ctx.func_use(*name, Some(lhs_info.ty.get_class_name()))),
                        lhs_ir_name: Some(lhs_info.get_right_ir_name(ctx)),
                        lhs_ty: lhs_info.ty,
                    }
                }
                _ => {
                    let res_ty = IRType::from(ty);
                    let lhs_ir_name = lhs_info.get_right_ir_name(ctx);
                    let ptr_name = ctx.generate();
                    ctx.func_defs.push(IRNode::GetElementPtr(
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
            let lhs_info = dfs(lhs, ctx);
            let mut args = Vec::new();
            let lhs_ir_name = lhs_info.get_right_ir_name(ctx);
            if let Some(class_name) = lhs_info.lhs_ir_name {
                args.push((lhs_info.lhs_ty, class_name));
            }
            for param in params {
                let param_info = dfs(param, ctx);
                let param_ir_name = param_info.get_right_ir_name(ctx);
                args.push((param_info.ty, param_ir_name));
            }
            let res_name = ctx.generate();
            let res_ty = IRType::from(ret_ty);

            if ret_ty.is_void() {
                ctx.func_defs.push(IRNode::Call(
                    None,
                    res_ty.clone(),
                    lhs_ir_name,
                    args,
                ));
            } else {
                ctx.func_defs.push(IRNode::Call(
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
        ASTNode::IfStmt(cond, if_block, else_block, _) => {
            let cond_info = dfs(cond, ctx);
            let cond_ir_name = cond_info.get_right_ir_name(ctx);
            let if_cnt = ctx.generate_if();
            let if_label = format!("if.then.{}", if_cnt);
            let else_label = format!("if.else.{}", if_cnt);
            let end_label = format!("if.end.{}", if_cnt);
            ctx.func_defs.push(IRNode::BrCond(
                cond_ir_name,
                if_label.clone(),
                match else_block {
                    Some(_) => else_label.clone(),
                    None => end_label.clone(),
                },
            ));
            ctx.func_defs.push(IRNode::Label(if_label.clone()));
            dfs(if_block, ctx);
            ctx.func_defs.push(IRNode::Br(end_label.clone()));
            if let Some(else_block) = else_block {
                ctx.func_defs.push(IRNode::Label(else_label.clone()));
                dfs(else_block, ctx);
                ctx.func_defs.push(IRNode::Br(end_label.clone()));
            }
            ctx.func_defs.push(IRNode::Label(end_label.clone()));
            ctx.last_label = end_label;
            IRInfo::void()
        }
        ASTNode::ForStmt(expr1, expr2, expr3, block, _) => {
            ctx.push();
            let for_cnt = ctx.generate_for();
            if let Some(expr1) = expr1 {
                dfs(expr1, ctx);
            }
            let cond_label = format!("for.cond.{}", for_cnt);
            let body_label = format!("for.body.{}", for_cnt);
            let inc_label = format!("for.inc.{}", for_cnt);
            let end_label = format!("for.end.{}", for_cnt);
            ctx.func_defs.push(IRNode::Br(cond_label.clone()));
            ctx.func_defs.push(IRNode::Label(cond_label.clone()));
            if let Some(expr2) = expr2 {
                let cond_info = dfs(expr2, ctx);
                let cond_ir_name = cond_info.get_right_ir_name(ctx);
                ctx.func_defs.push(IRNode::BrCond(
                    cond_ir_name,
                    body_label.clone(),
                    end_label.clone(),
                ));
            } else {
                ctx.func_defs.push(IRNode::Br(body_label.clone()));
            }
            ctx.func_defs.push(IRNode::Label(body_label.clone()));
            ctx.last_label = body_label.clone();
            dfs(block, ctx);
            ctx.func_defs.push(IRNode::Br(inc_label.clone()));
            ctx.func_defs.push(IRNode::Label(inc_label.clone()));
            ctx.last_label = inc_label.clone();
            if let Some(expr3) = expr3 {
                dfs(expr3, ctx);
            }
            ctx.func_defs.push(IRNode::Br(cond_label.clone()));
            ctx.func_defs.push(IRNode::Label(end_label.clone()));
            ctx.pop();
            ctx.last_label = end_label;
            IRInfo::void()
        }
        ASTNode::WhileStmt(cond, block, _) => {
            ctx.push();
            let while_cnt = ctx.generate_while();
            let cond_label = format!("while.cond.{}", while_cnt);
            let body_label = format!("while.body.{}", while_cnt);
            let end_label = format!("while.end.{}", while_cnt);
            ctx.func_defs.push(IRNode::Br(cond_label.clone()));
            ctx.func_defs.push(IRNode::Label(cond_label.clone()));
            ctx.last_label = cond_label.clone();
            if let Some(cond) = cond {
                let cond_info = dfs(cond, ctx);
                let cond_ir_name = cond_info.get_right_ir_name(ctx);
                ctx.func_defs.push(IRNode::BrCond(
                    cond_ir_name,
                    body_label.clone(),
                    end_label.clone(),
                ));
            }
            ctx.func_defs.push(IRNode::Label(body_label.clone()));
            ctx.last_label = body_label.clone();
            dfs(block, ctx);
            ctx.func_defs.push(IRNode::Br(cond_label.clone()));
            ctx.func_defs.push(IRNode::Label(end_label.clone()));
            ctx.last_label = end_label;
            ctx.pop();
            IRInfo::void()
        }
        ASTNode::ReturnStmt(ret, _) => {
            if let Some(expr) = ret {
                let expr_info = dfs(expr, ctx);
                let expr_ir_name = expr_info.get_right_ir_name(ctx);
                ctx.func_defs.push(IRNode::Ret(
                    expr_info.ty,
                    Some(expr_ir_name),
                ));
                IRInfo::void()
            } else {
                ctx.func_defs.push(IRNode::Ret(IRType::Var(String::from("void"), vec![]), None));
                IRInfo::void()
            }
        }
        ASTNode::Str(s, _) => {
            let str_cnt = ctx.generate_str();
            ctx.var_decls.push(IRNode::Str(
                format!("@.str.{}", str_cnt),
                IRType::Var(String::from("i8"), vec![s.len() as i32 + 1]),
                String::from(*s),
            ));
            IRInfo {
                ty: IRType::PTR(Box::from(IRType::Var(String::from("i8"), vec![s.len() as i32 + 1]))),
                left_ir_name: format!("@.str.{}", str_cnt),
                right_ir_name: Some(format!("@.str.{}", str_cnt)),
                lhs_ir_name: None,
                lhs_ty: IRType::void(),
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
                ctx.func_defs.push(IRNode::GetElementPtr(
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

fn alloc_arr_by_sizes<'a>(name: &str, sizes: &[Option<ASTNode>], ctx: &mut Context) -> IRInfo {
    let expr_info = dfs(&sizes[0].as_ref().unwrap(), ctx);
    if sizes.len() == 1 {
        let res_ty = IRType::from_str(name);
        let expr_ir_name = expr_info.get_right_ir_name(ctx);
        let mul_res_name = ctx.generate();

        ctx.func_defs.push(IRNode::Binary(
            mul_res_name.clone(),
            String::from("mul"),
            IRType::i32(),
            expr_ir_name,
            res_ty.size().to_string(),
        ));
        let res_name = ctx.generate();
        ctx.func_defs.push(IRNode::Call(
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