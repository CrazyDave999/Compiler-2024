use super::IRType;
use super::IRNode;
use super::ast::ASTNode;

struct Context {
    index: i32,
    cnt: i32, // 中间变量
    class_name: Option<String>,
}
impl Context {
    pub fn new() -> Self {
        Context {
            index: 0,
            cnt: 0,
            class_name: None,
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
    pub fn func_use(&self, name: &str, class_name: Option<&str>) -> String {
        if let Some(class_name) = class_name {
            format!("@{}.{}", class_name, name)
        } else {
            format!("@{}", name)
        }
    }
    pub fn is_global(&self) -> bool {
        self.index == 0
    }
    pub fn generate(&mut self) -> String {
        let res = self.cnt;
        self.cnt += 1;
        format!("%{}", res)
    }
}

struct IRInfo {
    pub ty: IRType,
    pub ir_name: String,
    pub lhs_ir_name: Option<String>,
}

impl IRInfo {
    pub fn void() -> Self {
        IRInfo {
            ty: IRType::Var("void".to_string(), vec![]),
            ir_name: String::from(""),
            lhs_ir_name: None,
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
            for node in ch {
                match node {
                    ASTNode::VarDecl(ty, vars, _) => {
                        for _ in 0..vars.len() {
                            fields.push(IRType::from(ty));
                        }
                    }
                    _ => {
                        dfs(node, ctx, class_defs, var_decls, func_defs);
                    }
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
            if let Some(_) = ctx.class_name {
                args_.push((IRType::PTR(String::from("this")), String::from("%this")));
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
                for (name, _) in vars {
                    func_defs.push(IRNode::Allocate(ctx.local_var_def(name), IRType::from(ty)));
                    // 初始化？
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
                ty: IRType::PTR(String::from("this")),
                ir_name: String::from("%this"),
                lhs_ir_name: None,
            }
        }
        ASTNode::BinaryExpr(op, lhs, rhs, _) => {
            let lhs_info = dfs(lhs, ctx, class_defs, var_decls, func_defs);
            let rhs_info = dfs(rhs, ctx, class_defs, var_decls, func_defs);
            match *op {
                "+" => {
                    let res_name = ctx.generate();
                    let res_ty = lhs_info.ty.clone();
                    func_defs.push(IRNode::Binary(
                        res_name.clone(),
                        "add".to_string(),
                        res_ty.clone(),
                        lhs_info.ir_name,
                        rhs_info.ir_name,
                    ));
                    IRInfo {
                        ty: res_ty,
                        ir_name: res_name,
                        lhs_ir_name: None,
                    }
                }
                "-" | "*" | "/" | "%" | "<<" | ">>" | "&" | "|" | "^" => {
                    let res_name = ctx.generate();
                    let res_ty = lhs_info.ty.clone();
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
                        lhs_info.ir_name,
                        rhs_info.ir_name,
                    ));
                    IRInfo {
                        ty: res_ty,
                        ir_name: res_name,
                        lhs_ir_name: None,
                    }
                }
                _ => {
                    IRInfo::void()
                }
            }
        }
        ASTNode::ArrayAccess(lhs, expr, _, ty) => {
            let lhs_info = dfs(lhs, ctx, class_defs, var_decls, func_defs);
            let expr_info = dfs(expr, ctx, class_defs, var_decls, func_defs);
            let res_name = ctx.generate();
            let res_ty = IRType::from(ty);
            func_defs.push(IRNode::GetElementPtr(
                res_name.clone(),
                res_ty.clone(),
                lhs_info.ir_name,
                vec![(IRType::i32(), String::from("0")), (IRType::i32(), expr_info.ir_name)],
            ));
            IRInfo {
                ty: res_ty,
                ir_name: res_name,
                lhs_ir_name: None,
            }
        }
        ASTNode::MemberAccess(lhs, name, _, idx, ty) => {
            let lhs_info = dfs(lhs, ctx, class_defs, var_decls, func_defs);
            match ty.name {
                "#METHOD#" => {
                    match lhs_info.ty {
                        IRType::PTR(class_name) => {
                            IRInfo {
                                ty: IRType::from(ty),
                                ir_name: ctx.func_use(*name, Some(&class_name)),
                                lhs_ir_name: Some(lhs_info.ir_name),
                            }
                        }
                        _ => unreachable!()
                    }
                }
                _ => {
                    let res_name = ctx.generate();
                    let res_ty = IRType::from(ty);
                    func_defs.push(IRNode::GetElementPtr(
                        res_name.clone(),
                        res_ty.clone(),
                        lhs_info.ir_name,
                        vec![(IRType::i32(), String::from("0")), (IRType::i32(), (*idx).to_string())],
                    ));
                    IRInfo {
                        ty: res_ty,
                        ir_name: res_name,
                        lhs_ir_name: None,
                    }
                }
            }
        }
        ASTNode::FuncCall(lhs, params, _, ret_ty) => {
            let lhs_info = dfs(lhs, ctx, class_defs, var_decls, func_defs);
            let mut args = Vec::new();
            match lhs_info.ty {
                IRType::PTR(name) => {
                    if name == "#METHOD#" {
                        args.push((IRType::PTR(String::from("")), lhs_info.lhs_ir_name.unwrap()));
                    }
                }
                _ => {}
            }
            for param in params {
                let param_info = dfs(param, ctx, class_defs, var_decls, func_defs);
                args.push((param_info.ty, param_info.ir_name));
            }
            let res_name = ctx.generate();
            let res_ty = IRType::from(ret_ty);
            func_defs.push(IRNode::Call(Some(res_name.clone()), res_ty.clone(), lhs_info.ir_name, args));
            IRInfo::void()
        }
        ASTNode::ReturnStmt(ret, _) => {
            if let Some(expr) = ret {
                let expr_info = dfs(expr, ctx, class_defs, var_decls, func_defs);
                func_defs.push(IRNode::Ret(expr_info.ty, Some(expr_info.ir_name)));
                IRInfo::void()
            } else {
                func_defs.push(IRNode::Ret(IRType::Var(String::from("void"), vec![]), None));
                IRInfo::void()
            }
        }
        ASTNode::Int(val, _) => {
            IRInfo {
                ty: IRType::i32(),
                ir_name: val.to_string(),
                lhs_ir_name: None,
            }
        }
        ASTNode::Bool(b, _) => {
            IRInfo {
                ty: IRType::i1(),
                ir_name: match b {
                    true => "1".to_string(),
                    false => "0".to_string(),
                },
                lhs_ir_name: None,
            }
        }

        ASTNode::Ident(name, _, idx, ty, layer) => {
            if *idx != -1 {
                // 成员
                let res_name = ctx.generate();
                let res_ty = IRType::from(ty);
                func_defs.push(IRNode::GetElementPtr(
                    res_name.clone(),
                    res_ty.clone(),
                    String::from("%this"),
                    vec![(IRType::i32(), String::from("0")), (IRType::i32(), (*idx).to_string())],
                ));
                IRInfo {
                    ty: res_ty,
                    ir_name: res_name,
                    lhs_ir_name: None,
                }
            } else {
                let res_name = ctx.generate();
                match ty.name {
                    "#FUNC#" | "#METHOD#" => {}
                    _ => {
                        func_defs.push(IRNode::Load(res_name.clone(), IRType::from(ty), ctx.local_var_use(name,*layer)));
                    }
                }

                IRInfo {
                    ty: IRType::from(ty),
                    ir_name: match ty.name {
                        "#FUNC#" => ctx.func_use(name, None),
                        "#METHOD#" => ctx.func_use(name, Some(ctx.class_name.as_ref().unwrap())),
                        _ => res_name,
                    },
                    lhs_ir_name: match ty.name {
                        "#METHOD#" => Some(String::from("%this")),
                        _ => None,
                    },
                }
            }
        }
        _ => {
            IRInfo::void()
        }
    }
}