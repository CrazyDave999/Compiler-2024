use std::collections::HashMap;
use super::utils::escape_string;
use super::IRType;
use super::IRNode;
use super::ast::ASTNode;

#[derive(Clone)]
enum LoopType {
    For,
    While,
    Null,
}
struct Context {
    index: i32,
    cnt: i32, // 中间变量
    if_cnt: i32,
    loop_cnt: i32,

    str_cnt: i32,
    land_cnt: i32,
    lor_cnt: i32,

    class_name: Option<String>,

    size_map: HashMap<String, i32>,
    last_label: String,
    class_defs: Vec<IRNode>,
    var_decls: Vec<IRNode>,
    func_defs: Vec<IRNode>,
    global_init: Vec<IRNode>,

    cur_loop: (i32, LoopType),
}
impl Context {
    pub fn new() -> Self {
        let res = Context {
            index: 0,
            cnt: 0,
            if_cnt: 0,
            loop_cnt: 0,
            str_cnt: 0,
            land_cnt: 0,
            lor_cnt: 0,
            class_name: None,
            size_map: HashMap::new(),
            last_label: String::from(""),
            class_defs: Vec::new(),
            var_decls: Vec::new(),
            func_defs: Vec::new(),
            global_init: Vec::new(),
            cur_loop: (-1, LoopType::Null),
        };
        res
    }
    pub fn push(&mut self) {
        self.index += 1;
    }
    pub fn pop(&mut self) {
        self.index -= 1;
    }

    pub fn local_var_def(&self, name: &str, cnt: i32) -> String {
        format!("%{}.{}", name, cnt)
    }
    pub fn local_var_use(&self, name: &str, cnt: i32) -> String {
        format!("%{}.{}", name, cnt)
    }

    pub fn param(&self, name: &str) -> String {
        format!("%{}.{}.param", name, self.index)
    }

    pub fn func_def(&self, name: &str) -> String {
        if let Some(class_name) = &self.class_name {
            format!("{}.{}", class_name, name)
        } else {
            format!("{}", name)
        }
    }
    pub fn func_use(&self, name: &str, class_name: Option<String>) -> String {
        if let Some(class_name) = class_name {
            format!("{}.{}", class_name, name)
        } else {
            format!("{}", name)
        }
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

    pub fn generate_loop(&mut self, ty: LoopType) -> String {
        let res = self.loop_cnt;
        self.cur_loop = (res, ty);
        self.loop_cnt += 1;
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
    pub fn generate_lor(&mut self) -> String {
        let res = self.lor_cnt;
        self.lor_cnt += 1;
        res.to_string()
    }

    pub fn insert_class_size(&mut self, name: &str, size: i32) {
        self.size_map.insert(name.to_string(), size);
    }
    pub fn size_of(&self, name: &str) -> i32 {
        *self.size_map.get(name).unwrap()
    }

    pub fn insert_statement(&mut self, node: IRNode) -> (bool, usize) { // (is_global, index)
        if self.is_global() {
            self.global_init.push(node);
            (true, self.global_init.len() - 1)
        } else {
            self.func_defs.push(node);
            (false, self.func_defs.len() - 1)
        }
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
                ctx.insert_statement(IRNode::Load(
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
    res.push(IRNode::FuncBegin(IRType::void(), String::from("global..init"), vec![]));
    for ch in ctx.global_init {
        res.push(ch);
    }
    res.push(IRNode::Ret(IRType::void(), None));
    res.push(IRNode::FuncEnd);
    for ch in ctx.func_defs {
        res.push(ch.clone());
        match &ch {
            IRNode::FuncBegin(_, name, _) => {
                if name == "main" {
                    res.push(IRNode::Call(None, IRType::void(), String::from("global..init"), vec![]));
                }
            }
            _ => {}
        }
    }
    res
}

fn dfs<'a>(ast: &ASTNode<'a>, ctx: &mut Context) -> IRInfo {
    match ast {
        ASTNode::Root(ch, _) => {
            for node in ch {
                match node {
                    ASTNode::ClassDef(name, ch, _) => {
                        let mut fields = Vec::new();
                        let mut size = 0;
                        for node in ch {
                            match node {
                                ASTNode::VarDecl(ty, vars, _) => {
                                    for _ in 0..vars.len() {
                                        let ir_ty = IRType::from(ty);
                                        fields.push(ir_ty.clone());
                                        size += 4;
                                    }
                                }
                                _ => {}
                            }
                        }
                        ctx.insert_class_size(*name, size);
                        ctx.class_defs.push(IRNode::Class(format!("%class.{}", *name), fields));
                    }
                    _ => {}
                }
            }
            for node in ch {
                dfs(node, ctx);
            }
            IRInfo::void()
        }
        ASTNode::ClassDef(name, ch, _) => {
            ctx.class_name = Some(name.to_string());
            ctx.push();

            let mut has_constr = false;
            for node in ch {
                match node {
                    ASTNode::FuncDef(_, _, _, _, _) => { dfs(node, ctx); }
                    ASTNode::ConstrDef(_, _, _) => {
                        dfs(node, ctx);
                        has_constr = true;
                    }
                    _ => {}
                }
            }
            if !has_constr {
                ctx.insert_statement(IRNode::FuncBegin(
                    IRType::void(),
                    ctx.func_def(name),
                    vec![(IRType::PTR(Box::from(IRType::class(name))), String::from("%this"))],
                ));
                ctx.insert_statement(IRNode::Ret(IRType::void(), None));
                ctx.insert_statement(IRNode::FuncEnd);
            }
            ctx.pop();
            ctx.class_name = None;

            IRInfo::void()
        }
        ASTNode::FuncDef(ret_ty, name, args, block, _) => {
            ctx.push();

            let mut args_ = Vec::new();
            if let Some(class_name) = ctx.class_name.as_ref() {
                args_.push((IRType::PTR(Box::from(IRType::class(class_name))), String::from("%this")));
            }
            for (ty, name, _) in args {
                args_.push((IRType::from(ty), ctx.param(name)));
            }

            ctx.func_defs.push(IRNode::FuncBegin(IRType::from(ret_ty), ctx.func_def(*name), args_));
            ctx.last_label = String::from("entry");
            for (ty, name, cnt) in args {
                ctx.func_defs.push(IRNode::Allocate(ctx.local_var_def(name, *cnt), IRType::from(ty)));
                ctx.func_defs.push(IRNode::Store(IRType::from(ty), ctx.param(name), ctx.local_var_def(name, *cnt)));
            }

            dfs(block, ctx);

            let last = ctx.func_defs.last().unwrap();
            match last {
                IRNode::Ret(_, _) => {}
                _ => {
                    if *name == "main" {
                        ctx.func_defs.push(IRNode::Ret(
                            IRType::Var(String::from("i32"), vec![]),
                            Some(String::from("0")),
                        ));
                    } else {
                        let ret_ty = IRType::from(ret_ty);
                        ctx.func_defs.push(IRNode::Ret(
                            ret_ty.clone(),
                            ret_ty.default_value(),
                        ));
                    }
                }
            }
            ctx.func_defs.push(IRNode::FuncEnd);
            ctx.pop();
            IRInfo::void()
        }
        ASTNode::ConstrDef(name, block, _) => {
            ctx.push();
            let mut args = Vec::new();
            if let Some(class_name) = ctx.class_name.as_ref() {
                args.push((IRType::PTR(Box::from(IRType::class(class_name))), String::from("%this")));
            }
            ctx.func_defs.push(IRNode::FuncBegin(IRType::void(), ctx.func_def(*name), args));
            ctx.last_label = String::from("entry");
            dfs(block, ctx);
            let last = ctx.func_defs.last().unwrap();
            match last {
                IRNode::Ret(_, _) => {}
                _ => {
                    ctx.func_defs.push(IRNode::Ret(IRType::void(), None));
                }
            }
            ctx.func_defs.push(IRNode::FuncEnd);
            ctx.pop();
            IRInfo::void()
        }
        ASTNode::VarDecl(ty, vars, _) => {
            if ctx.is_global() {
                let ir_type = IRType::from(ty);
                for (name, init, _) in vars {
                    ctx.var_decls.push(IRNode::Global(
                        name.to_string(),
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
                    if let Some(init) = init {
                        let init_info = dfs(init, ctx);
                        let init_ir_name = init_info.get_right_ir_name(ctx);
                        ctx.global_init.push(IRNode::Store(
                            ir_type.clone(),
                            init_ir_name,
                            format!("@{}", *name),
                        ));
                    }
                }
            } else {
                for (name, expr, cnt) in vars {
                    ctx.func_defs.push(IRNode::Allocate(ctx.local_var_def(name, *cnt), IRType::from(ty)));
                    if let Some(expr) = expr {
                        let expr_info = dfs(expr, ctx);
                        let right_ir_name = expr_info.get_right_ir_name(ctx);
                        ctx.func_defs.push(IRNode::Store(
                            IRType::from(ty),
                            right_ir_name,
                            ctx.local_var_def(name, *cnt),
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
            if let Some(op) = op {
                alloc_arr_by_const(op, ctx)
            } else {
                alloc_arr_by_sizes(name, sizes, 0, ctx)
            }
        }
        ASTNode::ClassInit(name, _) => {
            let res_name = ctx.generate();
            let res_ty = IRType::PTR(Box::from(IRType::class(name)));
            ctx.insert_statement(IRNode::Call(
                Some(res_name.clone()),
                res_ty.clone(),
                String::from("malloc"),
                vec![(IRType::i32(), ctx.size_of(name).to_string())],
            ));
            // 调用构造函数
            ctx.insert_statement(IRNode::Call(
                None,
                IRType::void(),
                ctx.func_use(name, Some(String::from(*name))),
                vec![(IRType::PTR(Box::from(IRType::class(name))), res_name.clone())],
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
                            ctx.insert_statement(IRNode::Binary(
                                res_name.clone(),
                                "add".to_string(),
                                res_ty.clone(),
                                lhs_ir_name,
                                rhs_ir_name,
                            ));
                        }
                        IRType::PTR(_) => {
                            // string
                            ctx.insert_statement(IRNode::Call(
                                Some(res_name.clone()),
                                res_ty.clone(),
                                String::from("string.add"),
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
                "-" | "*" | "/" | "%" | "<<" | ">>" | "&" | "|" | "^" => {
                    let rhs_info = dfs(rhs, ctx);
                    let res_ty = lhs_info.ty.clone();
                    let lhs_ir_name = lhs_info.get_right_ir_name(ctx);
                    let rhs_ir_name = rhs_info.get_right_ir_name(ctx);
                    let res_name = ctx.generate();
                    ctx.insert_statement(IRNode::Binary(
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
                            ctx.insert_statement(IRNode::ICMP(
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
                            // ptr of string or ptr of other
                            if lhs_info.ty.is_string() {
                                ctx.insert_statement(IRNode::Call(
                                    Some(res_name.clone()),
                                    res_ty.clone(),
                                    match *op {
                                        "==" => "string.eq",
                                        "!=" => "string.ne",
                                        "<" => "string.lt",
                                        "<=" => "string.le",
                                        ">" => "string.gt",
                                        ">=" => "string.ge",
                                        _ => unreachable!(),
                                    }.to_string(),
                                    vec![
                                        (IRType::PTR(Box::from(IRType::class("string"))), lhs_ir_name),
                                        (IRType::PTR(Box::from(IRType::class("string"))), rhs_ir_name),
                                    ],
                                ));
                            } else {
                                ctx.insert_statement(IRNode::ICMP(
                                    res_name.clone(),
                                    match *op {
                                        "==" => "eq",
                                        "!=" => "ne",
                                        _ => unreachable!(),
                                    }.to_string(),
                                    lhs_info.ty.clone(),
                                    lhs_ir_name,
                                    rhs_ir_name,
                                ));
                            }
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
                    // let last_label = ctx.last_label.clone();
                    let land_cnt = ctx.generate_land();
                    let land_true_label = format!("land.true.{}", land_cnt);
                    let land_end_label = format!("land.end.{}", land_cnt);
                    let lhs_ir_name = lhs_info.get_right_ir_name(ctx);

                    // for displacing phi
                    let res_name = ctx.generate();
                    ctx.insert_statement(IRNode::Allocate(
                        res_name.clone(),
                        IRType::i1(),
                    ));
                    ctx.insert_statement(IRNode::Store(
                        IRType::i1(),
                        lhs_ir_name.clone(),
                        res_name.clone(),
                    ));

                    ctx.insert_statement(IRNode::BrCond(
                        lhs_ir_name.clone(),
                        land_true_label.clone(),
                        land_end_label.clone(),
                    ));
                    ctx.insert_statement(IRNode::Label(land_true_label.clone()));
                    ctx.last_label = land_true_label.clone();
                    let rhs_info = dfs(rhs, ctx);
                    let rhs_ir_name = rhs_info.get_right_ir_name(ctx);

                    // for displacing phi
                    ctx.insert_statement(IRNode::Store(
                        IRType::i1(),
                        rhs_ir_name.clone(),
                        res_name.clone(),
                    ));

                    ctx.insert_statement(IRNode::Br(
                        land_end_label.clone(),
                    ));

                    ctx.insert_statement(IRNode::Label(land_end_label.clone()));

                    // ctx.insert_statement(IRNode::Phi(
                    //     res_name.clone(),
                    //     IRType::i1(),
                    //     vec![
                    //         (String::from("false"), last_label),
                    //         (rhs_ir_name, ctx.last_label.clone()),
                    //     ],
                    // ));
                    ctx.last_label = land_end_label;
                    IRInfo {
                        ty: IRType::i1(),
                        left_ir_name: res_name, // for displacing phi
                        right_ir_name: None, // for displacing phi
                        lhs_ir_name: None,
                        lhs_ty: IRType::void(),
                    }
                }
                "||" => {
                    // let last_label = ctx.last_label.clone();
                    let lor_cnt = ctx.generate_lor();
                    let lor_false_label = format!("lor.false.{}", lor_cnt);
                    let lor_end_label = format!("lor.end.{}", lor_cnt);
                    let lhs_ir_name = lhs_info.get_right_ir_name(ctx);

                    // for displacing phi
                    let res_name = ctx.generate();
                    ctx.insert_statement(IRNode::Allocate(
                        res_name.clone(),
                        IRType::i1(),
                    ));
                    ctx.insert_statement(IRNode::Store(
                        IRType::i1(),
                        lhs_ir_name.clone(),
                        res_name.clone(),
                    ));

                    ctx.insert_statement(IRNode::BrCond(
                        lhs_ir_name.clone(),
                        lor_end_label.clone(),
                        lor_false_label.clone(),
                    ));
                    ctx.insert_statement(IRNode::Label(lor_false_label.clone()));
                    ctx.last_label = lor_false_label.clone();
                    let rhs_info = dfs(rhs, ctx);
                    let rhs_ir_name = rhs_info.get_right_ir_name(ctx);

                    // for displacing phi
                    ctx.insert_statement(IRNode::Store(
                        IRType::i1(),
                        rhs_ir_name.clone(),
                        res_name.clone(),
                    ));

                    ctx.insert_statement(IRNode::Br(
                        lor_end_label.clone(),
                    ));
                    ctx.insert_statement(IRNode::Label(lor_end_label.clone()));

                    // ctx.insert_statement(IRNode::Phi(
                    //     res_name.clone(),
                    //     IRType::i1(),
                    //     vec![
                    //         (String::from("true"), last_label),
                    //         (rhs_ir_name, ctx.last_label.clone()),
                    //     ],
                    // ));
                    ctx.last_label = lor_end_label;
                    IRInfo {
                        ty: IRType::i1(),
                        left_ir_name: res_name, // for displacing phi
                        right_ir_name: None, // for displacing phi
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
        ASTNode::TernaryExpr(cond, expr1, expr2, _) => {
            let cond_info = dfs(cond, ctx);
            let cond_ir_name = cond_info.get_right_ir_name(ctx);
            let if_cnt = ctx.generate_if();
            let if_label = format!("if.then.{}", if_cnt);
            let else_label = format!("if.else.{}", if_cnt);
            let end_label = format!("if.end.{}", if_cnt);

            // for displacing phi
            let res_name = ctx.generate();
            let (is_global, index) = ctx.insert_statement(IRNode::Allocate(
                res_name.clone(),
                cond_info.ty.clone(),
            ));


            ctx.insert_statement(IRNode::BrCond(
                cond_ir_name,
                if_label.clone(),
                else_label.clone(),
            ));

            ctx.insert_statement(IRNode::Label(if_label.clone()));
            ctx.last_label = if_label.clone();
            let expr1_info = dfs(expr1, ctx);
            // let expr1_last_label = ctx.last_label.clone();
            let expr1_ir_name = expr1_info.get_right_ir_name(ctx);

            // for displacing phi
            if expr1_info.ty.is_void() {
                if is_global {
                    ctx.global_init.remove(index);
                } else {
                    ctx.func_defs.remove(index);
                }
            } else {
                if is_global {
                    ctx.global_init[index] = IRNode::Allocate(
                        res_name.clone(),
                        expr1_info.ty.clone(),
                    );
                } else {
                    ctx.func_defs[index] = IRNode::Allocate(
                        res_name.clone(),
                        expr1_info.ty.clone(),
                    );
                }
                ctx.insert_statement(IRNode::Store(
                    expr1_info.ty.clone(),
                    expr1_ir_name.clone(),
                    res_name.clone(),
                ));
            }


            ctx.insert_statement(IRNode::Br(end_label.clone()));

            ctx.insert_statement(IRNode::Label(else_label.clone()));
            ctx.last_label = else_label.clone();
            let expr2_info = dfs(expr2, ctx);
            // let expr2_last_label = ctx.last_label.clone();
            let expr2_ir_name = expr2_info.get_right_ir_name(ctx);

            // for displacing phi
            if !expr2_info.ty.is_void() {
                ctx.insert_statement(IRNode::Store(
                    expr2_info.ty.clone(),
                    expr2_ir_name.clone(),
                    res_name.clone(),
                ));
            }

            ctx.insert_statement(IRNode::Br(end_label.clone()));

            ctx.func_defs.push(IRNode::Label(end_label.clone()));
            ctx.last_label = end_label;


            // let res_name = if expr1_info.ty.is_void() {
            //     String::from("")
            // } else {
            //     let cond_name = ctx.generate();
            //     ctx.insert_statement(IRNode::Phi(
            //         cond_name.clone(),
            //         expr1_info.ty.clone(),
            //         vec![
            //             (expr1_ir_name, expr1_last_label),
            //             (expr2_ir_name, expr2_last_label),
            //         ],
            //     ));
            //     cond_name
            // };

            IRInfo {
                ty: expr1_info.ty.clone(),
                left_ir_name: if expr1_info.ty.is_void() {
                    String::from("")
                } else {
                    res_name.clone()
                },
                right_ir_name: if expr1_info.ty.is_void() {
                    Some(String::from(""))
                } else {
                    None
                },
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
                "#METHOD_SIZE#" => {
                    IRInfo {
                        ty: IRType::from(ty),
                        left_ir_name: String::from(""),
                        right_ir_name: Some(ctx.func_use("CrazyDave..GetArraySize", None)),
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
                ctx.insert_statement(IRNode::Call(
                    None,
                    res_ty.clone(),
                    lhs_ir_name,
                    args,
                ));
            } else {
                ctx.insert_statement(IRNode::Call(
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
            ctx.last_label = if_label.clone();
            dfs(if_block, ctx);
            ctx.func_defs.push(IRNode::Br(end_label.clone()));
            if let Some(else_block) = else_block {
                ctx.func_defs.push(IRNode::Label(else_label.clone()));
                ctx.last_label = else_label.clone();
                dfs(else_block, ctx);
                ctx.func_defs.push(IRNode::Br(end_label.clone()));
            }
            ctx.func_defs.push(IRNode::Label(end_label.clone()));
            ctx.last_label = end_label;
            IRInfo::void()
        }
        ASTNode::ForStmt(expr1, expr2, expr3, block, _) => {
            ctx.push();
            let pre_loop = ctx.cur_loop.clone();
            let for_cnt = ctx.generate_loop(LoopType::For);


            if let Some(expr1) = expr1 {
                dfs(expr1, ctx);
            }
            let cond_label = format!("for.cond.{}", for_cnt);
            let body_label = format!("for.body.{}", for_cnt);
            let inc_label = format!("for.inc.{}", for_cnt);
            let end_label = format!("for.end.{}", for_cnt);
            ctx.func_defs.push(IRNode::Br(cond_label.clone()));
            ctx.func_defs.push(IRNode::Label(cond_label.clone()));
            ctx.last_label = cond_label.clone();
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
            ctx.cur_loop = pre_loop;
            IRInfo::void()
        }
        ASTNode::WhileStmt(cond, block, _) => {
            ctx.push();
            let pre_loop = ctx.cur_loop.clone();
            let while_cnt = ctx.generate_loop(LoopType::While);
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
            ctx.cur_loop = pre_loop;
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
        ASTNode::BreakStmt(_) => {
            let (loop_cnt, ty) = ctx.cur_loop.clone();
            match ty {
                LoopType::For => {
                    ctx.func_defs.push(IRNode::Br(format!("for.end.{}", loop_cnt)));
                }
                LoopType::While => {
                    ctx.func_defs.push(IRNode::Br(format!("while.end.{}", loop_cnt)));
                }
                LoopType::Null => unreachable!()
            }
            IRInfo::void()
        }
        ASTNode::ContinueStmt(_) => {
            let (loop_cnt, ty) = ctx.cur_loop.clone();
            match ty {
                LoopType::For => {
                    ctx.func_defs.push(IRNode::Br(format!("for.inc.{}", loop_cnt)));
                }
                LoopType::While => {
                    ctx.func_defs.push(IRNode::Br(format!("while.cond.{}", loop_cnt)));
                }
                LoopType::Null => unreachable!()
            }
            IRInfo::void()
        }
        ASTNode::NULL(_) => {
            IRInfo {
                ty: IRType::PTR(Box::from(IRType::void())),
                left_ir_name: String::from(""),
                right_ir_name: Some("null".to_string()),
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
        ASTNode::Str(s, _) => {
            let (escaped, l) = escape_string(*s);
            let str_cnt = ctx.generate_str();
            ctx.var_decls.push(IRNode::Str(
                format!(".str.{}", str_cnt),
                IRType::Var(String::from("i8"), vec![l + 1]),
                escaped.clone(),
                s.to_string(),
            ));
            IRInfo {
                ty: IRType::PTR(Box::from(IRType::class("string"))),
                left_ir_name: format!("@.str.{}", str_cnt),
                right_ir_name: Some(format!("@.str.{}", str_cnt)),
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
        ASTNode::FmtStr(ch, _) => {
            let mut last_str = String::new();
            let mut lhs_name = String::new();
            for node in ch {
                match node {
                    ASTNode::Str(s, _) => {
                        last_str.push_str(*s);
                    }
                    _ => {
                        // i32, i1, fmt_string
                        let info = dfs(node, ctx);
                        let ir_name = info.get_right_ir_name(ctx);
                        let rhs_name = if info.ty.is_i32() {
                            let i32_name = ctx.generate();
                            ctx.insert_statement(IRNode::Call(
                                Some(i32_name.clone()),
                                IRType::PTR(Box::from(IRType::class("string"))),
                                String::from("toString"),
                                vec![(IRType::i32(), ir_name)],
                            ));
                            i32_name
                        } else if info.ty.is_i1() {
                            let i1_name = ctx.generate();
                            ctx.insert_statement(IRNode::Call(
                                Some(i1_name.clone()),
                                IRType::PTR(Box::from(IRType::class("string"))),
                                String::from("CrazyDave..boolToString"),
                                vec![(IRType::i1(), ir_name)],
                            ));
                            i1_name
                        } else {
                            ir_name
                        };
                        if last_str.is_empty() {
                            if lhs_name.is_empty() {
                                lhs_name = rhs_name;
                            } else {
                                let new_lhs_name = ctx.generate();
                                ctx.insert_statement(IRNode::Call(
                                    Some(new_lhs_name.clone()),
                                    IRType::PTR(Box::from(IRType::class("string"))),
                                    String::from("string.add"),
                                    vec![
                                        (IRType::PTR(Box::from(IRType::class("string"))), lhs_name.clone()),
                                        (IRType::PTR(Box::from(IRType::class("string"))), rhs_name),
                                    ],
                                ));
                                lhs_name = new_lhs_name;
                            }
                        } else {
                            let str_cnt = ctx.generate_str();
                            let last_str_name = format!("@.str.{}", str_cnt);
                            let (escaped, l) = escape_string(last_str.as_str());
                            ctx.var_decls.push(IRNode::Str(
                                format!(".str.{}", str_cnt),
                                IRType::Var(String::from("i8"), vec![l + 1]),
                                escaped,
                                last_str.to_string(),
                            ));

                            let add_name_1 = ctx.generate();
                            ctx.insert_statement(IRNode::Call(
                                Some(add_name_1.clone()),
                                IRType::PTR(Box::from(IRType::class("string"))),
                                String::from("string.add"),
                                vec![
                                    (IRType::PTR(Box::from(IRType::class("string"))), last_str_name.clone()),
                                    (IRType::PTR(Box::from(IRType::class("string"))), rhs_name.clone()),
                                ],
                            ));

                            if lhs_name.is_empty() {
                                lhs_name = add_name_1;
                            } else {
                                let add_name_2 = ctx.generate();
                                ctx.insert_statement(IRNode::Call(
                                    Some(add_name_2.clone()),
                                    IRType::PTR(Box::from(IRType::class("string"))),
                                    String::from("string.add"),
                                    vec![
                                        (IRType::PTR(Box::from(IRType::class("string"))), lhs_name.clone()),
                                        (IRType::PTR(Box::from(IRType::class("string"))), add_name_1.clone()),
                                    ],
                                ));
                                lhs_name = add_name_2;
                            }
                        }
                        last_str.clear();
                    }
                }
            }
            if !last_str.is_empty() {
                let str_cnt = ctx.generate_str();
                let last_str_name = format!("@.str.{}", str_cnt);
                let (escaped, l) = escape_string(last_str.as_str());
                ctx.var_decls.push(IRNode::Str(
                    format!(".str.{}", str_cnt),
                    IRType::Var(String::from("i8"), vec![l + 1]),
                    escaped,
                    last_str.to_string(),
                ));
                if !lhs_name.is_empty() {
                    let add_name = ctx.generate();
                    ctx.insert_statement(IRNode::Call(
                        Some(add_name.clone()),
                        IRType::PTR(Box::from(IRType::class("string"))),
                        String::from("string.add"),
                        vec![
                            (IRType::PTR(Box::from(IRType::class("string"))), lhs_name.clone()),
                            (IRType::PTR(Box::from(IRType::class("string"))), last_str_name.clone()),
                        ],
                    ));
                    lhs_name = add_name;
                } else {
                    lhs_name = last_str_name;
                }
            }
            IRInfo {
                ty: IRType::PTR(Box::from(IRType::class("string"))),
                left_ir_name: lhs_name.clone(),
                right_ir_name: Some(lhs_name),
                lhs_ir_name: None,
                lhs_ty: IRType::void(),
            }
        }
        ASTNode::ArrConst(_, _) => {
            alloc_arr_by_const(ast, ctx)
        }
        ASTNode::Ident(name, _, idx, ty, cnt, is_global) => {
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
                            lhs_ty: IRType::PTR(Box::from(IRType::class(ctx.class_name.as_ref().unwrap().as_str()))),
                        }
                    }
                    _ => {
                        IRInfo {
                            ty: IRType::from(ty),
                            left_ir_name: if *is_global {
                                format!("@{}", *name)
                            } else {
                                ctx.local_var_use(name, *cnt)
                            },
                            right_ir_name: None,
                            lhs_ir_name: None,
                            lhs_ty: IRType::void(),
                        }
                    }
                }
            }
        }
        // _ => {
        //     IRInfo::void()
        // }
    }
}


// 非常困难的一个部分，但是不要被困难吓倒，勇敢牛牛，不怕困难！
// 有两种可能，第一种是Type [][] A = new Type [N1][N2]; 这需要从外向内申请空间，只用调用AllocArray来申请指针的空间，
// 在最后一层申请类的空间，并把指针存回上一层
// 第二种是Type [][][] A = new Type [N1][N2][]; 这前面部分同上，但遇到第一个[]时直接终止即可
// 一个非常麻烦的问题是需要手写循环来申请各层空间
fn alloc_arr_by_sizes(name: &str, sizes: &Vec<Option<ASTNode>>, cur: i32, ctx: &mut Context) -> IRInfo {
    let expr = sizes[cur as usize].as_ref().unwrap();

    let expr_info = dfs(expr, ctx);
    let expr_ir_name = expr_info.get_right_ir_name(ctx);

    // res_name 存的是指向n个指针的空间的指针
    let res_name = ctx.generate();
    ctx.insert_statement(IRNode::Call(
        Some(res_name.clone()),
        IRType::PTR(Box::from(IRType::void())),
        ctx.func_use("CrazyDave..AllocArray", None),
        vec![
            (IRType::i32(), expr_ir_name.clone())
        ],
    ));
    let res_info = IRInfo {
        ty: IRType::PTR(Box::from(IRType::void())),
        left_ir_name: res_name.clone(),
        right_ir_name: Some(res_name.clone()),
        lhs_ir_name: None,
        lhs_ty: IRType::void(),
    };
    if cur == sizes.len() as i32 - 1 && (name == "int" || name == "bool" || name == "string") {
        // 最后一层若为i32和i1和string，就把这指针空间当成i32和i1的数组空间或string的指针空间
        return res_info;
    }
    if cur == sizes.len() as i32 - 1 || !&sizes[cur as usize + 1].is_none() {
        // 手写循环
        // 循环变量指针idx_name
        let idx_ptr_name = ctx.generate();
        ctx.insert_statement(IRNode::Allocate(
            idx_ptr_name.clone(),
            IRType::i32(),
        ));
        ctx.insert_statement(IRNode::Store(
            IRType::i32(),
            String::from("0"),
            idx_ptr_name.clone(),
        ));
        let loop_cnt = ctx.generate_loop(LoopType::While);
        let cond_label = format!("while.cond.{}", loop_cnt);
        let body_label = format!("while.body.{}", loop_cnt);
        let end_label = format!("while.end.{}", loop_cnt);
        ctx.insert_statement(IRNode::Br(cond_label.clone()));
        ctx.insert_statement(IRNode::Label(cond_label.clone()));

        let idx_name = ctx.generate();
        ctx.insert_statement(IRNode::Load(
            idx_name.clone(),
            IRType::i32(),
            idx_ptr_name.clone(),
        ));

        let cmp_name = ctx.generate();
        ctx.insert_statement(IRNode::ICMP(
            cmp_name.clone(),
            String::from("slt"),
            IRType::i32(),
            idx_name.clone(),
            expr_ir_name.clone(),
        ));
        ctx.insert_statement(IRNode::BrCond(
            cmp_name.clone(),
            body_label.clone(),
            end_label.clone(),
        ));
        ctx.insert_statement(IRNode::Label(body_label.clone()));

        // 进入下层，把返回指针存入本层
        // 或者为类申请空间，把指针存入本层
        let next_ir_name = if cur < sizes.len() as i32 - 1 {
            let next_info = alloc_arr_by_sizes(name, sizes, cur + 1, ctx);
            next_info.get_right_ir_name(ctx)
        } else {
            let alloc_name = ctx.generate();
            ctx.insert_statement(IRNode::Call(
                Some(alloc_name.clone()),
                IRType::PTR(Box::from(IRType::class(name))),
                String::from("malloc"),
                vec![(IRType::i32(), ctx.size_of(name).to_string())],
            ));
            // 调用构造函数
            ctx.insert_statement(IRNode::Call(
                None,
                IRType::void(),
                ctx.func_use(name, Some(String::from(name))),
                vec![
                    (IRType::PTR(Box::from(IRType::class(name))), alloc_name.clone())
                ],
            ));
            alloc_name
        };


        let ptr_name = ctx.generate();
        ctx.insert_statement(IRNode::GetElementPtr(
            ptr_name.clone(),
            IRType::PTR(Box::from(IRType::void())),
            res_name.clone(),
            vec![(IRType::i32(), idx_name.clone())],
        ));
        ctx.insert_statement(IRNode::Store(
            IRType::PTR(Box::from(IRType::void())),
            next_ir_name.clone(),
            ptr_name.clone(),
        ));

        // 循环变量自增
        let add_name = ctx.generate();
        ctx.insert_statement(IRNode::Binary(
            add_name.clone(),
            String::from("add"),
            IRType::i32(),
            idx_name.clone(),
            String::from("1"),
        ));
        ctx.insert_statement(IRNode::Store(
            IRType::i32(),
            add_name.clone(),
            idx_ptr_name.clone(),
        ));
        ctx.insert_statement(IRNode::Br(cond_label.clone()));
        ctx.insert_statement(IRNode::Label(end_label.clone()));
    }
    res_info
}

// 总是逃不过的，来吧！
fn alloc_arr_by_const(node: &ASTNode, ctx: &mut Context) -> IRInfo {
    match node {
        ASTNode::ArrConst(ch, _) => {
            let res_name = ctx.generate();
            ctx.insert_statement(IRNode::Call(
                Some(res_name.clone()),
                IRType::PTR(Box::from(IRType::void())),
                ctx.func_use("CrazyDave..AllocArray", None),
                vec![
                    (IRType::i32(), ch.len().to_string())
                ],
            ));
            for (i, node) in ch.iter().enumerate() {
                let info = alloc_arr_by_const(node, ctx);
                let idx_name = ctx.generate();
                ctx.insert_statement(IRNode::GetElementPtr(
                    idx_name.clone(),
                    IRType::PTR(Box::from(IRType::void())),
                    res_name.clone(),
                    vec![(IRType::i32(), i.to_string())],
                ));
                let info_ir_name = info.get_right_ir_name(ctx);
                ctx.insert_statement(IRNode::Store(
                    info.ty.clone(),
                    info_ir_name.clone(),
                    idx_name.clone(),
                ));
            }
            IRInfo {
                ty: IRType::PTR(Box::from(IRType::void())),
                left_ir_name: res_name.clone(),
                right_ir_name: Some(res_name),
                lhs_ir_name: None,
                lhs_ty: IRType::void(),
            }
        }
        _ => {
            dfs(node, ctx)
        }
    }
}