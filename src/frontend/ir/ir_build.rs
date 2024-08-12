use super::IRType;
use super::IRNode;
use super::ast::ASTNode;

struct Context {
    index: i32,
}
impl Context {
    pub fn new() -> Self {
        Context {
            index: 0,
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
    pub fn local_var(&self, name: &str) -> String {
        format!("%{}.{}", name, self.index)
    }
    pub fn func(&self, name: String) -> String {
        format!("@{}", name)
    }
    pub fn is_global(&self) -> bool {
        self.index == 0
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

pub fn dfs<'a>(ast: &ASTNode<'a>, ctx: &mut Context, class_defs: &mut Vec<IRNode>, var_decls: &mut Vec<IRNode>, func_defs: &mut Vec<IRNode>) {
    match ast {
        ASTNode::Root(ch, _) => {
            for node in ch {
                dfs(node, ctx, class_defs, var_decls, func_defs);
            }
        }
        ASTNode::ClassDef(name, ch, _) => {
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
            class_defs.push(IRNode::Class(format!("%class.{}", name), fields));
        }
        ASTNode::FuncDef(ret_ty, name, args, block, _) => {
            ctx.push();
            let mut args_ = Vec::new();
            for (ty, name) in args {
                args_.push((IRType::from(ty), ctx.local_var(name)));
            }

            func_defs.push(IRNode::FuncBegin(IRType::from(ret_ty), name.to_string(), args_));
            for (ty, name) in args {
                func_defs.push(IRNode::Allocate(ctx.local_var(name), IRType::from(ty)));
                func_defs.push(IRNode::Store(IRType::from(ty), ctx.local_var(name), name.to_string()));
            }

            dfs(block, ctx, class_defs, var_decls, func_defs);

            func_defs.push(IRNode::FuncEnd);
            ctx.pop();
        }
        ASTNode::ConstrDef(name, block, _) => {}
        ASTNode::VarDecl(ty, vars, _) => {
            if ctx.is_global() {
                for (name, _) in vars {
                    var_decls.push(IRNode::Global(ctx.global_var(name), IRType::from(ty), "0".to_string()));
                }
            }
        }
        ASTNode::Block(ch, _) => {
            ctx.push();
            for node in ch {
                dfs(node, ctx, class_defs, var_decls, func_defs);
            }
            ctx.pop();
        }
        _ => {}
    }
}