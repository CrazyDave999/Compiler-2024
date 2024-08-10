use std::collections::HashMap;
use pest::Span;
use super::ASTNode;
use super::Type;
use super::utils::{ExprInfo, Context};
use super::scope::{Scope, ScopeType, Member};


pub fn check<'a>(ast: &'a ASTNode) -> Result<(), (&'static str, Span<'a>)> {
    let mut scope = Scope::new();
    let mut ctx = Context::new();
    dfs(ast, &mut scope, &mut ctx)?;
    Ok(())
}

fn dfs<'a>(ast: &ASTNode<'a>, scope: &mut Scope<'a>, ctx: &mut Context<'a>) -> Result<ExprInfo<'a>, (&'static str, Span<'a>)> {
    match ast {
        ASTNode::Root(ch, sp) => {
            // 先收集类名
            for node in ch {
                match node {
                    ASTNode::ClassDef(name, _, sp) => {
                        if let Some(_) = scope.find_class(name) {
                            return Err(("Multiple Definitions", sp.clone()));
                        }
                        scope.insert_class(name, HashMap::new());
                    }
                    _ => {}
                }
            }

            let mut main = false;
            for node in ch {
                match node {
                    ASTNode::FuncDef(ty, name, args, _, sp) => {
                        if let Some(_) = scope.find_ident(name) {
                            return Err(("Multiple definitions", sp.clone()));
                        }
                        if let Some(_) = scope.find_class(name) {
                            return Err(("Multiple definitions", sp.clone()));
                        }
                        if *name == "main" {
                            main = true;
                            if !ty.is_int() {
                                return Err(("Invalid Type", sp.clone()));
                            }
                            if !args.is_empty() {
                                return Err(("Main function should have no arguments.", sp.clone()));
                            }
                        }

                        let mut my_args = vec![];
                        for (ty, _) in args {
                            if ty.is_void() {
                                return Err(("Invalid Type", sp.clone()));
                            }
                            if scope.find_class(ty.name).is_none() {
                                return Err(("Undefined Identifier", sp.clone()));
                            }
                            my_args.push(ty.clone());
                        }

                        scope.insert_func(name, ty, &my_args);
                    }
                    ASTNode::ClassDef(name, ch, sp) => {
                        match scope.find_ident_top(name) {
                            Some(Member::Func(_, _)) => {
                                return Err(("Multiple definitions", sp.clone()));
                            }
                            _ => {}
                        }

                        let mut members = HashMap::new();
                        let mut constr = false;
                        for mem in ch {
                            match mem {
                                ASTNode::ConstrDef(_, _, sp) => {
                                    if constr {
                                        // Two construct function
                                        return Err(("Multiple definitions", sp.clone()));
                                    }
                                    constr = true;
                                }
                                ASTNode::VarDecl(ty, ch, sp) => {
                                    if ty.is_void() {
                                        return Err(("Invalid Type", sp.clone()));
                                    }
                                    if scope.find_class(ty.name).is_none() {
                                        return Err(("Undefined Identifier", sp.clone()));
                                    }
                                    for (name, op) in ch {
                                        if members.contains_key(name) {
                                            // 重复定义
                                            return Err(("Multiple definitions", sp.clone()));
                                        }
                                        if let Some(_) = op {
                                            // 类成员默认初始化表达式为非法
                                            return Err(("Class member default init", sp.clone()));
                                        }
                                        members.insert(*name, Member::Var(ty.clone()));
                                    }
                                }
                                ASTNode::FuncDef(ty, name, args, _, sp) => {
                                    if members.contains_key(name) {
                                        // 重复定义
                                        return Err(("Multiple definitions", sp.clone()));
                                    }
                                    let mut my_args = vec![];
                                    for (ty, _) in args {
                                        if ty.is_void() {
                                            return Err(("Invalid Type", sp.clone()));
                                        }
                                        if scope.find_class(ty.name).is_none() {
                                            return Err(("Undefined Identifier", sp.clone()));
                                        }
                                        my_args.push(ty.clone());
                                    }

                                    members.insert(*name, Member::Func(ty.clone(), my_args));
                                }
                                _ => { return Err(("Invalid Member", sp.clone())) }
                            }
                        }
                        scope.insert_class(name, members);
                    }
                    _ => {}
                }
            }
            if !main {
                return Err(("No main function!", sp.clone()));
            }

            for node in ch {
                dfs(node, scope, ctx)?;
            }
            Ok(ExprInfo::void())
        }
        ASTNode::VarDecl(ty, ch, sp) => {
            if ty.is_void() {
                return Err(("Invalid Type", sp.clone()));
            }
            if scope.find_class(ty.name).is_none() {
                return Err(("Undefined Identifier", sp.clone()));
            }
            for (name, op) in ch {
                if let Some(_) = scope.find_ident_top(name) {
                    return Err(("Multiple definitions", sp.clone()));
                }

                if let Some(expr) = op {
                    let expr_info = dfs(expr, scope, ctx)?;
                    if *ty != expr_info.ty {
                        // 声明类型和初始化表达式类型不匹配
                        if ty.is_primitive() || !expr_info.ty.is_null() {
                            if !(ty.dim > 0 && expr_info.ty.name == "{}") {
                                return Err(("Type Mismatch", sp.clone()));
                            }
                        }
                    }
                }
                scope.insert_var(name, ty.clone());
            }
            Ok(ExprInfo::void())
        }
        ASTNode::FuncDef(ty, name, args, block, sp) => {
            // 考虑形参可以与函数重名
            scope.push(ScopeType::Func);
            for (ty, name) in args {
                scope.insert_var(name, ty.clone());
            }
            dfs(block, scope, ctx)?;
            if !ty.is_void() && ctx.ret_types.is_empty() && *name != "main" {
                return Err(("Missing Return Statement", sp.clone()));
            }
            for ret_ty in ctx.ret_types.iter() {
                if *ret_ty != *ty {
                    // 函数返回类型和返回表达式类型不匹配
                    if ty.is_primitive() || !ret_ty.is_null() {
                        return Err(("Type Mismatch", sp.clone()));
                    }
                }
            }
            scope.pop();
            ctx.ret_types.clear();
            Ok(ExprInfo::void())
        }
        ASTNode::ConstrDef(name, block, sp) => {
            if let Some(class_name) = scope.get_class_name() {
                if **name != *class_name {
                    // 构造函数名和类名不匹配
                    return Err(("Invalid Identifier", sp.clone()));
                }
            } else {
                return Err(("Invalid Identifier", sp.clone()));
            }
            scope.push(ScopeType::Func);
            dfs(block, scope, ctx)?;
            for ret_ty in ctx.ret_types.iter() {
                if !ret_ty.is_void() {
                    return Err(("Type Mismatch", sp.clone()));
                }
            }
            scope.pop();
            ctx.ret_types.clear();
            Ok(ExprInfo::void())
        }
        ASTNode::ClassDef(name, ch, _) => {
            scope.push(ScopeType::Class(name));
            for mem in ch {
                match mem {
                    ASTNode::VarDecl(ty, ch, _) => {
                        for (name, _) in ch {
                            scope.insert_var(name, ty.clone());
                        }
                    }
                    ASTNode::FuncDef(ret_ty, name, args, _, _) => {
                        let args = args.iter().map(|(ty, _)| ty.clone()).collect();
                        scope.insert_func(name, ret_ty, &args);
                    }
                    _ => {}
                }
            }
            for mem in ch {
                match mem {
                    ASTNode::FuncDef(_, _, _, _, _) => {
                        dfs(mem, scope, ctx)?;
                    }
                    ASTNode::ConstrDef(_, _, _) => {
                        dfs(mem, scope, ctx)?;
                    }
                    _ => {}
                }
            }
            scope.pop();
            Ok(ExprInfo::void())
        }
        ASTNode::Block(ch, _) => {
            scope.push(ScopeType::Block);
            for node in ch {
                dfs(node, scope, ctx)?;
            }
            scope.pop();
            Ok(ExprInfo::void())
        }
        ASTNode::ThisExpr(sp) => {
            match scope.get_class_name() {
                Some(name) => {
                    Ok(ExprInfo::normal_var(name))
                }
                None => return Err(("Invalid Identifier", sp.clone()))
            }
        }
        ASTNode::ArrayInit(name, sizes, op, sp) => {
            if let Some(_) = scope.find_class(name) {
                let my_type = Type { name, dim: sizes.len() as i32 };

                for node in sizes {
                    if let Some(expr) = node {
                        let expr_info = dfs(expr, scope, ctx)?;
                        if !expr_info.ty.is_int() {
                            return Err(("Type Mismatch", sp.clone()));
                        }
                    }
                }

                if let Some(expr) = op {
                    let expr_info = dfs(expr, scope, ctx)?;
                    if my_type != expr_info.ty && expr_info.ty.name != "{}" {
                        return Err(("Type Mismatch", sp.clone()));
                    }
                }
                Ok(ExprInfo {
                    ty: my_type,
                    is_left: false,
                    func: None,
                })
            } else {
                return Err(("Undefined Identifier", sp.clone()));
            }
        }
        ASTNode::ClassInit(name, sp) => {
            if let Some(_) = scope.find_class(name) {
                Ok(ExprInfo::normal_var(name))
            } else {
                return Err(("Undefined Identifier", sp.clone()));
            }
        }
        ASTNode::BinaryExpr(name, lhs, rhs, sp) => {
            let lhs_info = dfs(lhs, scope, ctx)?;
            let rhs_info = dfs(rhs, scope, ctx)?;
            if lhs_info.ty != rhs_info.ty {
                // 先解决类型不等的情况
                if *name == "=" {
                    return if lhs_info.is_left && rhs_info.ty.is_null() {
                        if lhs_info.ty.is_primitive() {
                            Err(("Type Mismatch", sp.clone()))
                        } else {
                            Ok(ExprInfo::void())
                        }
                    } else {
                        Err(("Type Mismatch", sp.clone()))
                    };
                }
                if *name == "==" || *name == "!=" {
                    return if (!lhs_info.ty.is_primitive() && rhs_info.ty.is_null())
                        || (!rhs_info.ty.is_primitive() && lhs_info.ty.is_null()) {
                        Ok(ExprInfo::normal_var("bool"))
                    } else {
                        Err(("Type Mismatch", sp.clone()))
                    };
                }
                return Err(("Type Mismatch", sp.clone()));
            }
            if lhs_info.ty.name == "null" {
                if *name == "==" || *name == "!=" {
                    return Ok(ExprInfo::normal_var("bool"));
                }
                return Err(("Invalid Type", sp.clone()));
            }
            // 类型相等
            match *name {
                "+" => {
                    if lhs_info.ty.is_int() || lhs_info.ty.is_string() {
                        Ok(ExprInfo::normal(lhs_info.ty))
                    } else {
                        return Err(("Invalid Type", sp.clone()));
                    }
                }
                "-" | "*" | "/" | "%" => {
                    if lhs_info.ty.is_int() {
                        Ok(ExprInfo::normal_var("int"))
                    } else {
                        return Err(("Invalid Type", sp.clone()));
                    }
                }
                "<" | ">" | "<=" | ">=" => {
                    if lhs_info.ty.is_int() || lhs_info.ty.is_string() {
                        Ok(ExprInfo::normal_var("bool"))
                    } else {
                        return Err(("Invalid Type", sp.clone()));
                    }
                }
                "==" | "!=" => {
                    Ok(ExprInfo::normal_var("bool"))
                }
                "&&" | "||" => {
                    if lhs_info.ty.is_bool() {
                        Ok(ExprInfo::normal_var("bool"))
                    } else {
                        return Err(("Invalid Type", sp.clone()));
                    }
                }
                "<<" | ">>" | "&" | "|" | "^" => {
                    if lhs_info.ty.is_int() {
                        Ok(ExprInfo::normal_var("int"))
                    } else {
                        return Err(("Invalid Type", sp.clone()));
                    }
                }
                "=" => {
                    if lhs_info.is_left {
                        Ok(ExprInfo::void())
                    } else {
                        return Err(("Invalid Type", sp.clone()));
                    }
                }
                _ => { return Err(("Invalid Identifier", sp.clone())) }
            }
        }
        ASTNode::UnitaryExpr(name, rhs, sp) => {
            let rhs_info = dfs(rhs, scope, ctx)?;

            return match *name {
                "++" | "--" => {
                    if rhs_info.is_left {
                        if rhs_info.ty.is_int() {
                            Ok(ExprInfo::left(rhs_info.ty.clone()))
                        } else {
                            Err(("Invalid Type", sp.clone()))
                        }
                    } else {
                        Err(("Invalid Type", sp.clone()))
                    }
                }
                "!" => {
                    if rhs_info.ty.is_bool() {
                        Ok(ExprInfo::normal_var("bool"))
                    } else {
                        Err(("Invalid Type", sp.clone()))
                    }
                }
                "+" | "-" | "~" => {
                    if rhs_info.ty.is_int() {
                        Ok(ExprInfo::normal_var("int"))
                    } else {
                        Err(("Invalid Type", sp.clone()))
                    }
                }
                _ => { Err(("Invalid Identifier", sp.clone())) }
            };
        }
        ASTNode::TernaryExpr(cond, expr1, expr2, sp) => {
            let cond_info = dfs(cond, scope, ctx)?;

            if cond_info.ty.is_bool() {
                let expr1_info = dfs(expr1, scope, ctx)?;
                let expr2_info = dfs(expr2, scope, ctx)?;

                if expr1_info.ty == expr2_info.ty {
                    Ok(ExprInfo::normal(expr1_info.ty))
                } else {
                    if expr1_info.ty.is_primitive() || expr2_info.ty.is_primitive() {
                        Err(("Type Mismatch", sp.clone()))
                    } else {
                        if expr1_info.ty.is_null() {
                            Ok(ExprInfo::normal(expr2_info.ty))
                        } else if expr2_info.ty.is_null() {
                            Ok(ExprInfo::normal(expr1_info.ty))
                        } else {
                            Err(("Type Mismatch", sp.clone()))
                        }
                    }
                }
            } else {
                Err(("Invalid Type", sp.clone()))
            }
        }
        ASTNode::Increment(lhs, _, sp) => {
            let lhs_info = dfs(lhs, scope, ctx)?;

            if lhs_info.is_left && lhs_info.ty.is_int() {
                Ok(ExprInfo::normal_var("int"))
            } else {
                return Err(("Invalid Type", sp.clone()));
            }
        }
        ASTNode::ArrayAccess(lhs, inner, sp) => {
            let lhs_info = dfs(lhs, scope, ctx)?;
            if lhs_info.ty.dim > 0 {
                let inner_info = dfs(inner, scope, ctx)?;
                if inner_info.ty.is_int() {
                    Ok(ExprInfo::left(Type {
                        name: &lhs_info.ty.name,
                        dim: lhs_info.ty.dim - 1,
                    }))
                } else {
                    return Err(("Invalid Type", sp.clone()));
                }
            } else {
                return Err(("Dimension Out Of Bound", sp.clone()));
            }
        }
        ASTNode::MemberAccess(lhs, name, sp) => {
            let lhs_info = dfs(lhs, scope, ctx)?;
            if let Some(members) = scope.find_class(&lhs_info.ty.name) {
                if let Some(member) = members.get(*name) {
                    match member {
                        Member::Var(ty) => {
                            Ok(ExprInfo::left(ty.clone()))
                        }
                        Member::Func(ret_ty, args) => {
                            Ok(ExprInfo {
                                ty: Type::func(),
                                is_left: false,
                                func: Some((ret_ty.clone(), args.clone())),
                            })
                        }
                    }
                } else {
                    if lhs_info.ty.dim > 0 && *name == "size" {
                        return Ok(ExprInfo::normal_var("#FUNC_SIZE#"));
                    }
                    return Err(("Undefined Identifier", sp.clone()));
                }
            } else {
                return Err(("Undefined Identifier", sp.clone()));
            }
        }
        ASTNode::FuncCall(lhs, params, sp) => {
            let lhs_info = dfs(lhs, scope, ctx)?;
            if lhs_info.ty.name == "#FUNC_SIZE#" {
                if params.len() != 0 {
                    return Err(("Type Mismatch", sp.clone()));
                }
                return Ok(ExprInfo::normal_var("int"));
            }
            if lhs_info.ty.name != "#FUNC#" {
                return Err(("Undefined Identifier", sp.clone()));
            }


            let (ret_ty, args) = lhs_info.func.unwrap();

            if args.len() != params.len() {
                return Err(("Type Mismatch", sp.clone()));
            }
            let my_type = ret_ty.clone();
            for (arg_ty, param) in args.iter().zip(params) {
                let param_info = dfs(param, scope, ctx)?;
                if *arg_ty != param_info.ty {
                    if arg_ty.is_primitive() || param_info.ty.name != "null" {
                        return Err(("Type Mismatch", sp.clone()));
                    }
                }
            }
            Ok(ExprInfo::normal(my_type))
        }
        ASTNode::IfStmt(cond, if_block, else_block, sp) => {
            let cond_info = dfs(cond, scope, ctx)?;
            if cond_info.ty.is_bool() {
                dfs(if_block, scope, ctx)?;
                if let Some(else_block) = else_block {
                    dfs(else_block, scope, ctx)?;
                }
                Ok(ExprInfo::void())
            } else {
                return Err(("Invalid Type", sp.clone()));
            }
        }
        ASTNode::ForStmt(expr1, expr2, expr3, block, sp) => {
            scope.push(ScopeType::Loop);
            if let Some(expr1) = expr1 {
                dfs(expr1, scope, ctx)?;
            }
            if let Some(expr2) = expr2 {
                let expr2_info = dfs(expr2, scope, ctx)?;
                if !expr2_info.ty.is_bool() {
                    return Err(("Invalid Type", sp.clone()));
                }
            }
            if let Some(expr3) = expr3 {
                dfs(expr3, scope, ctx)?;
            }
            dfs(block, scope, ctx)?;
            scope.pop();
            Ok(ExprInfo::void())
        }
        ASTNode::WhileStmt(cond, block, sp) => {
            scope.push(ScopeType::Loop);
            if let Some(expr) = cond {
                let expr_info = dfs(expr, scope, ctx)?;
                if !expr_info.ty.is_bool() {
                    return Err(("Invalid Type", sp.clone()));
                }
            }
            dfs(block, scope, ctx)?;
            scope.pop();
            Ok(ExprInfo::void())
        }
        ASTNode::ReturnStmt(ret, sp) => {
            if !scope.is_in_func() {
                return Err(("Invalid Control Flow", sp.clone()));
            }
            if let Some(expr) = ret {
                let expr_info = dfs(expr, scope, ctx)?;
                ctx.ret_types.push(expr_info.ty);
            } else {
                ctx.ret_types.push(Type::void());
            }
            Ok(ExprInfo::void())
        }
        ASTNode::BreakStmt(sp) => {
            if !scope.is_in_loop() {
                return Err(("Invalid Control Flow", sp.clone()));
            }
            Ok(ExprInfo::void())
        }
        ASTNode::ContinueStmt(sp) => {
            if !scope.is_in_loop() {
                return Err(("Invalid Control Flow", sp.clone()));
            }
            Ok(ExprInfo::void())
        }
        ASTNode::NULL(_) => {
            Ok(ExprInfo::normal_var("null"))
        }
        ASTNode::Int(_, _) => {
            Ok(ExprInfo::normal_var("int"))
        }
        ASTNode::Str(_, _) => {
            Ok(ExprInfo::normal_var("string"))
        }
        ASTNode::Bool(_, _) => {
            Ok(ExprInfo::normal_var("bool"))
        }
        ASTNode::ArrConst(ch, sp) => {
            let mut my_type_op = None;
            for node in ch {
                let const_info = dfs(node, scope, ctx)?;
                if !my_type_op.is_none() {
                    let my_type: Type = my_type_op.unwrap();
                    if my_type.dim != const_info.ty.dim && const_info.ty.name != "{}" {
                        return Err(("Type Mismatch", sp.clone()));
                    }

                    if my_type != const_info.ty {
                        if my_type.name == "{}" {
                            my_type_op = Some(const_info.ty);
                        } else if const_info.ty.name != "{}" {
                            return Err(("Type Mismatch", sp.clone()));
                        }
                    }
                } else {
                    my_type_op = Some(const_info.ty);
                }
            }
            if my_type_op.is_none() {
                my_type_op = Some(Type { name: "{}", dim: 0 });
            }
            Ok(ExprInfo::normal(Type {
                name: &my_type_op.unwrap().name,
                dim: &my_type_op.unwrap().dim + 1,
            }))
        }
        ASTNode::FmtStr(ch, sp) => {
            for node in ch {
                let info = dfs(node, scope, ctx)?;
                if info.ty.dim != 0 {
                    return Err(("Invalid Type", sp.clone()));
                }
                match info.ty.name {
                    "int" | "string" | "bool" => {}
                    _ => return Err(("Invalid Type", sp.clone()))
                }
            }
            Ok(ExprInfo::normal_var("string"))
        }
        ASTNode::Ident(name, sp) => {
            if let Some(res) = scope.find_ident(name) {
                Ok(res)
            } else {
                Err(("Undefined Identifier", sp.clone()))
            }
        }
    }
}