use std::collections::HashMap;
use super::ASTNode;
use super::Type;
use super::utils::{ExprInfo, Context};
use super::scope::{Scope, ScopeType, Member};


pub fn check(ast: &ASTNode) -> Result<(), &'static str> {
    let mut scope = Scope::new();
    let mut ctx = Context::new();
    dfs(ast, &mut scope, &mut ctx)?;
    Ok(())
}

fn dfs<'a>(ast: &ASTNode<'a>, scope: &mut Scope<'a>, ctx: &mut Context<'a>) -> Result<ExprInfo<'a>, &'static str> {
    match ast {
        ASTNode::Root(ch) => {
            // 先收集类名
            for node in ch {
                match node {
                    ASTNode::ClassDef(name, _) => {
                        if let Some(_) = scope.find_class(name) {
                            return Err("Multiple definition");
                        }
                        scope.insert_class(name, HashMap::new());
                    }
                    _ => {}
                }
            }

            let mut main = false;
            for node in ch {
                match node {
                    ASTNode::FuncDef(ty, name, args, _) => {
                        if let Some(_) = scope.find_ident(name) {
                            return Err("Multiple definition");
                        }
                        if let Some(_) = scope.find_class(name) {
                            return Err("Function name conflict with class name");
                        }
                        if *name == "main" {
                            main = true;
                            if !ty.is_int() {
                                return Err("Main function should return int.");
                            }
                            if !args.is_empty() {
                                return Err("Main function should have no arguments.");
                            }
                        }

                        let mut my_args = vec![];
                        for (ty, _) in args {
                            if ty.name == "void" {
                                return Err("Cannot declare void params!");
                            }
                            if scope.find_class(ty.name).is_none() {
                                return Err("Param type not found.");
                            }
                            my_args.push(ty.clone());
                        }

                        scope.insert_func(name, ty, &my_args);
                    }
                    ASTNode::ClassDef(name, ch) => {
                        match scope.find_ident_top(name) {
                            Some(Member::Func(_, _)) => {
                                return Err("Class name conflict with function name");
                            }
                            _ => {}
                        }

                        let mut members = HashMap::new();
                        let mut constr = false;
                        for mem in ch {
                            match mem {
                                ASTNode::ConstrDef(_, _) => {
                                    if constr {
                                        // Two construct function
                                        return Err("Two construct function");
                                    }
                                    constr = true;
                                }
                                ASTNode::VarDecl(ty, ch) => {
                                    if ty.name == "void" {
                                        return Err("Cannot declare void vars!");
                                    }
                                    if scope.find_class(ty.name).is_none() {
                                        return Err("Member type not found.");
                                    }
                                    for (name, op) in ch {
                                        if members.contains_key(name) {
                                            // 重复定义
                                            return Err("Duplicate var definition");
                                        }
                                        if let Some(_) = op {
                                            // 类成员默认初始化表达式为非法
                                            return Err("Class member default init");
                                        }
                                        members.insert(*name, Member::Var(ty.clone()));
                                    }
                                }
                                ASTNode::FuncDef(ty, name, args, _) => {
                                    if members.contains_key(name) {
                                        // 重复定义
                                        return Err("Method name conflict with member or function");
                                    }
                                    let mut my_args = vec![];
                                    for (ty, _) in args {
                                        if ty.name == "void" {
                                            return Err("Cannot declare void params!");
                                        }
                                        if scope.find_class(ty.name).is_none() {
                                            return Err("Param type not found.");
                                        }
                                        my_args.push(ty.clone());
                                    }

                                    members.insert(*name, Member::Func(ty.clone(), my_args));
                                }
                                _ => { return Err("What the fuck is the class?") }
                            }
                        }
                        scope.insert_class(name, members);
                    }
                    _ => {}
                }
            }
            if !main {
                return Err("No main function!");
            }

            for node in ch {
                dfs(node, scope, ctx)?;
            }
            Ok(ExprInfo::void())
        }
        ASTNode::VarDecl(ty, ch) => {
            if ty.name == "void" {
                return Err("Cannot declare void vars!");
            }
            if scope.find_class(ty.name).is_none() {
                return Err("Var type not found!");
            }
            for (name, op) in ch {
                if let Some(_) = scope.find_ident_top(name) {
                    return Err("Multiple definition");
                }

                if let Some(expr) = op {
                    let expr_info = dfs(expr, scope, ctx)?;
                    if *ty != expr_info.ty {
                        // 声明类型和初始化表达式类型不匹配
                        if ty.is_primitive() || expr_info.ty.name != "null" {
                            if !(ty.dim > 0 && expr_info.ty.name == "{}") {
                                return Err("VarDecl. Type doesn't match.");
                            }
                        }
                    }
                }
                scope.insert_var(name, ty.clone());
            }
            Ok(ExprInfo::void())
        }
        ASTNode::FuncDef(ty, name, args, block) => {
            // 考虑形参可以与函数重名
            scope.push(ScopeType::Func);
            for (ty, name) in args {
                scope.insert_var(name, ty.clone());
            }
            dfs(block, scope, ctx)?;
            if ty.name != "void" && ctx.ret_types.is_empty() && *name != "main" {
                return Err("FuncDef. Non void function should have return value.");
            }
            for ret_ty in ctx.ret_types.iter() {
                if *ret_ty != *ty {
                    // 函数返回类型和返回表达式类型不匹配
                    if ty.is_primitive() || ret_ty.name != "null" {
                        return Err("FuncDef. Type doesn't match.");
                    }
                }
            }
            scope.pop();
            ctx.ret_types.clear();
            Ok(ExprInfo::void())
        }
        ASTNode::ConstrDef(name, block) => {
            if let Some(class_name) = scope.get_class_name() {
                if **name != *class_name {
                    // 构造函数名和类名不匹配
                    return Err("ConstrDef. Name doesn't match.");
                }
            } else {
                return Err("ConstrDef. Not in class.");
            }
            scope.push(ScopeType::Func);
            dfs(block, scope, ctx)?;
            for ret_ty in ctx.ret_types.iter() {
                if ret_ty.name != "void" {
                    return Err("ConstrDef. Cannot return non-void value.");
                }
            }
            scope.pop();
            ctx.ret_types.clear();
            Ok(ExprInfo::void())
        }
        ASTNode::ClassDef(name, ch) => {
            scope.push(ScopeType::Class(name));
            for mem in ch {
                match mem {
                    ASTNode::VarDecl(ty, ch) => {
                        for (name, _) in ch {
                            scope.insert_var(name, ty.clone());
                        }
                    }
                    ASTNode::FuncDef(ret_ty, name, args, _) => {
                        let args = args.iter().map(|(ty, _)| ty.clone()).collect();
                        scope.insert_func(name, ret_ty, &args);
                    }
                    _ => {}
                }
            }
            for mem in ch {
                match mem {
                    ASTNode::FuncDef(_, _, _, _) => {
                        dfs(mem, scope, ctx)?;
                    }
                    ASTNode::ConstrDef(_, _) => {
                        dfs(mem, scope, ctx)?;
                    }
                    _ => {}
                }
            }
            scope.pop();
            Ok(ExprInfo::void())
        }
        ASTNode::Block(ch) => {
            scope.push(ScopeType::Block);
            for node in ch {
                dfs(node, scope, ctx)?;
            }
            scope.pop();
            Ok(ExprInfo::void())
        }
        ASTNode::ThisExpr => {
            match scope.get_class_name() {
                Some(name) => {
                    Ok(ExprInfo::normal_var(name))
                }
                None => return Err("ThisExpr. Not in class.")
            }
        }
        ASTNode::ArrayInit(name, sizes, op) => {
            if let Some(_) = scope.find_class(name) {
                let my_type = Type { name, dim: sizes.len() as i32 };

                for node in sizes {
                    if let Some(expr) = node {
                        let expr_info = dfs(expr, scope, ctx)?;
                        if !expr_info.ty.is_int() {
                            return Err("ArrayInit. Size is not int.");
                        }
                    }
                }

                if let Some(expr) = op {
                    let expr_info = dfs(expr, scope, ctx)?;
                    if my_type != expr_info.ty && expr_info.ty.name != "{}" {
                        return Err("ArrayInit. Type doesn't match.");
                    }
                }
                Ok(ExprInfo {
                    ty: my_type,
                    is_left: false,
                    func: None,
                })
            } else {
                return Err("ArrayInit. Class not found.");
            }
        }
        ASTNode::ClassInit(name) => {
            if let Some(_) = scope.find_class(name) {
                Ok(ExprInfo::normal_var(name))
            } else {
                return Err("ClassInit. Class not found.");
            }
        }
        ASTNode::BinaryExpr(name, lhs, rhs) => {
            let lhs_info = dfs(lhs, scope, ctx)?;
            let rhs_info = dfs(rhs, scope, ctx)?;
            if lhs_info.ty != rhs_info.ty {
                // 先解决类型不等的情况
                if *name == "=" {
                    return if lhs_info.is_left && rhs_info.ty.name == "null" {
                        if lhs_info.ty.is_primitive() {
                            Err("Null cannot be assigned to primitive type variable")
                        } else {
                            Ok(ExprInfo::void())
                        }
                    } else {
                        Err("BinaryExpr. Type doesn't match.")
                    };
                }
                if *name == "==" || *name == "!=" {
                    return if (!lhs_info.ty.is_primitive() && rhs_info.ty.name == "null") || (!rhs_info.ty.is_primitive() && lhs_info.ty.name == "null") {
                        Ok(ExprInfo::normal_var("bool"))
                    } else {
                        Err("BinaryExpr. Type doesn't match.")
                    };
                }
                return Err("BinaryExpr. Type doesn't match.");
            }
            if lhs_info.ty.name == "null" {
                if *name == "==" || *name == "!=" {
                    return Ok(ExprInfo::normal_var("bool"));
                }
                return Err("BinaryExpr. Null type.");
            }
            // 类型相等
            match *name {
                "+" => {
                    if lhs_info.ty.is_int() || lhs_info.ty.is_string() {
                        Ok(ExprInfo::normal(lhs_info.ty))
                    } else {
                        return Err("What the fuck did you add?");
                    }
                }
                "-" | "*" | "/" | "%" => {
                    if lhs_info.ty.is_int() {
                        Ok(ExprInfo::normal_var("int"))
                    } else {
                        return Err("What the fuck did you -*/% ?");
                    }
                }
                "<" | ">" | "<=" | ">=" => {
                    if lhs_info.ty.is_int() || lhs_info.ty.is_string() {
                        Ok(ExprInfo::normal_var("bool"))
                    } else {
                        return Err("What the fuck did you compare?");
                    }
                }
                "==" | "!=" => {
                    Ok(ExprInfo::normal_var("bool"))
                }
                "&&" | "||" => {
                    if lhs_info.ty.is_bool() {
                        Ok(ExprInfo::normal_var("bool"))
                    } else {
                        return Err("What the fuck did you &&|| ?");
                    }
                }
                "<<" | ">>" | "&" | "|" | "^" => {
                    if lhs_info.ty.is_int() {
                        Ok(ExprInfo::normal_var("int"))
                    } else {
                        return Err("What the fuck did you put in logic expr?");
                    }
                }
                "=" => {
                    if lhs_info.is_left {
                        Ok(ExprInfo::void())
                    } else {
                        return Err("Right value assignment");
                    }
                }
                _ => { return Err("What fucking binary operator?") }
            }
        }
        ASTNode::UnitaryExpr(name, rhs) => {
            let rhs_info = dfs(rhs, scope, ctx)?;

            return match *name {
                "++" | "--" => {
                    if rhs_info.is_left {
                        if rhs_info.ty.is_int() {
                            Ok(ExprInfo {
                                ty: rhs_info.ty,
                                is_left: true,
                                func: None,
                            })
                        } else {
                            Err("Only int can be ++/--")
                        }
                    } else {
                        Err("Only left value can be incremented")
                    }
                }
                "!" => {
                    if rhs_info.ty.name == "bool" && rhs_info.ty.dim == 0 {
                        Ok(ExprInfo::normal_var("bool"))
                    } else {
                        Err("Only bool can be !")
                    }
                }
                "+" | "-" | "~" => {
                    if rhs_info.ty.is_int() {
                        Ok(ExprInfo::normal_var("int"))
                    } else {
                        Err("Only int can be +-~")
                    }
                }
                _ => { Err("What the fuck operator?") }
            };
        }
        ASTNode::TernaryExpr(cond, expr1, expr2) => {
            let cond_info = dfs(cond, scope, ctx)?;

            if cond_info.ty.is_bool() {
                let expr1_info = dfs(expr1, scope, ctx)?;
                let expr2_info = dfs(expr2, scope, ctx)?;

                if expr1_info.ty == expr2_info.ty {
                    Ok(ExprInfo::normal(expr1_info.ty))
                } else {
                    if expr1_info.ty.is_primitive() || expr2_info.ty.is_primitive() {
                        Err("TernaryExpr. Expr1 and expr2 type mismatched!")
                    } else {
                        if expr1_info.ty.name == "null" {
                            Ok(ExprInfo::normal(expr2_info.ty))
                        } else if expr2_info.ty.name == "null" {
                            Ok(ExprInfo::normal(expr1_info.ty))
                        } else {
                            Err("TernaryExpr. Expr1 and expr2 type mismatched!")
                        }
                    }
                }
            } else {
                Err("TernaryExpr. Cond is not bool type!")
            }
        }
        ASTNode::Increment(lhs, _) => {
            let lhs_info = dfs(lhs, scope, ctx)?;

            if lhs_info.is_left && lhs_info.ty.is_int() {
                Ok(ExprInfo::normal_var("int"))
            } else {
                return Err("Only left-value int can be incremented.");
            }
        }
        ASTNode::ArrayAccess(lhs, inner) => {
            let lhs_info = dfs(lhs, scope, ctx)?;
            if lhs_info.ty.dim > 0 {
                let inner_info = dfs(inner, scope, ctx)?;
                if inner_info.ty.is_int() {
                    Ok(ExprInfo {
                        ty: Type { name: &lhs_info.ty.name, dim: lhs_info.ty.dim - 1 },
                        is_left: true,
                        func: None,
                    })
                } else {
                    return Err("ArrayAccess. Inner type is not int.");
                }
            } else {
                return Err("Dimension out of bound");
            }
        }
        ASTNode::MemberAccess(lhs, name) => {
            let lhs_info = dfs(lhs, scope, ctx)?;
            if let Some(members) = scope.find_class(&lhs_info.ty.name) {
                if let Some(member) = members.get(*name) {
                    match member {
                        Member::Var(ty) => {
                            Ok(ExprInfo {
                                ty: ty.clone(),
                                is_left: true,
                                
                                func: None,
                            })
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
                    return Err("MemberAccess. Member not found.");
                }
            } else {
                return Err("MemberAccess. LHS is not a class.");
            }
        }
        ASTNode::FuncCall(lhs, params) => {
            // 先找方法，再找全局函数
            let lhs_info = dfs(lhs, scope, ctx)?;
            if lhs_info.ty.name == "#FUNC_SIZE#" {
                if params.len() != 0 {
                    return Err("FuncCall. Size function should have no parameters.");
                }
                return Ok(ExprInfo::normal_var("int"));
            }
            if lhs_info.ty.name != "#FUNC#" {
                return Err("FuncCall. LHS is not a function.");
            }


            let (ret_ty, args) = lhs_info.func.unwrap();

            if args.len() != params.len() {
                return Err("MethodCall. Args number mismatched.");
            }
            let my_type = ret_ty.clone();
            for (arg_ty, param) in args.iter().zip(params) {
                let param_info = dfs(param, scope, ctx)?;
                if *arg_ty != param_info.ty {
                    if arg_ty.is_primitive() || param_info.ty.name != "null" {
                        return Err("MethodCall. Args type mismatched.");
                    }
                }
            }
            Ok(ExprInfo::normal(my_type))
        }
        ASTNode::IfStmt(cond, if_block, else_block) => {
            let cond_info = dfs(cond, scope, ctx)?;
            if cond_info.ty.is_bool() {
                dfs(if_block, scope, ctx)?;
                if let Some(else_block) = else_block {
                    dfs(else_block, scope, ctx)?;
                }
                Ok(ExprInfo::void())
            } else {
                return Err("IfStmt. Cond is not bool type!");
            }
        }
        ASTNode::ForStmt(expr1, expr2, expr3, block) => {
            scope.push(ScopeType::Loop);
            if let Some(expr1) = expr1 {
                dfs(expr1, scope, ctx)?;
            }
            if let Some(expr2) = expr2 {
                let expr2_info = dfs(expr2, scope, ctx)?;
                if !expr2_info.ty.is_bool() {
                    return Err("ForStmt. Expr2 is not bool type!");
                }
            }
            if let Some(expr3) = expr3 {
                dfs(expr3, scope, ctx)?;
            }
            dfs(block, scope, ctx)?;
            scope.pop();
            Ok(ExprInfo::void())
        }
        ASTNode::WhileStmt(cond, block) => {
            scope.push(ScopeType::Loop);
            if let Some(expr) = cond {
                let expr_info = dfs(expr, scope, ctx)?;
                if !expr_info.ty.is_bool() {
                    return Err("WhileStmt. Cond is not bool type!");
                }
            }
            dfs(block, scope, ctx)?;
            scope.pop();
            Ok(ExprInfo::void())
        }
        ASTNode::ReturnStmt(ret) => {
            if !scope.is_in_func() {
                return Err("FuncCall. Not in function.");
            }
            if let Some(expr) = ret {
                let expr_info = dfs(expr, scope, ctx)?;
                ctx.ret_types.push(expr_info.ty);
            } else {
                ctx.ret_types.push(Type::void());
            }
            Ok(ExprInfo::void())
        }
        ASTNode::BreakStmt => {
            if !scope.is_in_loop() {
                return Err("BreakStmt. Not in loop.");
            }
            Ok(ExprInfo::void())
        }
        ASTNode::ContinueStmt => {
            if !scope.is_in_loop() {
                return Err("ContinueStmt. Not in loop.");
            }
            Ok(ExprInfo::void())
        }
        ASTNode::NULL => {
            Ok(ExprInfo::normal_var("null"))
        }
        ASTNode::Int(_) => {
            Ok(ExprInfo::normal_var("int"))
        }
        ASTNode::Str(_) => {
            Ok(ExprInfo::normal_var("string"))
        }
        ASTNode::Bool(_) => {
            Ok(ExprInfo::normal_var("bool"))
        }
        ASTNode::ArrConst(ch) => {
            let mut my_type_op = None;
            for node in ch {
                let const_info = dfs(node, scope, ctx)?;
                if !my_type_op.is_none() {
                    let my_type: Type = my_type_op.unwrap();
                    if my_type.dim != const_info.ty.dim && const_info.ty.name != "{}" {
                        return Err("ArrConst. Dim mismatched.");
                    }

                    if my_type != const_info.ty {
                        if my_type.name == "{}" {
                            my_type_op = Some(const_info.ty);
                        } else if const_info.ty.name != "{}" {
                            return Err("ArrConst. Type mismatched.");
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
        ASTNode::FmtStr(ch) => {
            for node in ch {
                let info = dfs(node, scope, ctx)?;
                if info.ty.dim != 0 {
                    return Err("FmtStr. Type not supported.");
                }
                match info.ty.name {
                    "int" | "string" | "bool" => {}
                    _ => return Err("FmtStr. Type not supported.")
                }
            }
            Ok(ExprInfo::normal_var("string"))
        }
        ASTNode::Ident(name) => {
            if let Some(res) = scope.find_ident(name) {
                Ok(res)
            } else {
                Err("Undefined identifier.")
            }
        }
    }
}