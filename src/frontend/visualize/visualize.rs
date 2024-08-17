use std::fmt;
use crate::frontend::ast::ASTNode;
use crate::frontend::ast::Type;


impl fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        for _ in 0..self.dim {
            write!(f, "[]")?;
        }
        Ok(())
    }
}

pub fn print_tree(ast: &ASTNode) {
    print_ast(ast, 0);
}
fn show_indent(indent: usize) {
    print!("{}", "  ".repeat(indent as usize));
}
fn print_ast(ast: &ASTNode, indent: usize) {
    match ast {
        ASTNode::Root(ch, _) => {
            show_indent(indent);
            print!("Root\n");
            for node in ch {
                print_ast(node, indent + 2);
            }
        }
        ASTNode::VarDecl(ty, ch, _) => {
            show_indent(indent);
            print!("VarDecl\n");

            for (name, op,_) in ch {
                show_indent(indent);
                print!("{} {}\n", ty, name);
                if let Some(expr) = op {
                    print_ast(expr, indent + 2);
                } else {
                    show_indent(indent + 2);
                    print!("NULL\n");
                }
            }
        }
        ASTNode::FuncDef(ty, name, arg_list, block, _) => {
            show_indent(indent);
            print!("FuncDef\n");
            show_indent(indent);
            print!("{} {}\n", ty, name);
            show_indent(indent);
            print!("Args:\n");
            show_indent(indent);
            for (ty, name,_) in arg_list {
                print!("{} {},", ty, name);
            }
            print!("\n");
            print_ast(block, indent + 2);
        }
        ASTNode::ConstrDef(name, block, _) => {
            show_indent(indent);
            print!("ConstrDef\n");
            show_indent(indent);
            print!("{}\n", name);
            print_ast(block, indent + 2);
        }
        ASTNode::ClassDef(name, ch, _) => {
            show_indent(indent);
            print!("ClassDef\n");
            show_indent(indent);
            print!("{}\n", name);
            show_indent(indent);
            println!("Class Body:");
            for node in ch {
                print_ast(node, indent + 2);
            }
        }
        ASTNode::Block(ch, _) => {
            show_indent(indent);
            print!("Block\n");
            for node in ch {
                print_ast(node, indent + 2);
            }
        }
        ASTNode::ThisExpr(_) => {
            show_indent(indent);
            print!("ThisExpr\n");
        }
        ASTNode::ArrayInit(name, sizes, arr_const, _) => {
            show_indent(indent);
            print!("ArrayInit\n");
            show_indent(indent);
            print!("new {} \n", name);
            show_indent(indent);
            print!("[\n");
            for size in sizes {
                if let Some(size) = size {
                    print_ast(size, indent + 2);
                    show_indent(indent + 2);
                    print!(",\n");
                } else {
                    show_indent(indent + 2);
                    print!("NULL\n");
                }
            }
            show_indent(indent);
            print!("]\n");
            if let Some(arr_const) = arr_const {
                print_ast(arr_const, indent + 2);
            }
        }
        ASTNode::ClassInit(name, _) => {
            show_indent(indent);
            print!("ClassInit\n");
            show_indent(indent);
            print!("new {}\n", name);
        }
        ASTNode::BinaryExpr(op, lhs, rhs, _) => {
            show_indent(indent);
            print!("BinaryExpr\n");
            show_indent(indent);
            print!("{}\n", op);
            show_indent(indent);
            print!("LHS:\n");
            print_ast(lhs, indent + 2);
            show_indent(indent);
            print!("RHS:\n");
            print_ast(rhs, indent + 2);
        }
        ASTNode::UnitaryExpr(op, rhs, _) => {
            show_indent(indent);
            print!("UnitaryExpr\n");
            show_indent(indent);
            print!("{}\n", op);
            show_indent(indent);
            print!("RHS:\n");
            print_ast(rhs, indent + 2);
        }
        ASTNode::TernaryExpr(cond, expr1, expr2, _) => {
            show_indent(indent);
            print!("TernaryExpr\n");
            show_indent(indent);
            print!("Cond:\n");
            print_ast(cond, indent + 2);
            show_indent(indent);
            print!("Expr1:\n");
            print_ast(expr1, indent + 2);
            show_indent(indent);
            print!("Expr2:\n");
            print_ast(expr2, indent + 2);
        }
        ASTNode::Increment(lhs, op, _) => {
            show_indent(indent);
            print!("Increment\n");
            show_indent(indent);
            print!("LHS:\n");
            print_ast(lhs, indent + 2);
            show_indent(indent);
            print!("{}\n", op);
        }
        ASTNode::ArrayAccess(lhs, inner, _,_) => {
            show_indent(indent);
            print!("ArrayAccess\n");
            show_indent(indent);
            print!("LHS:\n");
            print_ast(lhs, indent + 2);
            show_indent(indent);
            print!("Inner:\n");
            print_ast(inner, indent + 2);
        }
        ASTNode::MemberAccess(lhs, name, _, _, _) => {
            show_indent(indent);
            print!("MemberAccess\n");
            show_indent(indent);
            print!("LHS:\n");
            print_ast(lhs, indent + 2);
            show_indent(indent);
            print!("{}\n", name);
        }
        ASTNode::FuncCall(lhs, params, _,_) => {
            show_indent(indent);
            print!("FuncCall\n");
            show_indent(indent);
            print!("LHS:\n");
            print_ast(lhs, indent + 2);
            show_indent(indent);
            print!("Params:\n");
            for param in params {
                print_ast(param, indent + 2);
            }
        }
        ASTNode::IfStmt(cond, if_block, else_block, _) => {
            show_indent(indent);
            print!("IfStmt\n");
            show_indent(indent);
            print!("Cond:\n");
            print_ast(cond, indent + 2);
            show_indent(indent);
            print!("If Block:\n");
            print_ast(if_block, indent + 2);
            if let Some(else_block) = else_block {
                show_indent(indent);
                print!("Else Block:\n");
                print_ast(else_block, indent + 2);
            }
        }
        ASTNode::ForStmt(expr1, expr2, expr3, block, _) => {
            show_indent(indent);
            print!("ForStmt\n");
            show_indent(indent);
            print!("Expr1:\n");
            if let Some(expr1) = expr1 {
                print_ast(expr1, indent + 2);
            } else {
                show_indent(indent + 2);
                print!("NULL\n");
            }
            show_indent(indent);
            print!("Expr2:\n");
            if let Some(expr2) = expr2 {
                print_ast(expr2, indent + 2);
            } else {
                show_indent(indent + 2);
                print!("NULL\n");
            }

            show_indent(indent);
            print!("Expr3:\n");
            if let Some(expr3) = expr3 {
                print_ast(expr3, indent + 2);
            } else {
                show_indent(indent + 2);
                print!("NULL\n");
            }
            print_ast(block, indent + 2);
        }
        ASTNode::WhileStmt(cond, block, _) => {
            show_indent(indent);
            print!("WhileStmt\n");
            if let Some(cond) = cond {
                print_ast(cond, indent + 2);
            } else {
                show_indent(indent + 2);
                print!("NULL\n");
            }
            print_ast(block, indent + 2);
        }
        ASTNode::ReturnStmt(ret, _) => {
            show_indent(indent);
            print!("ReturnStmt\n");
            if let Some(ret) = ret {
                print_ast(ret, indent + 2);
            } else {
                show_indent(indent + 2);
                print!("NULL\n");
            }
        }
        ASTNode::BreakStmt(_) => {
            show_indent(indent);
            print!("BreakStmt\n");
        }
        ASTNode::ContinueStmt(_) => {
            show_indent(indent);
            print!("ContinueStmt\n");
        }
        ASTNode::NULL(_) => {
            show_indent(indent);
            print!("NULL\n");
        }
        ASTNode::Int(val, _) => {
            show_indent(indent);
            print!("Int {}\n", val);
        }
        ASTNode::Str(s, _) => {
            show_indent(indent);
            print!("Str {}\n", s);
        }
        ASTNode::Bool(b, _) => {
            show_indent(indent);
            print!("Bool {}\n", b);
        }
        ASTNode::ArrConst(ch, _) => {
            show_indent(indent);
            print!("ArrConst\n");
            show_indent(indent);
            print!("{{\n");
            for node in ch {
                print_ast(node, indent + 2);
                show_indent(indent + 2);
                print!(",\n");
            }
            show_indent(indent);
            print!("}}\n");
        }
        ASTNode::FmtStr(ch, _) => {
            show_indent(indent);
            print!("FmtStr\n");
            for node in ch {
                print_ast(node, indent + 2);
            }
        }
        ASTNode::Ident(name, _, _, _, _,_) => {
            show_indent(indent);
            print!("Ident {}\n", name);
        }
    }
}