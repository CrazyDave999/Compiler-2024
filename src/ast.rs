use std::str::FromStr;
use pest::iterators::Pair;
use pest_derive::Parser;
#[derive(Parser)]
#[grammar = "parser.pest"]
pub struct MxParser;

// pub struct Context {}

#[derive(Debug)]
pub enum ASTNode<'a> {
    Root(Vec<ASTNode<'a>>),
    VarDecl(Type<'a>, Vec<(&'a str, Option<ASTNode<'a>>)>), // name, expressions
    FuncDef(Type<'a>, &'a str, Vec<(Type<'a>, &'a str)>, Box<ASTNode<'a>>), // name, parameters, block
    ConstrDef(&'a str, Box<ASTNode<'a>>), // name, block
    ClassDef(&'a str, Vec<ASTNode<'a>>), // name, class body
    Block(Vec<ASTNode<'a>>), // stmt

    ThisExpr,

    // two kinds of new expr
    ArrayInit(&'a str, Vec<Option<ASTNode<'a>>>, Option<Box<ASTNode<'a>>>), // sizes, array literal
    ClassInit(&'a str),

    BinaryExpr(&'a str, Box<ASTNode<'a>>, Box<ASTNode<'a>>), // operand, lhs, rhs
    UnitaryExpr(&'a str, Box<ASTNode<'a>>), // operand, rhs
    TernaryExpr(Box<ASTNode<'a>>, Box<ASTNode<'a>>, Box<ASTNode<'a>>), // expr(condition), expr1, expr2

    // four kinds of suffixes
    Increment(Box<ASTNode<'a>>, &'a str), // lhs, op
    ArrayAccess(Box<ASTNode<'a>>, Box<ASTNode<'a>>), // lhs, inner expr
    MemberAccess(Box<ASTNode<'a>>, &'a str), // lhs, name
    FuncCall(Box<ASTNode<'a>>, Vec<ASTNode<'a>>), // lhs, parameters

    IfStmt(Box<ASTNode<'a>>, Box<ASTNode<'a>>, Option<Box<ASTNode<'a>>>), // expr(condition), if-block, else-block
    ForStmt(Option<Box<ASTNode<'a>>>, Option<Box<ASTNode<'a>>>, Option<Box<ASTNode<'a>>>, Box<ASTNode<'a>>), // expr1, expr2, expr3, block
    WhileStmt(Option<Box<ASTNode<'a>>>, Box<ASTNode<'a>>), // expr(condition), block
    ReturnStmt(Option<Box<ASTNode<'a>>>), // return value
    BreakStmt,
    ContinueStmt,

    NULL,
    Int(i32),
    Str(&'a str),
    Bool(bool),
    ArrConst(Vec<ASTNode<'a>>),
    FmtStr(Vec<ASTNode<'a>>),
    Ident(&'a str),
}

#[derive(Debug)]
pub struct Type<'a> {
    pub name: &'a str,
    pub dim: i32,
}
pub fn visit_file(pair: Pair<Rule>) -> ASTNode {
    let mut ast = vec![];
    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::var_decl => ast.push(visit_var_decl(inner_pair)),
            Rule::func_def => ast.push(visit_func_def(inner_pair)),
            Rule::class_def => ast.push(visit_class_def(inner_pair)),
            Rule::EOI => {}
            _ => unreachable!(),
        }
    }
    ASTNode::Root(ast)
}
fn visit_var_decl(pair: Pair<'_, Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let type_pair = inner_pairs.next().unwrap();
    let my_type = visit_type(type_pair);
    let mut vars = vec![];
    while let Some(var_pair) = inner_pairs.next() {
        let mut var_inner_pairs = var_pair.into_inner();
        let name = var_inner_pairs.next().unwrap().as_str();
        let expr = if let Some(expr_pair) = var_inner_pairs.next() {
            Some(visit_expr(expr_pair))
        } else {
            None
        };
        vars.push((name, expr));
    }
    ASTNode::VarDecl(my_type, vars)
}
fn visit_func_def(pair: Pair<'_, Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();

    let ret_type = visit_type(inner_pairs.next().unwrap());

    let func_name = inner_pairs.next().unwrap().as_str();
    let mut params = vec![];
    if let Some(param_list_pair) = inner_pairs.next() {
        let mut param_inner = param_list_pair.into_inner();
        while let Some(param_type) = param_inner.next() {
            let param_name = param_inner.next().unwrap().as_str();
            params.push((visit_type(param_type), param_name));
        }
    }
    let block = visit_block(inner_pairs.next().unwrap());
    ASTNode::FuncDef(ret_type, func_name, params, Box::new(block))
}

fn visit_constr_def(pair: Pair<'_, Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let constr_name = inner_pairs.next().unwrap().as_str();
    let block = visit_block(inner_pairs.next().unwrap());
    ASTNode::ConstrDef(constr_name, Box::new(block))
}
fn visit_class_def(pair: Pair<'_, Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let class_name = inner_pairs.next().unwrap().as_str();
    let mut class_body = vec![];
    for class_inner_pair in inner_pairs {
        match class_inner_pair.as_rule() {
            Rule::var_decl => class_body.push(visit_var_decl(class_inner_pair)),
            Rule::func_def => class_body.push(visit_func_def(class_inner_pair)),
            Rule::constr_def => class_body.push(visit_constr_def(class_inner_pair)),
            _ => unreachable!(),
        }
    }
    ASTNode::ClassDef(class_name, class_body)
}
fn visit_block(pair: Pair<'_, Rule>) -> ASTNode {
    let mut stmts = vec![];
    let block_inner = pair.into_inner().next().unwrap();
    let mut stmt_pairs = block_inner.into_inner();
    while let Some(stmt_pair) = stmt_pairs.next() {
        stmts.push(visit_stmt(stmt_pair));
    }
    ASTNode::Block(stmts)
}

fn visit_stmt(pair: Pair<'_, Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();

    let stmt_pair = inner_pairs.next().unwrap();
    match stmt_pair.as_rule() {
        Rule::simple_stmt => {
            let Some(simple_inner) = stmt_pair.into_inner().next()
            else { unreachable!() };
            match simple_inner.as_rule() {
                Rule::expr => return visit_expr(simple_inner),
                Rule::var_decl => return visit_var_decl(simple_inner),
                _ => unreachable!()
            }
        }
        Rule::flow_stmt => {
            let Some(flow_inner) = stmt_pair.into_inner().next()
            else { unreachable!() };
            match flow_inner.as_rule() {
                Rule::break_stmt => return ASTNode::BreakStmt,
                Rule::continue_stmt => return ASTNode::ContinueStmt,
                Rule::return_stmt => return visit_return_stmt(flow_inner),
                _ => unreachable!()
            }
        }
        Rule::compound_stmt => {
            let Some(compound_inner) = stmt_pair.into_inner().next()
            else { unreachable!() };
            match compound_inner.as_rule() {
                Rule::if_stmt => return visit_if_stmt(compound_inner),
                Rule::for_stmt => return visit_for_stmt(compound_inner),
                Rule::while_stmt => return visit_while_stmt(compound_inner),
                _ => unreachable!()
            }
        }
        _ => unreachable!()
    }
}
fn visit_expr(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let base_expr_pair = inner_pairs.next().unwrap();
    let base_expr = visit_base_expr(base_expr_pair);
    if let Some(expr_tail_pair) = inner_pairs.next() {
        let mut tail_inner_pairs = expr_tail_pair.into_inner();
        let expr_pair1 = tail_inner_pairs.next().unwrap();
        let expr1 = visit_expr(expr_pair1);
        let expr_pair2 = tail_inner_pairs.next().unwrap();
        let expr2 = visit_expr(expr_pair2);
        return ASTNode::TernaryExpr(Box::new(base_expr), Box::new(expr1), Box::new(expr2));
    }
    return base_expr;
}
fn visit_base_expr(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let first_pair = inner_pairs.next().unwrap();
    match first_pair.as_rule() {
        Rule::test => {
            let test = visit_test(first_pair);
            if let Some(expr_pair) = inner_pairs.next() {
                let expr = visit_expr(expr_pair);
                ASTNode::BinaryExpr("=", Box::new(test), Box::new(expr))
            } else {
                test
            }
        }
        Rule::new_expr => visit_new_expr(first_pair),
        Rule::arr_const => visit_arr_const(first_pair),
        _ => { unreachable!() }
    }
}

fn visit_test(pair: Pair<Rule>) -> ASTNode {
    return visit_or_test(pair.into_inner().next().unwrap());
}

fn visit_new_expr(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let init_pair = inner_pairs.next().unwrap();
    match init_pair.as_rule() {
        Rule::class_init => {
            let mut init_inner_pairs = init_pair.into_inner();

            ASTNode::ClassInit(init_inner_pairs.next().unwrap().as_str())
        }
        Rule::array_init => {
            let init_inner_pairs = init_pair.into_inner();
            let my_type = inner_pairs.next().unwrap().as_str();

            let mut exprs = vec![];
            let mut array_const = None;
            for inner_pair in init_inner_pairs {
                match inner_pair.as_rule() {
                    Rule::expr => exprs.push(Some(visit_expr(inner_pair))),
                    Rule::blank_bracket => exprs.push(None),
                    Rule::arr_const => array_const = Some(Box::new(visit_arr_const(inner_pair))),
                    _ => { unreachable!() }
                }
            }
            ASTNode::ArrayInit(my_type, exprs, array_const)
        }
        _ => { unreachable!() }
    }
}
fn visit_or_test(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_and_test(inner_pairs.next().unwrap());
    for inner_pair in inner_pairs {
        let rhs = visit_and_test(inner_pair);
        lhs = ASTNode::BinaryExpr("||", Box::new(lhs), Box::new(rhs));
    }
    lhs
}
fn visit_and_test(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_bit_or_test(inner_pairs.next().unwrap());
    for inner_pair in inner_pairs {
        let rhs = visit_bit_or_test(inner_pair);
        lhs = ASTNode::BinaryExpr("&&", Box::new(lhs), Box::new(rhs));
    }
    lhs
}

fn visit_bit_or_test(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_bit_xor_test(inner_pairs.next().unwrap());
    for inner_pair in inner_pairs {
        let rhs = visit_bit_xor_test(inner_pair);
        lhs = ASTNode::BinaryExpr("|", Box::new(lhs), Box::new(rhs));
    }
    lhs
}

fn visit_bit_xor_test(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_bit_and_test(inner_pairs.next().unwrap());
    for inner_pair in inner_pairs {
        let rhs = visit_bit_and_test(inner_pair);
        lhs = ASTNode::BinaryExpr("^", Box::new(lhs), Box::new(rhs));
    }
    lhs
}
fn visit_bit_and_test(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_equal_test(inner_pairs.next().unwrap());
    for inner_pair in inner_pairs {
        let rhs = visit_equal_test(inner_pair);
        lhs = ASTNode::BinaryExpr("&", Box::new(lhs), Box::new(rhs));
    }
    lhs
}
fn visit_equal_test(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_comp_test(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() {
        let op = op_pair.as_str();
        let rhs = visit_comp_test(inner_pairs.next().unwrap());
        lhs = ASTNode::BinaryExpr(op, Box::new(lhs), Box::new(rhs));
    }
    lhs
}
fn visit_comp_test(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_shift_test(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() {
        let op = op_pair.as_str();
        let rhs = visit_shift_test(inner_pairs.next().unwrap());
        lhs = ASTNode::BinaryExpr(op, Box::new(lhs), Box::new(rhs));
    }
    lhs
}
fn visit_shift_test(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_add_sub_expr(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() {
        let op = op_pair.as_str();
        let rhs = visit_add_sub_expr(inner_pairs.next().unwrap());
        lhs = ASTNode::BinaryExpr(op, Box::new(lhs), Box::new(rhs));
    }
    lhs
}

fn visit_add_sub_expr(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_mul_div_mod_expr(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() {
        let op = op_pair.as_str();
        let rhs = visit_mul_div_mod_expr(inner_pairs.next().unwrap());
        lhs = ASTNode::BinaryExpr(op, Box::new(lhs), Box::new(rhs));
    }
    lhs
}
fn visit_mul_div_mod_expr(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_unitary_expr(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() {
        let op = op_pair.as_str();
        let rhs = visit_unitary_expr(inner_pairs.next().unwrap());
        lhs = ASTNode::BinaryExpr(op, Box::new(lhs), Box::new(rhs));
    }
    lhs
}
fn visit_unitary_expr(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let first_pair = inner_pairs.next().unwrap();
    match first_pair.as_rule() {
        Rule::unit_op => {
            let op = first_pair.as_str();
            let rhs = visit_unitary_expr(inner_pairs.next().unwrap());
            ASTNode::UnitaryExpr(op, Box::new(rhs))
        }
        Rule::suffix_expr => visit_suffix_expr(first_pair),
        _ => unreachable!()
    }
}
fn visit_suffix_expr(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_atom(inner_pairs.next().unwrap());
    for inner_pair in inner_pairs {
        match inner_pair.as_rule() {
            Rule::increment => {
                let op = inner_pair.as_str();
                lhs = ASTNode::Increment(Box::new(lhs), op);
            }
            Rule::array_access => {
                let access_inner_pair = inner_pair.into_inner().next().unwrap();
                lhs = ASTNode::ArrayAccess(Box::new(lhs), Box::new(visit_expr(access_inner_pair)));
            }
            Rule::member_access => {
                let access_inner_pair = inner_pair.into_inner().next().unwrap();
                lhs = ASTNode::MemberAccess(Box::new(lhs), access_inner_pair.as_str());
            }
            Rule::func_call => {
                let params_inner_pair = inner_pair.into_inner().next().unwrap();
                let mut params = vec![];
                for param_pair in params_inner_pair.into_inner() {
                    params.push(visit_test(param_pair));
                }
                lhs = ASTNode::FuncCall(Box::new(lhs), params);
            }
            _ => unreachable!()
        }
    }
    lhs
}

fn visit_atom(pair: Pair<Rule>) -> ASTNode {
    let inner_pair = pair.into_inner().next().unwrap();
    match inner_pair.as_rule() {
        Rule::this => ASTNode::ThisExpr,
        Rule::CONST => visit_const(inner_pair),
        Rule::fmt_string => visit_fmt_string(inner_pair),
        Rule::ident => ASTNode::Ident(inner_pair.as_str()),
        Rule::expr => visit_expr(inner_pair),
        _ => unreachable!()
    }
}
fn visit_fmt_string(pair: Pair<Rule>) -> ASTNode {
    let mut res = vec![];
    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::fmt_char => res.push(visit_str(inner_pair)),
            Rule::expr => res.push(visit_expr(inner_pair)),
            _ => { unreachable!() }
        }
    }
    ASTNode::FmtStr(res)
}

fn visit_return_stmt(pair: Pair<Rule>) -> ASTNode {
    if let Some(inner_pair) = pair.into_inner().next() {
        return ASTNode::ReturnStmt(Some(Box::new(visit_expr(inner_pair))));
    }
    return ASTNode::ReturnStmt(None);
}

fn visit_if_stmt(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let condition = Box::new(visit_expr(inner_pairs.next().unwrap()));

    let if_block = Box::new(visit_block(inner_pairs.next().unwrap()));

    let else_block = if let Some(else_pair) = inner_pairs.next() {
        Some(Box::new(visit_block(else_pair)))
    } else { None };
    ASTNode::IfStmt(condition, if_block, else_block)
}
fn visit_for_stmt(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let mut for_init_pairs = inner_pairs.next().unwrap().into_inner();
    let init_expr = if let Some(init_pair) = for_init_pairs.next() {
        match init_pair.as_rule() {
            Rule::expr => Some(Box::new(visit_expr(init_pair))),
            Rule::var_decl => Some(Box::new(visit_var_decl(init_pair))),
            _ => unreachable!(),
        }
    } else { None };

    let mut for_cond_pairs = inner_pairs.next().unwrap().into_inner();
    let cond_expr = if let Some(cond_pair) = for_cond_pairs.next() {
        Some(Box::new(visit_expr(cond_pair)))
    } else { None };

    let mut for_update_pairs = inner_pairs.next().unwrap().into_inner();
    let update_expr = if let Some(update_pair) = for_update_pairs.next() {
        Some(Box::new(visit_expr(update_pair)))
    } else { None };

    let block_pair = inner_pairs.next().unwrap();

    ASTNode::ForStmt(
        init_expr,
        cond_expr,
        update_expr,
        Box::new(visit_block(block_pair)),
    )
}
fn visit_while_stmt(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let mut cond_pairs = inner_pairs.next().unwrap().into_inner();
    let cond_expr = if let Some(cond_pair) = cond_pairs.next() {
        Some(Box::new(visit_expr(cond_pair)))
    } else {
        None
    };
    let block_pair = inner_pairs.next().expect("Expected block for 'while' statement");
    ASTNode::WhileStmt(cond_expr, Box::new(visit_block(block_pair)))
}
fn visit_int(pair: Pair<Rule>) -> i32 {
    i32::from_str(pair.as_str()).unwrap()
}
fn visit_bool(pair: Pair<'_, Rule>) -> ASTNode {
    match pair.as_str() {
        "true" => ASTNode::Bool(true),
        "false" => ASTNode::Bool(false),
        _ => unreachable!(),
    }
}
fn visit_str(pair: Pair<Rule>) -> ASTNode {
    ASTNode::Str(pair.as_str())
}

fn visit_arr_const(pair: Pair<Rule>) -> ASTNode {
    let mut res = vec![];
    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::CONST => res.push(visit_const(inner_pair)),
            _ => { unreachable!() }
        }
    }
    ASTNode::ArrConst(res)
}


fn visit_const(pair: Pair<Rule>) -> ASTNode {
    let inner_pair = pair.into_inner().next().unwrap();
    match inner_pair.as_rule() {
        Rule::INT => ASTNode::Int(visit_int(inner_pair)),
        Rule::BOOL => visit_bool(inner_pair),
        Rule::STRING => visit_str(inner_pair),
        Rule::arr_const => visit_arr_const(inner_pair),
        Rule::NULL => ASTNode::NULL,
        _ => unreachable!(),
    }
}

fn visit_type(pair: Pair<Rule>) -> Type {
    let pairs: Vec<_> = pair.into_inner().collect();
    let name = pairs[0].as_str();
    let dim = pairs.len() as i32 - 1;
    Type { name, dim }
}