use std::str::FromStr;
use pest::iterators::Pair;
use super::ASTNode;
use super::utils::Type;
use super::Rule;


pub fn visit_file(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
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
    ASTNode::Root(ast, span)
}
fn visit_var_decl(pair: Pair<'_, Rule>) -> ASTNode {
    let span = pair.as_span();
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
        vars.push((name, expr, -1));
    }
    ASTNode::VarDecl(my_type, vars, span)
}
fn visit_func_def(pair: Pair<'_, Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();

    let ret_type = visit_type(inner_pairs.next().unwrap());

    let func_name = inner_pairs.next().unwrap().as_str();
    let mut params = vec![];
    if let Some(param_list_pair) = inner_pairs.next() {
        let mut param_inner = param_list_pair.into_inner();
        while let Some(param_type) = param_inner.next() {
            let param_name = param_inner.next().unwrap().as_str();
            params.push((visit_type(param_type), param_name, -1));
        }
    }
    let block = visit_block(inner_pairs.next().unwrap());
    ASTNode::FuncDef(ret_type, func_name, params, Box::new(block), span)
}

fn visit_constr_def(pair: Pair<'_, Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let constr_name = inner_pairs.next().unwrap().as_str();
    let block = visit_block(inner_pairs.next().unwrap());
    ASTNode::ConstrDef(constr_name, Box::new(block), span)
}
fn visit_class_def(pair: Pair<'_, Rule>) -> ASTNode {
    let span = pair.as_span();
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
    ASTNode::ClassDef(class_name, class_body, span)
}
fn visit_block(pair: Pair<'_, Rule>) -> ASTNode {
    let span = pair.as_span();
    let block_inner = pair.into_inner().next().unwrap();
    match block_inner.as_rule() {
        Rule::normal_block => visit_normal_block(block_inner),
        Rule::small_block => {
            let mut stmts = vec![];
            if let Some(stmt_pair) = block_inner.into_inner().next() {
                stmts.push(visit_stmt(stmt_pair));
            }
            ASTNode::Block(stmts, span)
        }
        _ => unreachable!()
    }
}
fn visit_normal_block(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut stmts = vec![];
    let mut inner_pairs = pair.into_inner();
    while let Some(pr) = inner_pairs.next() {
        match pr.as_rule() {
            Rule::stmt => stmts.push(visit_stmt(pr)),
            Rule::normal_block => stmts.push(visit_normal_block(pr)),
            _ => { unreachable!() }
        }
    }
    ASTNode::Block(stmts, span)
}

fn visit_stmt(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();

    let stmt_pair = inner_pairs.next().unwrap();
    match stmt_pair.as_rule() {
        Rule::simple_stmt => {
            let Some(simple_inner) = stmt_pair.into_inner().next()
            else { unreachable!() };
            match simple_inner.as_rule() {
                Rule::expr => visit_expr(simple_inner),
                Rule::var_decl => visit_var_decl(simple_inner),
                _ => unreachable!()
            }
        }
        Rule::flow_stmt => {
            let Some(flow_inner) = stmt_pair.into_inner().next()
            else { unreachable!() };
            match flow_inner.as_rule() {
                Rule::break_stmt => ASTNode::BreakStmt(span),
                Rule::continue_stmt => ASTNode::ContinueStmt(span),
                Rule::return_stmt => visit_return_stmt(flow_inner),
                _ => unreachable!()
            }
        }
        Rule::compound_stmt => {
            let Some(compound_inner) = stmt_pair.into_inner().next()
            else { unreachable!() };
            match compound_inner.as_rule() {
                Rule::if_stmt => visit_if_stmt(compound_inner),
                Rule::for_stmt => visit_for_stmt(compound_inner),
                Rule::while_stmt => visit_while_stmt(compound_inner),
                _ => unreachable!()
            }
        }
        _ => unreachable!()
    }
}
fn visit_expr(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let base_expr_pair = inner_pairs.next().unwrap();
    let base_expr = visit_base_expr(base_expr_pair);
    if let Some(expr_tail_pair) = inner_pairs.next() {
        let mut tail_inner_pairs = expr_tail_pair.into_inner();
        let expr_pair1 = tail_inner_pairs.next().unwrap();
        let expr1 = visit_expr(expr_pair1);
        let expr_pair2 = tail_inner_pairs.next().unwrap();
        let expr2 = visit_expr(expr_pair2);
        return ASTNode::TernaryExpr(Box::new(base_expr), Box::new(expr1), Box::new(expr2), span);
    }
    base_expr
}
fn visit_base_expr(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let first_pair = inner_pairs.next().unwrap();
    match first_pair.as_rule() {
        Rule::test => {
            let test = visit_test(first_pair);
            if let Some(expr_pair) = inner_pairs.next() {
                let expr = visit_expr(expr_pair);
                ASTNode::BinaryExpr("=", Box::new(test), Box::new(expr), span)
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
    visit_or_test(pair.into_inner().next().unwrap())
}

fn visit_new_expr(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let init_pair = inner_pairs.next().unwrap();
    match init_pair.as_rule() {
        Rule::class_init => {
            let mut init_inner_pairs = init_pair.into_inner();

            ASTNode::ClassInit(init_inner_pairs.next().unwrap().as_str(), span)
        }
        Rule::array_init => {
            let mut init_inner_pairs = init_pair.into_inner();


            let my_type = init_inner_pairs.next().unwrap().as_str();

            let mut expr_arr = vec![];
            let mut array_const = None;
            for inner_pair in init_inner_pairs {
                match inner_pair.as_rule() {
                    Rule::expr => expr_arr.push(Some(visit_expr(inner_pair))),
                    Rule::blank_bracket => expr_arr.push(None),
                    Rule::arr_const => array_const = Some(Box::new(visit_arr_const(inner_pair))),
                    _ => { unreachable!() }
                }
            }
            ASTNode::ArrayInit(my_type, expr_arr, array_const, span)
        }
        _ => { unreachable!() }
    }
}
fn visit_or_test(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_and_test(inner_pairs.next().unwrap());
    for inner_pair in inner_pairs {
        let rhs = visit_and_test(inner_pair);
        match &lhs {
            ASTNode::Bool(b1, _) => {
                if *b1{
                    return lhs;
                }
                lhs = rhs;
            }
            _ => {
                match &rhs{
                    ASTNode::Bool(b, _)=>{
                        if *b{
                            lhs = ASTNode::BinaryExpr("||", Box::new(lhs), Box::new(rhs), span);
                            return lhs;
                        }else{
                            continue;
                        }
                    }
                    _=>{}
                }
                lhs = ASTNode::BinaryExpr("||", Box::new(lhs), Box::new(rhs), span);
            }
        }
    }
    lhs
}
fn visit_and_test(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_bit_or_test(inner_pairs.next().unwrap());
    for inner_pair in inner_pairs {
        let rhs = visit_bit_or_test(inner_pair);
        match &lhs {
            ASTNode::Bool(b1, _) => {
                if !*b1{
                    return lhs;
                }
                lhs = rhs;
            }
            _ => {
                match &rhs{
                    ASTNode::Bool(b, _)=>{
                        if !*b{
                            lhs = ASTNode::BinaryExpr("&&", Box::new(lhs), Box::new(rhs), span);
                            return lhs;
                        }else{
                            continue;
                        }
                    }
                    _=>{}
                }
                lhs = ASTNode::BinaryExpr("&&", Box::new(lhs), Box::new(rhs), span);
            }
        }
    }
    lhs
}

fn visit_bit_or_test(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_bit_xor_test(inner_pairs.next().unwrap());
    for inner_pair in inner_pairs {
        let rhs = visit_bit_xor_test(inner_pair);
        match &lhs {
            ASTNode::Int(i1, _) => {
                match &rhs {
                    ASTNode::Int(i2, _) => {
                        lhs = ASTNode::Int(i1 | i2, span);
                        continue;
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        lhs = ASTNode::BinaryExpr("|", Box::new(lhs), Box::new(rhs), span);
    }
    lhs
}

fn visit_bit_xor_test(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_bit_and_test(inner_pairs.next().unwrap());
    for inner_pair in inner_pairs {
        let rhs = visit_bit_and_test(inner_pair);
        match &lhs {
            ASTNode::Int(i1, _) => {
                match &rhs {
                    ASTNode::Int(i2, _) => {
                        lhs = ASTNode::Int(i1 ^ i2, span);
                        continue;
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        lhs = ASTNode::BinaryExpr("^", Box::new(lhs), Box::new(rhs), span);
    }
    lhs
}
fn visit_bit_and_test(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_equal_test(inner_pairs.next().unwrap());
    for inner_pair in inner_pairs {
        let rhs = visit_equal_test(inner_pair);
        match &lhs {
            ASTNode::Int(i1, _) => {
                match &rhs {
                    ASTNode::Int(i2, _) => {
                        lhs = ASTNode::Int(i1 & i2, span);
                        continue;
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        lhs = ASTNode::BinaryExpr("&", Box::new(lhs), Box::new(rhs), span);
    }
    lhs
}
fn visit_equal_test(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_comp_test(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() {
        let op = op_pair.as_str();
        let rhs = visit_comp_test(inner_pairs.next().unwrap());
        match &lhs {
            ASTNode::Int(i1, _) => {
                match &rhs {
                    ASTNode::Int(i2, _) => {
                        if op == "==" {
                            lhs = ASTNode::Bool(*i1 == *i2, span);
                        } else {
                            lhs = ASTNode::Bool(*i1 != *i2, span);
                        }
                        continue;
                    }
                    _ => {}
                }
            }
            ASTNode::Bool(b1, _) => {
                match &rhs {
                    ASTNode::Bool(b2, _) => {
                        if op == "==" {
                            lhs = ASTNode::Bool(*b1 == *b2, span);
                        } else {
                            lhs = ASTNode::Bool(*b1 != *b2, span);
                        }
                        continue;
                    }
                    _ => {}
                }
            }
            ASTNode::Ident(n1,_,_,_,_,_)=>{
                match &rhs{
                    ASTNode::Ident(n2,_,_,_,_,_)=>{
                        if *n1 == *n2{
                            if op == "=="{
                                lhs = ASTNode::Bool(true, span);
                            }else{
                                lhs = ASTNode::Bool(false, span);
                            }
                            continue;
                        }
                    }
                    _=>{}
                }
            }
            _ => {}
        }
        lhs = ASTNode::BinaryExpr(op, Box::new(lhs), Box::new(rhs), span);
    }
    lhs
}
fn visit_comp_test(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_shift_expr(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() {
        let op = op_pair.as_str();
        let rhs = visit_shift_expr(inner_pairs.next().unwrap());
        match &lhs {
            ASTNode::Int(i1, _) => {
                match &rhs {
                    ASTNode::Int(i2, _) => {
                        match op {
                            "<" => lhs = ASTNode::Bool(*i1 < *i2, span),
                            "<=" => lhs = ASTNode::Bool(*i1 <= *i2, span),
                            ">" => lhs = ASTNode::Bool(*i1 > *i2, span),
                            ">=" => lhs = ASTNode::Bool(*i1 >= *i2, span),
                            _ => unreachable!()
                        }
                        continue;
                    }
                    _ => {}
                }
            }
            ASTNode::Ident(n1,_,_,_,_,_)=>{
                match &rhs{
                    ASTNode::Ident(n2,_,_,_,_,_)=>{
                        if *n1 == *n2{
                            match op{
                                "<=" => lhs = ASTNode::Bool(true, span),
                                ">=" => lhs = ASTNode::Bool(true, span),
                                "<" => lhs = ASTNode::Bool(false, span),
                                ">" => lhs = ASTNode::Bool(false, span),
                                _=> unreachable!()
                            }
                            continue;
                        }
                    }
                    _=>{}
                }
            }
            _ => {}
        }
        lhs = ASTNode::BinaryExpr(op, Box::new(lhs), Box::new(rhs), span);
    }
    lhs
}
fn visit_shift_expr(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_add_sub_expr(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() {
        let op = op_pair.as_str();
        let rhs = visit_add_sub_expr(inner_pairs.next().unwrap());
        match &lhs {
            ASTNode::Int(i1, _) => {
                match &rhs {
                    ASTNode::Int(i2, _) => {
                        match op {
                            "<<" => lhs = ASTNode::Int(*i1 << *i2, span),
                            ">>" => lhs = ASTNode::Int(*i1 >> *i2, span),
                            _ => unreachable!()
                        }
                        continue;
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        lhs = ASTNode::BinaryExpr(op, Box::new(lhs), Box::new(rhs), span);
    }
    lhs
}

fn visit_add_sub_expr(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_mul_div_mod_expr(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() {
        let op = op_pair.as_str();
        let rhs = visit_mul_div_mod_expr(inner_pairs.next().unwrap());
        match &lhs {
            ASTNode::Int(i1, _) => {
                match &rhs {
                    ASTNode::Int(i2, _) => {
                        match op {
                            "+" => lhs = ASTNode::Int(*i1 + *i2, span),
                            "-" => lhs = ASTNode::Int(*i1 - *i2, span),
                            _ => unreachable!()
                        }
                        continue;
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        lhs = ASTNode::BinaryExpr(op, Box::new(lhs), Box::new(rhs), span);
    }
    lhs
}
fn visit_mul_div_mod_expr(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_unitary_expr(inner_pairs.next().unwrap());
    while let Some(op_pair) = inner_pairs.next() {
        let op = op_pair.as_str();
        let rhs = visit_unitary_expr(inner_pairs.next().unwrap());
        match &lhs {
            ASTNode::Int(i1, _) => {
                match &rhs {
                    ASTNode::Int(i2, _) => {
                        match op {
                            "*" => lhs = ASTNode::Int(*i1 * *i2, span),
                            "/" => {
                                if *i2 != 0 {
                                    lhs = ASTNode::Int(*i1 / *i2, span);
                                }
                            }
                            "%" => lhs = ASTNode::Int(*i1 % *i2, span),
                            _ => unreachable!()
                        }
                        continue;
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        lhs = ASTNode::BinaryExpr(op, Box::new(lhs), Box::new(rhs), span);
    }
    lhs
}
fn visit_unitary_expr(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let first_pair = inner_pairs.next().unwrap();
    match first_pair.as_rule() {
        Rule::unit_op => {
            let op = first_pair.as_str();
            let rhs = visit_unitary_expr(inner_pairs.next().unwrap());
            match &rhs {
                ASTNode::Int(i, _) => {
                    match op {
                        "++" => return ASTNode::Int(*i + 1, span),
                        "--" => return ASTNode::Int(*i - 1, span),
                        "~" => return ASTNode::Int(!*i, span),
                        "+" => return ASTNode::Int(*i, span),
                        "-" => return ASTNode::Int(-*i, span),
                        _ => {}
                    }
                }
                ASTNode::Bool(b, _) => {
                    match op {
                        "!" => return ASTNode::Bool(!*b, span),
                        _ => {}
                    }
                }
                _ => {}
            }
            ASTNode::UnitaryExpr(op, Box::new(rhs), span)
        }
        Rule::suffix_expr => visit_suffix_expr(first_pair),
        _ => unreachable!()
    }
}
fn visit_suffix_expr(pair: Pair<Rule>) -> ASTNode {
    let mut inner_pairs = pair.into_inner();
    let mut lhs = visit_atom(inner_pairs.next().unwrap());
    for inner_pair in inner_pairs {
        let span = inner_pair.as_span();
        match inner_pair.as_rule() {
            Rule::increment => {
                let op = inner_pair.as_str();
                match &lhs {
                    ASTNode::Int(i, _) => {
                        match op {
                            "++" => lhs = ASTNode::Int(*i + 1, span),
                            "--" => lhs = ASTNode::Int(*i - 1, span),
                            _ => unreachable!()
                        }
                        continue;
                    }
                    _ => {}
                }
                lhs = ASTNode::Increment(Box::new(lhs), op, span);
            }
            Rule::array_access => {
                let access_inner_pair = inner_pair.into_inner().next().unwrap();
                lhs = ASTNode::ArrayAccess(Box::new(lhs), Box::new(visit_expr(access_inner_pair)), span, Type::void());
            }
            Rule::member_access => {
                let access_inner_pair = inner_pair.into_inner().next().unwrap();
                lhs = ASTNode::MemberAccess(Box::new(lhs), access_inner_pair.as_str(), span, -1, Type::void());
            }
            Rule::func_call => {
                let params_inner_pair = inner_pair.into_inner().next().unwrap();
                let mut params = vec![];
                for param_pair in params_inner_pair.into_inner() {
                    params.push(visit_expr(param_pair));
                }
                lhs = ASTNode::FuncCall(Box::new(lhs), params, span, Type::void());
            }
            _ => unreachable!()
        }
    }
    lhs
}

fn visit_atom(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let inner_pair = pair.into_inner().next().unwrap();
    match inner_pair.as_rule() {
        Rule::this => ASTNode::ThisExpr(span),
        Rule::CONST => visit_const(inner_pair),
        Rule::fmt_string => visit_fmt_string(inner_pair),
        Rule::ident => ASTNode::Ident(inner_pair.as_str(), span, -1, Type::void(), -1, false),
        Rule::expr => visit_expr(inner_pair),
        _ => unreachable!()
    }
}
fn visit_fmt_string(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut res = vec![];
    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::fmt_char => res.push(visit_str(inner_pair)),
            Rule::inner_expr => res.push(visit_expr(inner_pair.into_inner().next().unwrap())),
            _ => { unreachable!() }
        }
    }
    ASTNode::FmtStr(res, span)
}

fn visit_return_stmt(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    if let Some(inner_pair) = pair.into_inner().next() {
        return ASTNode::ReturnStmt(Some(Box::new(visit_expr(inner_pair))), span);
    }
    ASTNode::ReturnStmt(None, span)
}

fn visit_if_stmt(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let condition = Box::new(visit_expr(inner_pairs.next().unwrap()));

    let if_block = Box::new(visit_block(inner_pairs.next().unwrap()));

    let else_block = if let Some(else_pair) = inner_pairs.next() {
        Some(Box::new(visit_block(else_pair)))
    } else { None };

    ASTNode::IfStmt(condition, if_block, else_block, span)
}
fn visit_for_stmt(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
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
        span,
    )
}
fn visit_while_stmt(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let mut cond_pairs = inner_pairs.next().unwrap().into_inner();
    let cond_expr = if let Some(cond_pair) = cond_pairs.next() {
        Some(Box::new(visit_expr(cond_pair)))
    } else {
        None
    };
    let block_pair = inner_pairs.next().expect("Expected block for 'while' statement");
    ASTNode::WhileStmt(cond_expr, Box::new(visit_block(block_pair)), span)
}
fn visit_int(pair: Pair<Rule>) -> i32 {
    i32::from_str(pair.as_str()).unwrap()
}
fn visit_bool(pair: Pair<'_, Rule>) -> ASTNode {
    let span = pair.as_span();
    match pair.as_str() {
        "true" => ASTNode::Bool(true, span),
        "false" => ASTNode::Bool(false, span),
        _ => unreachable!(),
    }
}
fn visit_str(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let s = pair.as_str();
    if s.chars().next().unwrap() == '"' {
        ASTNode::Str(&s[1..s.len() - 1], span)
    } else {
        ASTNode::Str(s, span)
    }
}

fn visit_arr_const(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let mut res = vec![];
    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::CONST => res.push(visit_const(inner_pair)),
            _ => { unreachable!() }
        }
    }
    ASTNode::ArrConst(res, span)
}


fn visit_const(pair: Pair<Rule>) -> ASTNode {
    let span = pair.as_span();
    let inner_pair = pair.into_inner().next().unwrap();
    match inner_pair.as_rule() {
        Rule::INT => ASTNode::Int(visit_int(inner_pair), span),
        Rule::BOOL => visit_bool(inner_pair),
        Rule::STRING => visit_str(inner_pair),
        Rule::arr_const => visit_arr_const(inner_pair),
        Rule::NULL => ASTNode::NULL(span),
        _ => unreachable!(),
    }
}

fn visit_type(pair: Pair<Rule>) -> Type {
    let pairs: Vec<_> = pair.into_inner().collect();
    let name = pairs[0].as_str();
    let dim = pairs.len() as i32 - 1;
    Type { name, dim }
}