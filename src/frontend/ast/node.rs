use super::Type;
#[derive(Debug)]
pub enum ASTNode<'a> {
    Root(Vec<ASTNode<'a>>),
    VarDecl(Type<'a>, Vec<(&'a str, Option<ASTNode<'a>>)>), // name, expressions
    FuncDef(Type<'a>, &'a str, Vec<(Type<'a>, &'a str)>, Box<ASTNode<'a>>), // name, parameters, block
    ConstrDef(&'a str, Box<ASTNode<'a>>), // name, block
    ClassDef(&'a str, Vec<ASTNode<'a>>), // name, class body
    Block(Vec<ASTNode<'a>>), // stmts | blocks

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

