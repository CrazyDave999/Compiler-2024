use pest::Span;
use super::Type;
#[derive(Debug, Clone)]
pub enum ASTNode<'a> {
    Root(Vec<ASTNode<'a>>, Span<'a>),
    VarDecl(Type<'a>, Vec<(&'a str, Option<ASTNode<'a>>, i32)>, Span<'a>), // name, expressions
    FuncDef(Type<'a>, &'a str, Vec<(Type<'a>, &'a str, i32)>, Box<ASTNode<'a>>, Span<'a>), // name, parameters, block
    ConstrDef(&'a str, Box<ASTNode<'a>>, Span<'a>), // name, block
    ClassDef(&'a str, Vec<ASTNode<'a>>, Span<'a>), // name, class body
    Block(Vec<ASTNode<'a>>, Span<'a>), // stmts | blocks

    ThisExpr(Span<'a>),

    // two kinds of new expr
    ArrayInit(&'a str, Vec<Option<ASTNode<'a>>>, Option<Box<ASTNode<'a>>>, Span<'a>), // sizes, array literal
    ClassInit(&'a str, Span<'a>),

    BinaryExpr(&'a str, Box<ASTNode<'a>>, Box<ASTNode<'a>>, Span<'a>), // operand, lhs, rhs
    UnitaryExpr(&'a str, Box<ASTNode<'a>>, Span<'a>), // operand, rhs
    TernaryExpr(Box<ASTNode<'a>>, Box<ASTNode<'a>>, Box<ASTNode<'a>>, Span<'a>), // expr(condition), expr1, expr2

    // four kinds of suffixes
    Increment(Box<ASTNode<'a>>, &'a str, Span<'a>), // lhs, op
    ArrayAccess(Box<ASTNode<'a>>, Box<ASTNode<'a>>, Span<'a>, Type<'a>), // lhs, inner expr, type
    MemberAccess(Box<ASTNode<'a>>, &'a str, Span<'a>, i32, Type<'a>), // lhs, name
    FuncCall(Box<ASTNode<'a>>, Vec<ASTNode<'a>>, Span<'a>, Type<'a>), // lhs, parameters, ret_ty

    IfStmt(Box<ASTNode<'a>>, Box<ASTNode<'a>>, Option<Box<ASTNode<'a>>>, Span<'a>), // expr(condition), if-block, else-block
    ForStmt(Option<Box<ASTNode<'a>>>, Option<Box<ASTNode<'a>>>, Option<Box<ASTNode<'a>>>, Box<ASTNode<'a>>, Span<'a>), // expr1, expr2, expr3, block
    WhileStmt(Option<Box<ASTNode<'a>>>, Box<ASTNode<'a>>, Span<'a>), // expr(condition), block
    ReturnStmt(Option<Box<ASTNode<'a>>>, Span<'a>), // return value
    BreakStmt(Span<'a>),
    ContinueStmt(Span<'a>),

    NULL(Span<'a>),
    Int(i32, Span<'a>),
    Str(&'a str, Span<'a>),
    Bool(bool, Span<'a>),
    ArrConst(Vec<ASTNode<'a>>, Span<'a>),
    FmtStr(Vec<ASTNode<'a>>, Span<'a>),
    Ident(&'a str, Span<'a>, i32, Type<'a>, i32, bool), // index, type, cnt, bool(is_global)
}



