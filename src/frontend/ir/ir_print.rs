use std::fs::File;
use std::io::{self, Write};
use super::IRNode;

pub fn print_ir(ir: Vec<IRNode>) -> io::Result<()>{
    let mut file = File::create("test.ll")?;
    writeln!(file, "
declare dso_local void @print(ptr)
declare dso_local void @println(ptr)
declare dso_local void @printInt(i32)
declare dso_local void @printlnInt(i32)
declare dso_local ptr @getString()
declare dso_local i32 @getInt()
declare dso_local ptr @toString(i32)
declare dso_local ptr @string.string()
declare dso_local i32 @string.length(ptr)
declare dso_local ptr @string.substring(ptr, i32, i32)
declare dso_local i32 @string.parseInt(ptr)
declare dso_local i32 @string.ord(ptr, i32)
declare dso_local ptr @string.add(ptr, ptr)
declare dso_local i1 @string.equal(ptr, ptr)
declare dso_local i1 @string.notEqual(ptr, ptr)
declare dso_local i1 @string.less(ptr, ptr)
declare dso_local i1 @string.lessOrEqual(ptr, ptr)
declare dso_local i1 @string.greater(ptr, ptr)
declare dso_local i1 @string.greaterOrEqual(ptr, ptr)
declare dso_local ptr @malloc(i32)
declare dso_local ptr @allocPtr(i32)
    ")?;
    let mut indent = 0;
    for node in ir {
        if let IRNode::FuncEnd = node {
            indent -= 2;
        }
        if let IRNode::Label(_)=node{
            write!(file, "{}", node)?;
        }else {
            write!(file, "{}{}", " ".repeat(indent), node)?;
        }
        if let IRNode::FuncBegin(_, _, _) = node {
            indent += 2;
        }
    }
    Ok(())
}