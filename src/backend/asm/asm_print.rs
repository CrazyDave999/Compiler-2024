use std::io;
use std::io::Write;
use super::ASMNode;

pub fn print_asm<W: Write>(asm: &Vec<ASMNode>, os: &mut W) -> io::Result<()> {
    let indent = 4;
    for node in asm{
        if let ASMNode::Label(_)=node{}
        else {
            write!(os, "{}", " ".repeat(indent))?;
        }
        write!(os, "{}", node)?;
    }
    Ok(())
}