use super::ASMNode;
use std::io;
use std::io::Write;

pub fn print_asm<W: Write>(asm: &Vec<ASMNode>, os: &mut W) -> io::Result<()> {
    let indent = 4;
    write!(os, "{}.text\n", " ".repeat(indent))?;
    for node in asm {
        if let ASMNode::Label(_) = node {
        } else {
            write!(os, "{}", " ".repeat(indent))?;
        }
        write!(os, "{}", node)?;
    }
    Ok(())
}
