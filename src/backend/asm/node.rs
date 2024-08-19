use std::fmt::Display;

pub enum ASMNode {
    SW(String, i32, String), // sw rs2 imm(rs1)
    LW(String, i32, String), // lw rd imm(rs1)
    La,
    J,
    Ret,
    Branch,
    Arith(String, String, String, String), // op rd rs1 rs2
    ArithI(String, String, String, i32), // op rd rs1 imm
    Label(String),
    Call(String), // call symbol
    Text,
    Global(String),
}

impl Display for ASMNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
            ASMNode::SW(rs2, imm, rs1) => {
                write!(f, "sw {}, {}({})\n", rs2, imm, rs1)
            }
            ASMNode::LW(rd, imm, rs1) => {
                write!(f, "lw {}, {}({})\n", rd, imm, rs1)
            }
            ASMNode::La => {
                write!(f, "la")
            }
            ASMNode::J => {
                write!(f, "j")
            }
            ASMNode::Ret => {
                write!(f, "ret")
            }
            ASMNode::Branch => {
                write!(f, "branch")
            }
            ASMNode::Arith(op, rd, rs1, rs2) => {
                write!(f, "{} {}, {}, {}\n", op, rd, rs1, rs2)
            }
            ASMNode::ArithI(op, rd, rs1, imm) => {
                write!(f, "{} {}, {}, {}\n",op, rd, rs1, imm)
            }
            ASMNode::Label(label) => {
                write!(f, "{}:\n", label)
            }
            ASMNode::Call(symbol) => {
                write!(f, "call {}\n", symbol)
            }
            ASMNode::Text => {
                write!(f, ".text\n")
            }
            ASMNode::Global(symbol) => {
                write!(f, ".global {}\n", symbol)
            }
        }
    }
}