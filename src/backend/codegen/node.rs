use std::fmt::Display;

pub enum ASMNode {
    Store(i32, String, String, String), // sw/sh/sb rs2 imm(rs1)
    Load(i32, String, String, String),  // lw/lh/lb rd imm(rs1)
    Lui(String, String),                // lui rd imm
    J(String),                          // j label
    Ret,
    Branch(String, String, String, String), // beq/bne rs1, rs2, label
    Arith(String, String, String, String),  // op rd rs1 rs2
    ArithI(String, String, String, String), // op rd rs1 imm
    Move(String, String),                   // mv rd rs
    Label(String),
    Call(String), // call symbol
    Segment(String),
    Global(String),
    Data(i32, i32), // length, value
    Str(String),
    Section(String),
    Align(i32),
}

impl Display for ASMNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASMNode::Store(sz, rs2, imm, rs1) => {
                let inst = match sz {
                    1 => "sb",
                    2 => "sh",
                    4 => "sw",
                    _ => unreachable!(),
                };
                write!(f, "{} {}, {}({})\n", inst, rs2, imm, rs1)
            }
            ASMNode::Load(sz, rd, imm, rs1) => {
                let inst = match sz {
                    1 => "lb",
                    2 => "lh",
                    4 => "lw",
                    _ => unreachable!(),
                };
                write!(f, "{} {}, {}({})\n", inst, rd, imm, rs1)
            }
            ASMNode::Lui(rd, imm) => {
                write!(f, "lui {}, {}\n", rd, imm)
            }
            ASMNode::J(label) => {
                write!(f, "j {}\n", label)
            }
            ASMNode::Ret => {
                write!(f, "ret\n")
            }
            ASMNode::Branch(op, rs1, rs2, label) => {
                write!(f, "{} {}, {}, {}\n", op, rs1, rs2, label)
            }
            ASMNode::Arith(op, rd, rs1, rs2) => {
                write!(f, "{} {}, {}, {}\n", op, rd, rs1, rs2)
            }
            ASMNode::ArithI(op, rd, rs1, imm) => {
                write!(f, "{} {}, {}, {}\n", op, rd, rs1, imm)
            }
            ASMNode::Move(rd, rs) => {
                write!(f, "mv {}, {}\n", rd, rs)
            }
            ASMNode::Label(label) => {
                write!(f, "{}:\n", label)
            }
            ASMNode::Call(symbol) => {
                write!(f, "call {}\n", symbol)
            }
            ASMNode::Segment(s) => {
                write!(f, "{}\n", s)
            }
            ASMNode::Global(symbol) => {
                write!(f, ".globl {}\n", symbol)
            }
            ASMNode::Data(len, val) => {
                let name = match len {
                    1 => "byte",
                    2 => "half",
                    4 => "word",
                    _ => unreachable!(),
                };
                write!(f, ".{} {}\n", name, val)
            }
            ASMNode::Str(name) => {
                write!(f, ".asciz \"{}\"\n", name)
            }
            ASMNode::Section(name) => {
                write!(f, ".section {}\n", name)
            }
            ASMNode::Align(align) => {
                write!(f, ".align {}\n", align)
            }
        }
    }
}
