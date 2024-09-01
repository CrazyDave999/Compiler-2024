use super::IRNode;
use super::ASMNode;
use super::AllocResult;

pub fn build_asm(alloc_res: AllocResult) -> Result<Vec<ASMNode>, String> {
    let mut asm = Vec::new();
    let mut bss = Vec::new();
    let mut rodata = Vec::new();

    let mut beq_cnt = 0;
    let mut i = 0;
    Ok(asm)
}