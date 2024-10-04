mod cfg_build;
mod dt_build;
mod phi_put;
mod mem2reg;
mod eliminate_phi;

use super::ir::IRNode;
use super::ir::IRType;

pub use mem2reg::pass;
use cfg_build::BasicBlock;
