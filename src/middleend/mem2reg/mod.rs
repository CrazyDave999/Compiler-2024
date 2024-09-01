mod cfg_build;
mod dt_build;
mod phi_put;
mod mem2reg;
mod ssa;

use super::ir::IRNode;
use super::ir::IRType;

pub use mem2reg::pass;
