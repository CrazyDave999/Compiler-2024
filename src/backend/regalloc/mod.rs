mod regalloc;
mod allocator;
mod utils;
mod alloc_ir;

use super::ir::IRNode;
use allocator::Allocator;

pub use utils::AllocResult;
pub use regalloc::pass;
