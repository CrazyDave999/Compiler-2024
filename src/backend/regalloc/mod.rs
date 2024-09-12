mod regalloc;
mod allocator;
mod cfg;
mod utils;

use super::ir::IRNode;
use allocator::Allocator;

pub use utils::AllocResult;
pub use regalloc::pass;