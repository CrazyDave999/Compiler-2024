mod allocator;
mod regalloc;
mod utils;

use super::ir::IRNode;
use allocator::Allocator;

pub use regalloc::pass;
pub use utils::AllocResult;
