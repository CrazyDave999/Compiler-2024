mod node;
mod utils;

mod ir_build;
mod ir_print;

use super::ast;
use utils::IRType;
use utils::escape_string;
use node::IRNode;
pub use ir_build::build_ir;
pub use ir_print::print_ir;
