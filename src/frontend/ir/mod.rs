mod node;
mod utils;

mod ir_build;
mod ir_print;

use super::ast;
use utils::IRType;
use node::IRNode;
pub use ir_build::build_ir;