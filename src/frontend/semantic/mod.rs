mod semantic;
mod utils;

mod scope;

use super::ast::Type;
use super::ast::ASTNode;

pub use semantic::check;