use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "frontend/ast/parser.pest"]
pub struct MxParser;
