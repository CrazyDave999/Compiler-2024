use std::{fs, io};
use std::io::Read;
use pest::Parser;

pub mod frontend;
use frontend::ast;
use frontend::visualize;
use frontend::semantic;


fn main() -> Result<(), Box<dyn std::error::Error>> {
    // let file_name = "test";
    // let input = fs::read_to_string(file_name)?;

    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;


    let pairs_res = ast::MxParser::parse(ast::Rule::file, &input);
    match pairs_res {
        Ok(pairs) => {
            let ast = ast::build(pairs.into_iter().next().unwrap());
            visualize::print_tree(&ast);
            semantic::check(&ast);
        }
        Err(e) => {
            panic!("{}", e)
        }
    }
    Ok(())
}

