use std::fs;
use std::env;
use std::io;
use std::io::Read;
use pest::Parser;

pub mod frontend;
use frontend::ast;
use frontend::visualize;
use frontend::semantic;


fn fail(s: &str) {
    println!("{}", s);
    std::process::exit(1);
}

fn syntax_only_oj() -> Result<(), Box<dyn std::error::Error>> {
    let mut input = String::new();

    match io::stdin().read_to_string(&mut input) {
        Ok(_) => (),
        Err(e) => fail(&format!("Error reading input: {}", e))
    }

    match ast::MxParser::parse(ast::Rule::file, &input) {
        Ok(pairs) => {
            let ast = ast::build_tree(pairs.into_iter().next().unwrap());
            match semantic::check(&ast) {
                Ok(_) => (),
                Err(e) => fail(e)
            }
        }
        Err(_) => fail("Invalid Identifier")
    }
    Ok(())
}

fn syntax_only_test(file: &str) -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string(file).unwrap();
    match ast::MxParser::parse(ast::Rule::file, &input) {
        Ok(pairs) => {
            let ast = ast::build_tree(pairs.into_iter().next().unwrap());
            match semantic::check(&ast) {
                Ok(_) => (),
                Err(e) => fail(e)
            }
        }
        Err(_) => fail("Invalid Identifier")
    }
    Ok(())
}

fn detailed_debug(file: &str) -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string(file).unwrap();
    match ast::MxParser::parse(ast::Rule::file, &input) {
        Ok(pairs) => {
            let ast = ast::build_tree(pairs.into_iter().next().unwrap());
            visualize::print_tree(&ast);
            semantic::check(&ast)?;
            Ok(())
        }
        Err(e) => {
            println!("{}", e);
            Err(Box::new(e))
        }
    }
}


fn main() -> Result<(), Box<dyn std::error::Error>> {
    let env_args: Vec<_> = env::args().collect();
    if env_args.len() < 2 {
        fail("Invalid arguments");
    }
    match env_args[1].as_str() {
        "-fsyntax-only" => syntax_only_oj(),
        "-fsyntax-test" => syntax_only_test(env_args[2].as_str()),
        "-debug" => detailed_debug(env_args[2].as_str()),
        _ => {
            fail("Invalid arguments");
            Ok(())
        }
    }
}

