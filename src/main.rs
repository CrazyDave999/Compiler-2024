use std::fs;
use std::env;
use std::io;
use std::io::Read;
use pest::{Parser, Span};

pub mod frontend;
use frontend::ast;
use frontend::visualize;
use frontend::semantic;
use frontend::ir;


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
            let mut ast = ast::build_tree(pairs.into_iter().next().unwrap());
            match semantic::check(&mut ast) {
                Ok(_) => (),
                Err(e) => fail(e.0)
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
            let mut ast = ast::build_tree(pairs.into_iter().next().unwrap());
            match semantic::check(&mut ast) {
                Ok(_) => (),
                Err(e) => fail(e.0)
            }
        }
        Err(_) => fail("Invalid Identifier")
    }
    Ok(())
}

pub fn detailed_debug(file: &str) -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string(file)?;
    match ast::MxParser::parse(ast::Rule::file, &input) {
        Ok(pairs) => {
            let mut ast = ast::build_tree(pairs.into_iter().next().unwrap());
            visualize::print_tree(&ast);

            match semantic::check(&mut ast) {
                Ok(_) => Ok(()),
                Err((msg, span)) => {
                    println!("\nError: {}\n", msg);
                    print_error_context(&input, &span, msg);
                    Err(Box::new(io::Error::new(io::ErrorKind::Other, msg)))
                }
            }
        }
        Err(e) => {
            println!("\nParse error: {}\n", e);
            Err(Box::new(e))
        }
    }
}

// 打印源代码的错误上下文
fn print_error_context(source: &str, span: &Span, msg: &str) {
    let start = span.start();
    let end = span.end();

    // 获取错误所在行
    let lines: Vec<&str> = source.lines().collect();
    let error_line_index = source[..start].matches('\n').count() as i32;

    eprintln!("Error line: {}", error_line_index + 1);

    for (i, line) in lines.iter().enumerate() {
        if i as i32 >= error_line_index - 2 && i as i32 <= error_line_index {
            eprintln!("{: >4} | {}", i + 1, line);
        }
    }
    let pre_len = source[..start].rfind('\n').unwrap_or(0);
    let start_col = start - pre_len;
    let end_col = if end - start > lines[error_line_index as usize].len() {
        lines[error_line_index as usize].len() + 1
    } else {
        end - pre_len
    };

    eprintln!("{}{}", " ".repeat(start_col + 6), "^".repeat(end_col - start_col));

    eprintln!("{}{}\n", " ".repeat(start_col + 6), msg);
}

fn emit_llvm(file: &str) -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string(file)?;
    match ast::MxParser::parse(ast::Rule::file, &input) {
        Ok(pairs) => {
            let mut ast = ast::build_tree(pairs.into_iter().next().unwrap());
            match semantic::check(&mut ast) {
                Ok(_) => {
                    let ir_nodes = ir::build_ir(&ast);
                    ir::print_ir(ir_nodes).expect("FUCK YOU PRINT_IR!");
                    Ok(())
                }
                Err((msg, span)) => {
                    println!("\nError: {}\n", msg);
                    print_error_context(&input, &span, msg);
                    Err(Box::new(io::Error::new(io::ErrorKind::Other, msg)))
                }
            }
        }
        Err(e) => {
            println!("\nParse error: {}\n", e);
            Err(Box::new(e))
        }
    }
}


fn main() -> Result<(), Box<dyn std::error::Error>> {
    let env_args: Vec<_> = env::args().collect();
    if env_args.len() < 2 {
        fail("Invalid Arguments");
    }
    match env_args[1].as_str() {
        "-fsyntax-only" => syntax_only_oj(),
        "-fsyntax-test" => syntax_only_test(env_args[2].as_str()),
        "-debug" => detailed_debug(env_args[2].as_str()),
        "-emit-llvm" => emit_llvm(env_args[2].as_str()),
        _ => {
            fail("Invalid Arguments");
            Ok(())
        }
    }
}

