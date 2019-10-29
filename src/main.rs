extern crate peeking_take_while;

mod parse;

use std::env::args;
use std::fs;
use std::path::Path;
use std::process::Command;

use parse::ast::ASTNode;

fn main() {
    let args: Vec<String> = args().collect();
    let src = fs::read_to_string(&args[1]).unwrap();
    let tok = parse::lex(&src);
    println!("Tokens:\n{:#?}", tok);
    let ast = parse::parse(tok);
    println!("AST:\n{:#?}", ast);
    let out = ast.emit();
    println!("Emitted:\n{}", out);
    fs::write("/tmp/t.asm", out).expect("Failed to write assembly out");
    //    std::process::Command::new("gcc").arg("/tmp/t.asm").arg(format!("-o {}", Path::new(
}
