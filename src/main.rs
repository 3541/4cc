extern crate itertools;
//#[macro_use]
extern crate snafu;

mod parse;

use std::collections::HashMap;
use std::env::args;
use std::fs;
use std::path::Path;
use std::process::Command;

use parse::ast::ASTNode;

fn main() {
    let args: Vec<String> = args().collect();
    let path = Path::new(&args[1]);
    let src = fs::read_to_string(path).unwrap();
    let tok = parse::lex(&src);
    println!("Tokens:\n{:#?}", tok);
    match exec(tok, &path) {
        Ok(_) => {}
        Err(e) => eprintln!("{}", e),
    };
}

fn exec(tok: Vec<parse::lex::Token>, path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let ast = parse::parse(tok)?;
    println!("AST:\n{:#?}", ast);
    let out = ast.emit(&mut HashMap::new(), &mut 8)?;
    println!("Emitted:\n{}", out);
    fs::write("/tmp/t.asm", out).expect("Failed to write assembly out");
    println!(
        "{:?}",
        Command::new("nasm")
            .arg("-felf64")
            .arg("/tmp/t.asm")
            .arg("-o /tmp/t.o")
            .output()?
    );
    /*    println!(
        "{:?}",
        Command::new("ld")
            .arg("-dynamic-linker")
            .arg("/lib64/ld-linux-x86-64.so.2")
            .arg("/usr/lib64/crt1.o")
            .arg("/usr/lib64/crti.o")
            .arg("-lc")
            .arg("/tmp/t.o")
            .arg("/usr/lib64/crtn.o")
            .arg(format!(
                "-o{}/{}",
                path.parent().unwrap().to_str().unwrap(),
                path.file_stem().unwrap().to_str().unwrap()
            ))
            .output()?
    );*/
    println!(
        "{:?}",
        Command::new("gcc")
            .arg("/tmp/t.o")
            .arg(format!(
                "-o{}/{}",
                path.parent().unwrap().to_str().unwrap(),
                path.file_stem().unwrap().to_str().unwrap()
            ))
            .output()?
    );

    Ok(())
}
