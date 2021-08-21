mod ast;
mod lexer;
mod parser;

use std::{
    env, fs,
    io::{self, Read, Result},
};

use lexer::Lexer;
use parser::Parser;

fn process(code: &str) {
    let lexer = Lexer::new(code);
    match lexer.into_tokens() {
        Ok(tokens) => {
            let parser = Parser::new(tokens);
            println!("{:?}", parser.parse());
        }
        Err(..) => {
            eprintln!("Parser error");
        }
    }
}

fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();

    if args.len() == 1 {
        let mut code = String::new();
        io::stdin().read_to_string(&mut code)?;
        process(&code);
    } else if args.len() == 2 {
        let code = fs::read_to_string(&args[1])?;
        process(&code);
    } else {
        eprintln!("Usage: {} [<filename>]", args[0]);
    }

    Ok(())
}
