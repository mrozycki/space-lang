#![feature(try_trait_v2)]
mod ast;
mod builtins;
mod error_reporter;
mod interpreter;
mod lexer;
mod llvm_generator;
mod parser;

use std::{
    env, fs,
    io::{self, Read, Result},
    rc::Rc,
};

use error_reporter::ConsoleErrorReporter;
use interpreter::{Exec, Interpreter};
use lexer::Lexer;
use llvm_generator::LLVMGenerator;
use parser::Parser;

fn process(code: &str) {
    let error_reporter = Rc::new(ConsoleErrorReporter::new());
    let lexer = Lexer::new(code);
    match lexer.into_tokens() {
        Ok(tokens) => {
            let mut parser = Parser::new(tokens, error_reporter.clone());
            let statements = parser.parse();
            if parser.had_errors() {
                eprintln!("Program had parse errors; returning");
                return;
            }

            let mut generator = LLVMGenerator::new(&statements);
            println!("{}", generator.generate());
            /*let mut interpreter = Interpreter::with_ast(&statements);
            match interpreter.run() {
                Exec::Err(e) => eprintln!("Interpreter error: {}", e),
                Exec::Return(_) => eprintln!("return outside of a function"),
                Exec::Break => eprintln!("break outside of a loop"),
                Exec::Continue => eprintln!("continue outside of a loop"),
                Exec::Ok => (),
            }*/
        }
        Err(e) => {
            eprintln!("Lexer error: {}", e);
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
