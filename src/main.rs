mod lexer;

use std::{
    env, fs,
    io::{self, Read, Result},
};

use lexer::{Lexer, Token};

fn process(code: &str) {
    let mut lexer = Lexer::new(code);
    #[allow(irrefutable_let_patterns)]
    while let result = lexer.next() {
        match result {
            Ok(Token::Eof) => {
                println!("Eof");
                break;
            }
            Ok(token) => {
                println!("{:?}", token);
            }
            Err(e) => {
                eprintln!("{}", e);
                break;
            }
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
