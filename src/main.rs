use std::{
    env, fs,
    io::{self, Read, Result},
};

fn process(code: &str) {
    println!("{}", code);
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
