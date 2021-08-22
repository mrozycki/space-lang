use crate::ast::Statement;
use crate::interpreter::Value;
use qp_trie::{wrapper::BString, Trie};

pub type BuiltinFunction = fn(Vec<Value>) -> Result<Value, String>;

fn create_builtin_statement(func: BuiltinFunction) -> Statement {
    Statement::CallBuiltin { function: func }
}

pub fn builtins() -> Trie<BString, Statement> {
    [
        ("pow", pow as BuiltinFunction),
        ("read line", read_line as BuiltinFunction),
        ("print", print as BuiltinFunction),
        ("println", println as BuiltinFunction),
        ("length of", len as BuiltinFunction),
    ]
    .iter()
    .map(|(name, func)| ((*name).into(), create_builtin_statement(*func)))
    .collect()
}

fn pow(args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        Err(format!(
            "builtin `pow` requires exactly 2 arguments, got {}",
            args.len()
        ))
    } else {
        args[0].pow(args[1].clone())
    }
}

fn read_line(_args: Vec<Value>) -> Result<Value, String> {
    let mut buf = String::new();
    std::io::stdin()
        .read_line(&mut buf)
        .map_err(|e| format!("in builtin `read line`: IO Error: {}", e))?;
    Ok(Value::String(buf))
}

fn print(args: Vec<Value>) -> Result<Value, String> {
    for value in args {
        print!("{}", value);
    }
    Ok(Value::Null)
}

fn println(args: Vec<Value>) -> Result<Value, String> {
    for value in args {
        print!("{}", value);
    }
    println!();
    Ok(Value::Null)
}

fn len(arr: Vec<Value>) -> Result<Value, String> {
    match &arr.first() {
        Some(Value::Array(inner)) => Ok(Value::Number(inner.borrow().len() as i64)),
        Some(Value::String(inner)) => Ok(Value::Number(inner.len() as i64)),
        _ => Err("builtin `len` requires an array or string type argument".to_owned()),
    }
}
