use crate::interpreter::Value;
use crate::ast::Statement;
use qp_trie::{wrapper::BString, Trie};

pub type BuiltinFunction = fn(Vec<Value>) -> Result<Value, String>;

fn create_builtin_statement(func: BuiltinFunction) -> Statement {
    Statement::CallBuiltin {function: func}
}

pub fn builtins() -> Trie<BString, Statement> {
    [
        ("pow", pow), 
    ].iter().map(|(name, func)| ((*name).into(), create_builtin_statement(*func))).collect()
}

fn pow(args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        Err(format!("pow() requires exactly 2 arguments, got {}", args.len()))
    } else {
        args[0].pow(args[1].clone())
    }
}
