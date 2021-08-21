use crate::interpreter::{Value, InterpreterError};
use crate::ast::Statement;
use qp_trie::{wrapper::BString, Trie};

pub type BuiltinFunction = fn(Vec<Value>) -> Result<Value, InterpreterError>;

fn create_builtin_statement(func: BuiltinFunction) -> Statement {
    Statement::CallBuiltin {function: func}
}

pub fn builtins() -> Trie<BString, Statement> {
    [
        ("space test", test), 
    ].iter().map(|(name, func)| ((*name).into(), create_builtin_statement(*func))).collect()
}

fn test(_args: Vec<Value>) -> Result<Value, InterpreterError> {
    Ok(Value::String("test".to_string()))
}
