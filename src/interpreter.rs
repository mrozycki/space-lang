use crate::{
    ast::{Expression, Statement},
    lexer::{Token, TokenType},
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Value {
    Number(i64),
    String(String),
}

impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::Number(n) => format!("{}", n),
            Value::String(s) => s.clone(),
        }
    }
}

#[derive(Debug, Default)]
pub struct Scope<'a> {
    parent: Option<&'a mut Scope<'a>>,
    variables: HashMap<String, Value>,
    functions: HashMap<String, Statement>,
    is_block_scope: bool,
}

impl<'a> Scope<'a> {
    pub fn with_functions(functions: HashMap<String, Statement>) -> Self {
        Self {
            functions,
            ..Default::default()
        }
    }

    /// Checks is a variable is defined in the current scope or any parent scopes
    pub fn is_var_defined(&self, ident: &String) -> bool {
        let mut scope = self;
        loop {
            if scope
                .variables
                .keys()
                .filter(|k| k.starts_with(ident))
                .next()
                != None
            {
                return true;
            }

            if let Some(s) = &scope.parent {
                scope = s;
            } else {
                break;
            }
        }

        false
    }

    /// Gets a variable from the current scope or any parent scopes
    pub fn get_var(&self, ident: &String) -> Option<&Value> {
        let mut scope = self;
        loop {
            if let Some(key) = scope
                .variables
                .keys()
                .filter(|k| k.starts_with(ident))
                .next()
            {
                return Some(scope.variables.get(key).unwrap());
            }

            if let Some(s) = &scope.parent {
                scope = s;
            } else {
                break;
            }
        }

        None
    }

    /// Sets a variable in the scope it was originally defined in
    /// Returns a boolean indicating whether the variable was defined
    pub fn set_var(&mut self, ident: &String, val: Value) -> bool {
        if let Some(key) = self
            .variables
            .clone()
            .keys()
            .filter(|k| k.starts_with(ident))
            .next()
        {
            self.variables.insert(key.to_string(), val);
            return true;
        } else if self.parent.is_some() {
            return self.parent.as_deref_mut().unwrap().set_var(ident, val);
        }

        false
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterpreterError {
    message: String,
    line: usize,
    column: usize,
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{} in line {}, col {}",
            self.message, self.line, self.column
        )
    }
}

#[derive(Debug)]
pub struct Interpreter<'a> {
    scope: Scope<'a>,
    body: Vec<Statement>,
    line: usize,
    column: usize,
}

impl<'a> Interpreter<'a> {
    pub fn with_ast(ast: Vec<Statement>) -> Self {
        let mut functions = HashMap::new();
        let mut body = Vec::new();

        for stmt in ast {
            if let Statement::FunctionDefinition {
                name,
                parameters: _,
                body: _,
            } = stmt.clone()
            {
                functions.insert(name, stmt);
            } else {
                body.push(stmt);
            };
        }

        Self {
            scope: Scope::with_functions(functions),
            body,
            line: 0,
            column: 0,
        }
    }

    fn get_token_type(&mut self, token: Token) -> TokenType {
        self.line = token.line;
        self.column = token.column;
        token.token_type
    }

    fn error(&self, message: String) -> InterpreterError {
        InterpreterError {
            line: self.line,
            column: self.column,
            message,
        }
    }

    fn eval_literal_token(&mut self, token: Token) -> Result<Value, InterpreterError> {
        match self.get_token_type(token) {
            TokenType::Number(num_str) => {
                let num = num_str.parse::<i64>().unwrap();
                Ok(Value::Number(num))
            }
            TokenType::String(s) => Ok(Value::String(s)),
            _ => unreachable!(),
        }
    }

    fn eval_variable_token(&mut self, token: Token) -> Result<Value, InterpreterError> {
        match self.get_token_type(token) {
            TokenType::Identifier(ident, args) => {
                if args.len() != 0 {
                    return Err(self.error(
                        "variable identifiers cannot contain backtick arguments".to_owned(),
                    ));
                }

                if let Some(v) = self.scope.get_var(&ident) {
                    Ok(v.clone())
                } else {
                    Err(self.error(format!("undefined reference to variable `{}`", ident)))
                }
            }
            _ => unreachable!(),
        }
    }

    fn eval_assignment(
        &mut self,
        variable: Token,
        expr: Box<Expression>,
    ) -> Result<Value, InterpreterError> {
        match self.get_token_type(variable) {
            TokenType::Identifier(ident, args) => {
                if args.len() != 0 {
                    return Err(self.error(
                        "variable identifiers cannot contain backtick arguments".to_owned(),
                    ));
                }

                let val = self.eval(*expr)?;
                if self.scope.set_var(&ident, val.clone()) {
                    Ok(val)
                } else {
                    Err(self.error(format!(
                        "attempt to assign to undefined variable `{}`",
                        ident
                    )))
                }
            }
            _ => unreachable!(),
        }
    }

    fn eval(&mut self, expr: Expression) -> Result<Value, InterpreterError> {
        match expr {
            Expression::Literal { value } => self.eval_literal_token(value),
            Expression::Variable { name } => self.eval_variable_token(name),
            Expression::Assignment { variable, value } => self.eval_assignment(variable, value),
            _ => todo!(),
        }
    }

    pub fn run(&mut self) -> Result<(), InterpreterError> {
        for stmt in self.body.clone() {
            match stmt {
                Statement::Expression { expr } => {
                    self.eval(expr)?;
                }
                Statement::Print { expr, newline } => {
                    let mut str = self.eval(expr)?.to_string();
                    if newline {
                        str.push('\n')
                    }
                    print!("{}", str);
                }
                Statement::Definition { variable, value } => {
                    if let TokenType::Identifier(ident, args) = self.get_token_type(variable) {
                        if args.len() != 0 {
                            return Err(self.error(
                                "variable identifiers cannot contain backtick arguments".to_owned(),
                            ));
                        }

                        if self.scope.is_var_defined(&ident) {
                            return Err(self.error(format!(
                                "attempt to redefine already defined variable `{}`",
                                ident
                            )));
                        }

                        let val = self.eval(value)?.clone();
                        self.scope.variables.insert(ident, val);
                    }
                }
                _ => todo!(),
            }
        }

        Ok(())
    }
}
