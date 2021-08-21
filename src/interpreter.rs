use crate::{
    ast::{Expression, Statement},
    lexer::{Token, TokenType},
};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
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

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Number(n) if n > &0 => true,
            Value::String(s) if s.len() > 0 => true,
            _ => false
        }
    }

    pub fn not_equal(&self, rhs: Value) -> Value {
        Value::Number((self != &rhs).into())
    }
}

#[derive(Debug, Default, Clone)]
pub struct Scope {
    variables: HashMap<String, Value>,
    functions: HashMap<String, Statement>
}

impl Scope {
    /// Checks is a variable is defined in the current scope or any parent scopes
    pub fn is_var_defined(&self, ident: &String) -> bool {
        self.variables
            .keys()
            .filter(|k| k.starts_with(ident))
            .next()
            != None
    }

    /// Gets a variable from the current scope or any parent scopes
    pub fn get_var(&self, ident: &String) -> Option<&Value> {
        if let Some(key) = self
            .variables
            .keys()
            .filter(|k| k.starts_with(ident))
            .next()
        {
            return Some(self.variables.get(key).unwrap());
        }

        None
    }

    /// Sets the value of a variable
    /// Returns an error string if the variable is not defined
    pub fn set_var(&mut self, ident: &String, val: Value) -> Result<(), String> {
        if let Some(key) = self
            .variables
            .clone()
            .keys()
            .filter(|k| k.starts_with(ident))
            .next()
        {
            self.variables.insert(key.to_string(), val);
            return Ok(());
        } else {
            return Err(format!(
                "attempt to assign to an undefined variable `{}`",
                ident
            ));
        }
    }

    pub fn add_functions_from_ast(&mut self, ast: &[Statement]) {
        for stmt in ast {
            if let Statement::FunctionDefinition {
                name,
                parameters: _,
                body: _,
            } = &stmt
            {
                self.functions.insert(name.to_string(), stmt.clone());
            };
        }
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
    scope: Scope,
    body: &'a [Statement],
    line: usize,
    column: usize,
    modified_variables: HashSet<String>
}

impl<'a> Interpreter<'a> {
    pub fn new(mut scope: Scope, ast: &'a [Statement]) -> Self {
        scope.add_functions_from_ast(ast);

        Self {
            scope,
            body: ast,
            line: 0,
            column: 0,
            modified_variables: HashSet::new()
        }
    }

    pub fn with_ast(ast: &'a [Statement]) -> Self {
        Self::new(Default::default(), ast)
    }

    fn get_token_type<'b>(&mut self, token: &'b Token) -> &'b TokenType {
        self.line = token.line;
        self.column = token.column;
        &token.token_type
    }

    fn error(&self, message: String) -> InterpreterError {
        InterpreterError {
            line: self.line,
            column: self.column,
            message,
        }
    }

    fn eval_literal_token(&mut self, token: &Token) -> Result<Value, InterpreterError> {
        match self.get_token_type(token) {
            TokenType::Number(num_str) => {
                let num = num_str.parse::<i64>().unwrap();
                Ok(Value::Number(num))
            }
            TokenType::String(s) => Ok(Value::String(s.to_string())),
            _ => unreachable!(),
        }
    }

    fn eval_variable_token(&mut self, token: &Token) -> Result<Value, InterpreterError> {
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
        variable: &Token,
        expr: &Box<Expression>,
    ) -> Result<Value, InterpreterError> {
        match self.get_token_type(variable) {
            TokenType::Identifier(ident, args) => {
                if args.len() != 0 {
                    return Err(self.error(
                        "variable identifiers cannot contain backtick arguments".to_owned(),
                    ));
                }

                let val = self.eval(expr)?;
                self.scope
                    .set_var(&ident, val.clone())
                    .map_err(|e| self.error(e))?;

                self.modified_variables.insert(ident.to_string());
                return Ok(val);
            }
            _ => unreachable!(),
        }
    }

    fn eval_binaryop(&mut self, operator: &Token, left: &Box<Expression>, right: &Box<Expression>) -> Result<Value, InterpreterError> {
        let left_val = self.eval(left)?;
        let right_val = self.eval(right)?;

        match self.get_token_type(operator) {
            TokenType::Plus => todo!(),
            TokenType::Minus => todo!(),
            TokenType::Star => todo!(),
            TokenType::Slash => todo!(),
            TokenType::Modulo => todo!(),
            TokenType::And => todo!(),
            TokenType::Or => todo!(),
            TokenType::Not => todo!(),
            TokenType::Equal => todo!(),
            TokenType::NotEqual => Ok(left_val.not_equal(right_val)),
            TokenType::LessThan => todo!(),
            TokenType::LessThanOrEqual => todo!(),
            TokenType::GreaterThan => todo!(),
            TokenType::GreaterThanOrEqual => todo!(),
            _ => unreachable!()
        }
    }

    fn eval(&mut self, expr: &Expression) -> Result<Value, InterpreterError> {
        match expr {
            Expression::Literal { value } => self.eval_literal_token(value),
            Expression::Variable { name } => self.eval_variable_token(name),
            Expression::Assignment { variable, value } => self.eval_assignment(variable, value),
            Expression::BinaryOp { operator, left, right } => self.eval_binaryop(operator, left, right),
            _ => todo!(),
        }
    }

    pub fn run(&mut self) -> Result<(), InterpreterError> {
        for stmt in self.body {
            match stmt {
                Statement::Expression { expr } => {
                    self.eval(&expr)?;
                }

                Statement::Print { expr, newline } => {
                    let mut str = self.eval(&expr)?.to_string();
                    if *newline {
                        str.push('\n')
                    }
                    print!("{}", str);
                }

                Statement::Definition { variable, value } => {
                    if let TokenType::Identifier(ident, args) = self.get_token_type(&variable) {
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

                        let val = self.eval(&value)?.clone();
                        self.scope.variables.insert(ident.to_string(), val);
                    }
                }

                Statement::Loop { condition, body } => {
                    let loop_ast = match body.as_ref() {
                        Statement::Block { statements } => statements,
                        _ => unreachable!(),
                    };

                    let mut loop_scope = self.scope.clone();
                    loop_scope.add_functions_from_ast(&loop_ast);

                    while self.eval(&condition)?.is_truthy() {
                        let mut loop_interpreter = Interpreter::new(loop_scope.clone(), &loop_ast);
                        loop_interpreter.run()?;
                        
                        for key in &loop_interpreter.modified_variables {
                            match loop_interpreter.scope.variables.get(key) {
                                Some(v) => {
                                    loop_scope.variables.insert(key.to_string(), v.clone());
                                    self.scope.variables.insert(key.to_string(), v.clone());
                                },
                                None => unreachable!()
                            };
                        }
                    }

                }
                _ => todo!(),
            }
        }

        Ok(())
    }
}
