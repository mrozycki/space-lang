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
            Value::Number(n) if *n > 0 => true,
            Value::String(s) if s.len() > 0 => true,
            _ => false,
        }
    }

    // Operator impls:

    fn get_numbers_binop(&self, rhs: Value) -> Result<(i64, i64), String> {
        if let Value::Number(a) = self {
            if let Value::Number(b) = rhs {
                Ok((*a, b))
            } else {
                Err("RHS Value in binary operator must be a number".to_string())
            }
        } else {
            Err("LHS Value in binary operator must be a number".to_string())
        }
    }

    pub fn equal(&self, rhs: Value) -> Result<Value, String> {
        Ok(Value::Number((*self == rhs).into()))
    }

    pub fn not_equal(&self, rhs: Value) -> Result<Value, String> {
        Ok(Value::Number((*self != rhs).into()))
    }

    pub fn less_than(&self, rhs: Value) -> Result<Value, String> {
        let (a, b) = self.get_numbers_binop(rhs)?;
        Ok(Value::Number((a < b).into()))
    }

    pub fn less_than_eq(&self, rhs: Value) -> Result<Value, String> {
        let (a, b) = self.get_numbers_binop(rhs)?;
        Ok(Value::Number((a <= b).into()))
    }

    pub fn greater_than(&self, rhs: Value) -> Result<Value, String> {
        let (a, b) = self.get_numbers_binop(rhs)?;
        Ok(Value::Number((a > b).into()))
    }

    pub fn greater_than_eq(&self, rhs: Value) -> Result<Value, String> {
        let (a, b) = self.get_numbers_binop(rhs)?;
        Ok(Value::Number((a >= b).into()))
    }

    pub fn add(&self, rhs: Value) -> Result<Value, String> {
        let (a, b) = self.get_numbers_binop(rhs)?;
        Ok(Value::Number((a + b).into()))
    }

    pub fn subtract(&self, rhs: Value) -> Result<Value, String> {
        let (a, b) = self.get_numbers_binop(rhs)?;
        Ok(Value::Number((a - b).into()))
    }

    pub fn multiply(&self, rhs: Value) -> Result<Value, String> {
        let (a, b) = self.get_numbers_binop(rhs)?;
        Ok(Value::Number((a * b).into()))
    }

    pub fn divide(&self, rhs: Value) -> Result<Value, String> {
        let (a, b) = self.get_numbers_binop(rhs)?;
        Ok(Value::Number((a / b).into()))
    }

    pub fn modulo(&self, rhs: Value) -> Result<Value, String> {
        let (a, b) = self.get_numbers_binop(rhs)?;
        Ok(Value::Number((a % b).into()))
    }

    pub fn and(&self, rhs: Value) -> Result<Value, String> {
        let (a, b) = self.get_numbers_binop(rhs)?;
        Ok(Value::Number((a & b).into()))
    }

    pub fn or(&self, rhs: Value) -> Result<Value, String> {
        let (a, b) = self.get_numbers_binop(rhs)?;
        Ok(Value::Number((a | b).into()))
    }
}

#[derive(Debug, Default, Clone)]
pub struct Scope {
    variables: HashMap<String, Value>,
    functions: HashMap<String, Statement>,
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
    pub fn get_var(&self, ident: &String) -> Result<Option<&Value>, String> {
        let matches = self
            .variables
            .keys()
            .filter(|k| k.starts_with(ident))
            .collect::<Vec<&String>>();

        if matches.len() == 0 {
            Ok(None)
        } else if matches.len() > 1 {
            Err(format!(
                "Use of the identifier `{}` is ambiguous, it could refer to more than one variable",
                ident
            ))
        } else {
            Ok(Some(self.variables.get(matches[0]).unwrap()))
        }
    }

    /// Sets the value of a variable
    /// Returns an error string if the variable is not defined
    pub fn set_var(&mut self, ident: &String, val: Value) -> Result<(), String> {
        let vars = self.variables.clone();
        let matches = vars
            .keys()
            .filter(|k| k.starts_with(ident))
            .collect::<Vec<&String>>();

        if matches.len() == 0 {
            Err(format!(
                "attempt to assign to an undefined variable `{}`",
                ident
            ))
        } else if matches.len() > 1 {
            Err(format!(
                "Use of the identifier `{}` is ambiguous, it could refer to more than one variable",
                ident
            ))
        } else {
            self.variables.insert(matches[0].to_string(), val);
            Ok(())
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
    modified_variables: HashSet<String>,
}

impl<'a> Interpreter<'a> {
    pub fn new(mut scope: Scope, ast: &'a [Statement]) -> Self {
        scope.add_functions_from_ast(ast);

        Self {
            scope,
            body: ast,
            line: 0,
            column: 0,
            modified_variables: HashSet::new(),
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

                if let Some(v) = self.scope.get_var(&ident).map_err(|e| self.error(e))? {
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

    fn eval_binaryop(
        &mut self,
        operator: &Token,
        left: &Box<Expression>,
        right: &Box<Expression>,
    ) -> Result<Value, InterpreterError> {
        let left_val = self.eval(left)?;
        let right_val = self.eval(right)?;

        match self.get_token_type(operator) {
            TokenType::Plus => left_val.add(right_val),
            TokenType::Minus => left_val.subtract(right_val),
            TokenType::Star => left_val.multiply(right_val),
            TokenType::Slash => left_val.divide(right_val),
            TokenType::Modulo => left_val.modulo(right_val),
            TokenType::And => left_val.and(right_val),
            TokenType::Or => left_val.or(right_val),
            TokenType::Equal => left_val.equal(right_val),
            TokenType::NotEqual => left_val.not_equal(right_val),
            TokenType::LessThan => left_val.less_than(right_val),
            TokenType::LessThanOrEqual => left_val.less_than_eq(right_val),
            TokenType::GreaterThan => left_val.greater_than(right_val),
            TokenType::GreaterThanOrEqual => left_val.greater_than_eq(right_val),
            _ => unreachable!(),
        }
        .map_err(|e| self.error(e))
    }

    fn eval(&mut self, expr: &Expression) -> Result<Value, InterpreterError> {
        match expr {
            Expression::Literal { value } => self.eval_literal_token(value),
            Expression::Variable { name } => self.eval_variable_token(name),
            Expression::Assignment { variable, value } => self.eval_assignment(variable, value),
            Expression::BinaryOp {
                operator,
                left,
                right,
            } => self.eval_binaryop(operator, left, right),
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
                        loop_interpreter.line = self.line;
                        loop_interpreter.column = self.column;

                        loop_interpreter.run()?;
                        self.modified_variables
                            .extend(loop_interpreter.modified_variables.clone());

                        for key in &loop_interpreter.modified_variables {
                            if !self
                                .scope
                                .get_var(key)
                                .map_err(|e| self.error(e))?
                                .is_some()
                            {
                                continue;
                            }

                            match loop_interpreter
                                .scope
                                .get_var(key)
                                .map_err(|e| self.error(e))?
                            {
                                Some(v) => {
                                    loop_scope
                                        .set_var(key, v.clone())
                                        .map_err(|e| self.error(e))?;
                                    self.scope
                                        .set_var(key, v.clone())
                                        .map_err(|e| self.error(e))?;
                                }
                                None => unreachable!(),
                            };
                        }
                    }
                }

                Statement::Conditional {
                    condition,
                    if_true,
                    if_false,
                } => {
                    let condition_val = self.eval(condition)?;

                    let body = if condition_val.is_truthy() {
                        if_true
                    } else if let Some(b) = if_false {
                        b
                    } else {
                        continue;
                    };

                    let body_ast = match body.as_ref() {
                        Statement::Block { statements } => statements,
                        _ => unreachable!(),
                    };

                    let mut if_scope = self.scope.clone();
                    if_scope.add_functions_from_ast(&body_ast);
                    let mut if_interpreter = Interpreter::new(if_scope, &body_ast);
                    if_interpreter.line = self.line;
                    if_interpreter.column = self.column;

                    if_interpreter.run()?;

                    for key in &if_interpreter.modified_variables {
                        if !self
                            .scope
                            .get_var(key)
                            .map_err(|e| self.error(e))?
                            .is_some()
                        {
                            continue;
                        }

                        match if_interpreter
                            .scope
                            .get_var(key)
                            .map_err(|e| self.error(e))?
                        {
                            Some(v) => self
                                .scope
                                .set_var(key, v.clone())
                                .map_err(|e| self.error(e))?,
                            None => unreachable!(),
                        };
                    }

                    self.modified_variables
                        .extend(if_interpreter.modified_variables);
                }

                _ => todo!(),
            }
        }

        Ok(())
    }
}
