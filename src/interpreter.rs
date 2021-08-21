use std::cell::RefCell;
use crate::{
    ast::{Expression, Statement},
    lexer::{Token, TokenType},
};
use qp_trie::{Trie, wrapper::BString};

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

    pub fn not(&self) -> Result<Value, String> {
        if let Value::Number(a) = self {
            Ok(Value::Number(!a))
        } else {
            Err("operand for unary operator must be a number".to_string())
        }
    }
}

#[derive(Default, Debug)]
pub struct Scope<'s> {
    parent_scope: Option<&'s Scope<'s>>,
    variables: Trie<BString, RefCell<Value>>,
    functions: Trie<BString, Statement>,
}

impl Scope<'_> {
    /// Checks is a variable is defined in the current scope or any parent scopes
    pub fn is_var_defined(&self, ident: &str) -> bool {
        self.variables
            .iter_prefix_str(ident)
            .next()
            .is_some()
    }

    pub fn var(&self, ident: &str) -> Result<&RefCell<Value>, String> {
        let mut scope = self;
        loop {
            let mut matches = scope.variables.iter_prefix_str(ident);
            let var = match matches.next() {
                Some(v) => v,
                None => {
                    if let Some(parent) = scope.parent_scope {
                        scope = parent;
                        continue;
                    } else {
                        return Err(format!(
                            "no such variable: `{}`",
                            ident
                        ));
                    }
                },
            };

            if matches.next().is_some() {
                return Err(format!(
                    "Use of the identifier `{}` is ambiguous, it could refer to more than one variable",
                    ident
                ))
            } else {
                return Ok(var.1);
            }
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
                self.functions.insert_str(name, stmt.clone());
            };
        }
    }

    /// Create a new scope, with `self` as the parent.
    pub fn child_scope(&self) -> Scope<'_> {
        Scope {
            parent_scope: Some(self),
            variables: Trie::new(),
            functions: Trie::new(),
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
pub struct Interpreter<'b, 's> {
    scope: Scope<'s>,
    body: &'b [Statement],
    line: usize,
    column: usize,
}

impl<'b, 's> Interpreter<'b, 's> {
    pub fn new(mut scope: Scope<'s>, ast: &'b [Statement]) -> Self {
        scope.add_functions_from_ast(ast);

        Self {
            scope,
            body: ast,
            line: 0,
            column: 0,
        }
    }

    pub fn with_ast(ast: &'b [Statement]) -> Self {
        Self::new(Default::default(), ast)
    }

    fn get_token_type<'t>(&mut self, token: &'t Token) -> &'t TokenType {
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

                let v: RefCell<Value> = self.scope.var(&ident).map_err(|e| self.error(e))?.clone();
                Ok(v.into_inner())
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

                let val = self.eval(expr, false)?;
                *self.scope
                    .var(&ident)
                    .map_err(|e| self.error(e))?
                    .borrow_mut()
                    = val.clone();

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
        let left_val = self.eval(left, false)?;
        let right_val = self.eval(right, false)?;

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

    fn eval_unaryop(&mut self, operator: &Token, operand: &Box<Expression>) -> Result<Value, InterpreterError> {
        let operand_val = self.eval(operand, false)?;

        match self.get_token_type(operator) {
            TokenType::Not => operand_val.not(),
            TokenType::Minus => operand_val.multiply(Value::Number(-1)),
            _ => unreachable!()
        }.map_err(|e| self.error(e))
    }

    fn eval(&mut self, expr: &Expression, value_ignored: bool) -> Result<Value, InterpreterError> {
        match expr {
            Expression::Literal { value } => self.eval_literal_token(value),
            Expression::Variable { name } if !value_ignored  => self.eval_variable_token(name),
            Expression::Variable { name: _ } => Ok(Value::Number(0)),
            Expression::Assignment { variable, value } => self.eval_assignment(variable, value),
            Expression::BinaryOp {
                operator,
                left,
                right,
            } => self.eval_binaryop(operator, left, right),
            Expression::UnaryOp { operator, right } => self.eval_unaryop(operator, right),
            _ => todo!(),
        }
    }

    pub fn run(&mut self) -> Result<(), InterpreterError> {
        for stmt in self.body {
            match stmt {
                Statement::Expression { expr } => {
                    self.eval(&expr, true)?;
                }

                Statement::Print { expr, newline } => {
                    let mut str = self.eval(&expr, false)?.to_string();
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

                        let val = self.eval(&value, false)?;
                        self.scope.variables.insert_str(ident, RefCell::new(val));
                    }
                }

                Statement::Loop { condition, body } => {
                    let loop_ast = match body.as_ref() {
                        Statement::Block { statements } => statements,
                        _ => unreachable!(),
                    };

                    while self.eval(&condition, false)?.is_truthy() {
                        let mut loop_interpreter = Interpreter::new(self.scope.child_scope(), &loop_ast);
                        loop_interpreter.line = self.line;
                        loop_interpreter.column = self.column;

                        loop_interpreter.run()?;
                    }
                }

                Statement::Conditional {
                    condition,
                    if_true,
                    if_false,
                } => {
                    let condition_val = self.eval(condition, false)?;

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

                    let if_scope = self.scope.child_scope();
                    let mut if_interpreter = Interpreter::new(if_scope, &body_ast);
                    if_interpreter.line = self.line;
                    if_interpreter.column = self.column;

                    if_interpreter.run()?;
                }

                Statement::FunctionDefinition { name: _, parameters: _, body: _ } => (),
                _ => todo!(),
            }
        }

        Ok(())
    }
}
