use std::fmt;

use crate::ast::{Expression, Statement};
use crate::lexer::{Token, TokenType};

struct TokenIterator {
    tokens: Vec<Token>,
    index: usize,
}

impl TokenIterator {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn is_at_end(&self) -> bool {
        self.peek()
            .map(|t| t.token_type == TokenType::Eof)
            .unwrap_or(true)
    }

    fn peek(&self) -> Option<&Token> {
        if self.index < self.tokens.len() {
            Some(&self.tokens[self.index])
        } else {
            None
        }
    }

    fn next(&mut self) -> Option<Token> {
        if self.index >= self.tokens.len() {
            None
        } else {
            let result = self.tokens[self.index].clone();
            self.index += 1;
            Some(result)
        }
    }

    fn is_same_token_type(a: &TokenType, b: &TokenType) -> bool {
        match (a, b) {
            (TokenType::Identifier(..), TokenType::Identifier(..)) => true,
            (TokenType::Number(..), TokenType::Number(..)) => true,
            (TokenType::String(..), TokenType::String(..)) => true,
            (a, b) => a == b,
        }
    }

    fn consume(&mut self, options: Vec<TokenType>) -> Option<Token> {
        for option in &options {
            if Self::is_same_token_type(
                &option,
                self.peek()
                    .map(|t| &t.token_type)
                    .unwrap_or(&TokenType::Eof),
            ) {
                return self.next();
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct ParserError {
    message: &'static str,
    location: Option<(usize, usize)>,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some((line, column)) = self.location {
            write!(f, "{} in line {}, column {}", self.message, line, column)
        } else {
            write!(f, "{} at end", self.message)
        }
    }
}

pub struct Parser {
    tokens: TokenIterator,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: TokenIterator::new(tokens),
        }
    }

    fn error(&self, message: &'static str) -> ParserError {
        ParserError {
            message,
            location: self.tokens.peek().map(|t| (t.line, t.column)),
        }
    }

    pub fn parse(mut self) -> Result<Vec<Statement>, ParserError> {
        let mut statements = Vec::new();
        while !self.tokens.is_at_end() {
            statements.push(self.statement()?);
        }
        Ok(statements)
    }

    pub fn statement(&mut self) -> Result<Statement, ParserError> {
        match self.tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Identifier(name, _))
                if name.trim_end() == "print" || name.trim_end() == "println" =>
            {
                let newline = name.trim_end() == "println";
                self.tokens.next();
                self.print_statement(newline)
            }
            Some(TokenType::Let) => self.definition_statement(),
            Some(TokenType::If) => self.conditional_statement(),
            Some(_) => self.expression_statement(),
            None => Err(self.error("Expected a statement")),
        }
    }

    pub fn print_statement(&mut self, newline: bool) -> Result<Statement, ParserError> {
        let expression = self.expression()?;
        if let Some(..) = self.tokens.consume(vec![TokenType::Semicolon]) {
            Ok(Statement::Print {
                expr: expression,
                newline,
            })
        } else {
            Err(self.error("Expected ';' at the end of print statement"))
        }
    }

    pub fn expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression = self.expression()?;
        if let Some(..) = self.tokens.consume(vec![TokenType::Semicolon]) {
            Ok(Statement::Expression { expr: expression })
        } else {
            Err(self.error("Expected ';' at the end of expression statement"))
        }
    }

    pub fn definition_statement(&mut self) -> Result<Statement, ParserError> {
        self.tokens
            .consume(vec![TokenType::Let])
            .ok_or(self.error("Expected 'let' keyword"))?;

        let name = self
            .tokens
            .consume(vec![TokenType::Identifier(String::new(), Vec::new())])
            .ok_or(self.error("Expected identifier after 'let' keyword"))?;

        self.tokens
            .consume(vec![TokenType::Assign])
            .ok_or(self.error("Expected ':=' after variable name in definition"))?;

        let initializer = self.expression()?;

        self.tokens
            .consume(vec![TokenType::Semicolon])
            .ok_or(self.error("Expected ';' at the end of a variable definition"))?;

        Ok(Statement::Definition {
            variable: name,
            value: initializer,
        })
    }

    pub fn conditional_statement(&mut self) -> Result<Statement, ParserError> {
        self.tokens
            .consume(vec![TokenType::If])
            .ok_or(self.error("Expected 'if' keyword"))?;

        let condition = self.expression()?;
        let if_true = Box::new(self.block()?);

        let if_false = if self
            .tokens
            .peek()
            .map(|t| t.token_type == TokenType::Else)
            .unwrap_or(false)
        {
            self.tokens.next();
            Some(Box::new(match self.tokens.peek().map(|t| &t.token_type) {
                Some(TokenType::LeftBrace) => self.block(),
                Some(TokenType::If) => self.conditional_statement(),
                _ => Err(self.error("Expected a block or 'if' statement after 'else'")),
            }?))
        } else {
            None
        };

        Ok(Statement::Conditional {
            condition,
            if_true,
            if_false,
        })
    }

    pub fn block(&mut self) -> Result<Statement, ParserError> {
        self.tokens
            .consume(vec![TokenType::LeftBrace])
            .ok_or(self.error("Expected '{' at the start of the block"))?;

        let mut statements = Vec::new();
        while !self
            .tokens
            .peek()
            .map(|t| t.token_type == TokenType::RightBrace)
            .unwrap_or(true)
        {
            statements.push(self.statement()?);
        }

        self.tokens
            .consume(vec![TokenType::RightBrace])
            .ok_or(self.error("Expected '}' at the end of the block"))?;

        Ok(Statement::Block { statements })
    }

    fn expression(&mut self) -> Result<Expression, ParserError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression, ParserError> {
        let lvalue = self.equality()?;

        if let Some(..) = self.tokens.consume(vec![TokenType::Assign]) {
            if let Expression::Variable { name } = lvalue {
                let rvalue = self.equality()?;
                Ok(Expression::Assignment {
                    variable: name,
                    value: Box::new(rvalue),
                })
            } else {
                Err(self.error("Invalid lvalue: expected a variable name"))
            }
        } else {
            Ok(lvalue)
        }
    }

    fn equality(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.comparison()?;

        while let Some(operator) = self
            .tokens
            .consume(vec![TokenType::NotEqual, TokenType::Equal])
        {
            let right = self.comparison()?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                right: Box::new(right),
                operator,
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.term()?;

        while let Some(operator) = self.tokens.consume(vec![
            TokenType::LessThan,
            TokenType::LessThanOrEqual,
            TokenType::GreaterThan,
            TokenType::GreaterThanOrEqual,
        ]) {
            let right = self.term()?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                right: Box::new(right),
                operator,
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.factor()?;

        while let Some(operator) = self.tokens.consume(vec![TokenType::Plus, TokenType::Minus]) {
            let right = self.factor()?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                right: Box::new(right),
                operator,
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.unary()?;

        while let Some(operator) = self.tokens.consume(vec![TokenType::Star, TokenType::Slash]) {
            let right = self.unary()?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                right: Box::new(right),
                operator,
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, ParserError> {
        if let Some(operator) = self.tokens.consume(vec![TokenType::Minus, TokenType::Not]) {
            Ok(Expression::UnaryOp {
                right: Box::new(self.primary()?),
                operator,
            })
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expression, ParserError> {
        if let Some(string) = self.tokens.consume(vec![TokenType::String(String::new())]) {
            Ok(Expression::Literal { value: string })
        } else if let Some(number) = self.tokens.consume(vec![TokenType::Number(String::new())]) {
            Ok(Expression::Literal { value: number })
        } else if let Some(identifier) = self
            .tokens
            .consume(vec![TokenType::Identifier(String::new(), Vec::new())])
        {
            Ok(Expression::Variable { name: identifier })
        } else if let Some(..) = self.tokens.consume(vec![TokenType::LeftParen]) {
            let expr = self.expression()?;
            if let Some(..) = self.tokens.consume(vec![TokenType::RightParen]) {
                Ok(expr)
            } else {
                Err(self.error("Expected ')' after expression"))
            }
        } else {
            Err(self.error("Expected a primary value"))
        }
    }
}
