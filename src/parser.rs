use std::fmt;

use crate::ast::Expression;
use crate::lexer::{Token, TokenType};

struct TokenIterator {
    tokens: Vec<Token>,
    index: usize,
}

impl TokenIterator {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        if self.index < self.tokens.len() {
            Some(&self.tokens[self.index])
        } else {
            None
        }
    }

    fn previous(&self) -> Option<&Token> {
        if self.index > 0 {
            Some(&self.tokens[self.index - 1])
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

    pub fn parse(mut self) -> Result<Expression, ParserError> {
        self.expression()
    }

    fn error(&self, message: &'static str) -> ParserError {
        ParserError {
            message,
            location: self.tokens.peek().map(|t| (t.line, t.column)),
        }
    }

    fn expression(&mut self) -> Result<Expression, ParserError> {
        self.equality()
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
