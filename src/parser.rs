use crate::ast::Expression;
use crate::lexer::Token;

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

    fn is_same_token_type(a: &Token, b: &Token) -> bool {
        match (a, b) {
            (Token::Identifier(..), Token::Identifier(..)) => true,
            (Token::Number(..), Token::Number(..)) => true,
            (Token::String(..), Token::String(..)) => true,
            (a, b) => a == b,
        }
    }

    fn consume(&mut self, options: Vec<Token>) -> Option<Token> {
        for option in &options {
            if Self::is_same_token_type(option, self.peek().unwrap_or(&Token::Eof)) {
                return self.next();
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct ParserError {}

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

    fn expression(&mut self) -> Result<Expression, ParserError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.comparison()?;

        while let Some(operator) = self.tokens.consume(vec![Token::NotEqual, Token::Equal]) {
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
            Token::LessThan,
            Token::LessThanOrEqual,
            Token::GreaterThan,
            Token::GreaterThanOrEqual,
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

        while let Some(operator) = self.tokens.consume(vec![Token::Plus, Token::Minus]) {
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

        while let Some(operator) = self.tokens.consume(vec![Token::Star, Token::Slash]) {
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
        if let Some(operator) = self.tokens.consume(vec![Token::Minus, Token::Not]) {
            Ok(Expression::UnaryOp {
                right: Box::new(self.primary()?),
                operator,
            })
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expression, ParserError> {
        if let Some(string) = self.tokens.consume(vec![Token::String(String::new())]) {
            Ok(Expression::Literal { value: string })
        } else if let Some(number) = self.tokens.consume(vec![Token::Number(String::new())]) {
            Ok(Expression::Literal { value: number })
        } else if let Some(identifier) = self
            .tokens
            .consume(vec![Token::Identifier(String::new(), Vec::new())])
        {
            Ok(Expression::Variable { name: identifier })
        } else if let Some(..) = self.tokens.consume(vec![Token::LeftParen]) {
            let expr = self.expression()?;
            if let Some(..) = self.tokens.consume(vec![Token::RightParen]) {
                Ok(expr)
            } else {
                Err(ParserError {})
            }
        } else {
            Err(ParserError {})
        }
    }
}
