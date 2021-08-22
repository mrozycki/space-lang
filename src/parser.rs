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
            (TokenType::Integer(..), TokenType::Integer(..)) => true,
            (TokenType::Float(..), TokenType::Float(..)) => true,
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
            Some(TokenType::Let) => self.definition_statement(),
            Some(TokenType::If) => self.conditional_statement(),
            Some(TokenType::While) => self.loop_statement(),
            Some(TokenType::LeftParen) => self.block(),
            Some(TokenType::Func) => self.function_definition(),
            Some(TokenType::Return) => self.return_statement(),
            Some(TokenType::Break) => self.break_statement(),
            Some(TokenType::Continue) => self.continue_statement(),
            Some(TokenType::Export) => self.export_statement(),
            Some(TokenType::Import) => self.import_statement(),
            Some(_) => self.expression_statement(),
            None => Err(self.error("Expected a statement")),
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

    pub fn export_statement(&mut self) -> Result<Statement, ParserError> {
        self.tokens
            .consume(vec![TokenType::Export])
            .ok_or(self.error("Expected 'export' keyword"))?;

        match self
            .tokens
            .consume(vec![TokenType::Identifier(String::new(), Vec::new())])
        {
            Some(name) => {
                self.tokens
                    .consume(vec![TokenType::Assign])
                    .ok_or(self.error("Expected ':=' after name in export"))?;
                let value = self.expression()?;

                self.tokens
                    .consume(vec![TokenType::Semicolon])
                    .ok_or(self.error("Expected ';' at the end of an export statement"))?;

                Ok(Statement::ExportVariable { name, value })
            }
            None => {
                let func = self.function_definition().or(Err(
                    self.error("Expected function or identifier after 'export' keyword")
                ))?;
                Ok(Statement::ExportFunction {
                    definition: Box::new(func),
                })
            }
        }
    }

    pub fn import_statement(&mut self) -> Result<Statement, ParserError> {
        self.tokens
            .consume(vec![TokenType::Import])
            .ok_or(self.error("Expected 'import' keyword"))?;

        let name = self
            .tokens
            .consume(vec![TokenType::Identifier(String::new(), Vec::new())])
            .ok_or(self.error("Expected identifier after 'import' keyword"))?;

        self.tokens
            .consume(vec![TokenType::Semicolon])
            .ok_or(self.error("Expected ';' at the end of an import statement"))?;

        Ok(Statement::Import { name })
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

    pub fn loop_statement(&mut self) -> Result<Statement, ParserError> {
        self.tokens
            .consume(vec![TokenType::While])
            .ok_or(self.error("Expected 'while' keyword"))?;

        let condition = self.expression()?;
        let body = Box::new(self.block()?);
        Ok(Statement::Loop { condition, body })
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

    pub fn function_definition(&mut self) -> Result<Statement, ParserError> {
        self.tokens
            .consume(vec![TokenType::Func])
            .ok_or(self.error("Expected 'func' keyword at the start of function definition"))?;

        if let Some(Token {
            token_type: TokenType::Identifier(name, parameters),
            ..
        }) = self.tokens.next()
        {
            let body = Box::new(self.block()?);
            Ok(Statement::FunctionDefinition {
                name,
                parameters,
                body,
            })
        } else {
            Err(self.error("Expected an function signature after `func` keyword"))
        }
    }

    pub fn return_statement(&mut self) -> Result<Statement, ParserError> {
        self.tokens.consume(vec![TokenType::Return]).unwrap();

        if let Some(..) = self.tokens.consume(vec![TokenType::Semicolon]) {
            Ok(Statement::Return { expression: None })
        } else {
            let expression = self.expression()?;

            self.tokens
                .consume(vec![TokenType::Semicolon])
                .ok_or(self.error("Expected ';' at the end of return statement"))?;

            Ok(Statement::Return {
                expression: Some(expression),
            })
        }
    }

    pub fn break_statement(&mut self) -> Result<Statement, ParserError> {
        self.tokens.consume(vec![TokenType::Break]).unwrap();

        self.tokens
            .consume(vec![TokenType::Semicolon])
            .ok_or(self.error("Expected ';' after 'break'"))?;

        Ok(Statement::Break)
    }

    pub fn continue_statement(&mut self) -> Result<Statement, ParserError> {
        self.tokens.consume(vec![TokenType::Continue]).unwrap();

        self.tokens
            .consume(vec![TokenType::Semicolon])
            .ok_or(self.error("Expected ';' after 'continue'"))?;

        Ok(Statement::Continue)
    }

    fn expression(&mut self) -> Result<Expression, ParserError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression, ParserError> {
        let lvalue = self.logic_or()?;

        if let Some(..) = self.tokens.consume(vec![TokenType::Assign]) {
            let rvalue = self.logic_or()?;
            Ok(Expression::Assignment {
                target: Box::new(lvalue),
                value: Box::new(rvalue),
            })
        } else {
            Ok(lvalue)
        }
    }

    fn logic_or(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.logic_and()?;

        while let Some(operator) = self.tokens.consume(vec![TokenType::Or]) {
            let right = self.logic_and()?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                right: Box::new(right),
                operator,
            };
        }

        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.equality()?;

        while let Some(operator) = self.tokens.consume(vec![TokenType::And]) {
            let right = self.equality()?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                right: Box::new(right),
                operator,
            };
        }

        Ok(expr)
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

        while let Some(operator) =
            self.tokens
                .consume(vec![TokenType::Star, TokenType::Slash, TokenType::Modulo])
        {
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
                right: Box::new(self.postfix()?),
                operator,
            })
        } else {
            self.postfix()
        }
    }

    fn postfix(&mut self) -> Result<Expression, ParserError> {
        let mut expression = self.primary()?;

        while let Some(tok) = self.tokens.peek() {
            match tok.token_type {
                TokenType::LeftParen => {
                    self.tokens.next();
                    let mut arguments = Vec::new();
                    while self
                        .tokens
                        .peek()
                        .map_or(false, |tok| tok.token_type != TokenType::RightParen)
                    {
                        arguments.push(self.expression()?);
                        if let None = self.tokens.consume(vec![TokenType::Comma]) {
                            break;
                        }
                    }
                    self.tokens
                        .consume(vec![TokenType::RightParen])
                        .ok_or(self.error("Expected ')' at the end of the parameter list"))?;
                    expression = Expression::FunctionCall {
                        callee: Box::new(expression),
                        arguments,
                    };
                }
                TokenType::LeftSquare => {
                    self.tokens.next();
                    let index = self.expression()?;
                    self.tokens
                        .consume(vec![TokenType::RightSquare])
                        .ok_or(self.error("Expected ']' at the end of the array access"))?;
                    expression = Expression::ArrayRef {
                        array: Box::new(expression),
                        index: Box::new(index),
                    };
                }
                _ => break,
            }
        }

        Ok(expression)
    }

    fn primary(&mut self) -> Result<Expression, ParserError> {
        if let Some(value) = self.tokens.consume(vec![
            TokenType::String(String::new()),
            TokenType::Integer(String::new()),
            TokenType::Float(String::new()),
        ]) {
            Ok(Expression::Literal { value })
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
        } else if let Some(_) = self.tokens.consume(vec![TokenType::LeftSquare]) {
            let mut elements = Vec::new();
            while self
                .tokens
                .peek()
                .map_or(false, |tok| tok.token_type != TokenType::RightSquare)
            {
                elements.push(self.expression()?);
                if let None = self.tokens.consume(vec![TokenType::Comma]) {
                    break;
                }
            }
            self.tokens
                .consume(vec![TokenType::RightSquare])
                .ok_or(self.error("Expected ']' at the end of the array literal"))?;
            Ok(Expression::ArrayLiteral { elements })
        } else {
            Err(self.error("Expected a primary value"))
        }
    }
}
