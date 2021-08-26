use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::ast::{Expression, Statement};
use crate::error_reporter::{CodeLocation, ErrorReporter};
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

    fn current(&self) -> Option<&Token> {
        if self.index == 0 {
            None
        } else {
            Some(&self.tokens[self.index - 1])
        }
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
    error_reporter: Rc<dyn ErrorReporter>,
    had_errors: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, error_reporter: Rc<dyn ErrorReporter>) -> Parser {
        Self {
            tokens: TokenIterator::new(tokens),
            error_reporter,
            had_errors: false,
        }
    }

    fn error(&mut self, message: &'static str) {
        self.error_reporter.report(
            message,
            self.tokens
                .peek()
                .map(|t| CodeLocation {
                    line: t.line,
                    column: t.column,
                })
                .as_ref(),
        );
        self.had_errors = true;
    }

    fn synchronize(&mut self) {
        loop {
            if self.tokens.is_at_end()
                || self.tokens.current().map_or(false, |t| {
                    t.token_type == TokenType::Semicolon || t.token_type == TokenType::LeftBrace
                })
            {
                break;
            }

            if self.tokens.peek().map_or(false, Token::is_keyword) {
                break;
            }

            if self
                .tokens
                .peek()
                .map_or(false, |t| t.token_type == TokenType::RightBrace)
            {
                break;
            }

            self.tokens.next();
        }
    }

    fn error_sync(&mut self, message: &'static str) {
        self.error(message);
        self.synchronize();
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();
        while !self.tokens.is_at_end() {
            statements.push(self.statement());
        }
        statements
    }

    pub fn had_errors(&self) -> bool {
        self.had_errors
    }

    pub fn statement(&mut self) -> Statement {
        match self.tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Let) => self.definition_statement(),
            Some(TokenType::If) => self.conditional_statement(),
            Some(TokenType::While) => self.loop_statement(),
            Some(TokenType::LeftParen) => self.block(),
            Some(TokenType::Func) => self.function_definition(),
            Some(TokenType::Return) => self.return_statement(),
            Some(TokenType::Struct) => self.struct_definition(),
            Some(TokenType::Break) => self.break_statement(),
            Some(TokenType::Continue) => self.continue_statement(),
            Some(TokenType::Export) => self.export_statement(),
            Some(TokenType::Import) => self.import_statement(),
            Some(_) => self.expression_statement(),
            None => {
                self.error("Expected a statement");
                Statement::Invalid
            }
        }
    }

    pub fn expression_statement(&mut self) -> Statement {
        if let Some(expression) = self.expression(true).ok() {
            if self.tokens.consume(vec![TokenType::Semicolon]).is_none() {
                self.error("Expected ';' at the end of expression statement")
            }
            Statement::Expression { expr: expression }
        } else {
            self.synchronize();
            Statement::Invalid
        }
    }

    pub fn definition_statement(&mut self) -> Statement {
        if self.tokens.consume(vec![TokenType::Let]).is_none() {
            self.error_sync("Expected 'let' keyword");
            return Statement::Invalid;
        }

        let variable = if let Some(token) = self
            .tokens
            .consume(vec![TokenType::Identifier(String::new(), Vec::new())])
        {
            token
        } else {
            self.error_sync("Expected identifier after 'let' keyword");
            return Statement::Invalid;
        };

        if self.tokens.consume(vec![TokenType::Assign]).is_none() {
            self.error_sync("Expected ':=' after variable name in definition");
            return Statement::Invalid;
        }

        let value = if let Some(expr) = self.expression(true).ok() {
            expr
        } else {
            return Statement::Invalid;
        };

        if self.tokens.consume(vec![TokenType::Semicolon]).is_none() {
            self.error_sync("Expected ';' after variable definition");
            return Statement::Invalid;
        }

        Statement::Definition { variable, value }
    }

    pub fn export_statement(&mut self) -> Statement {
        self.tokens.consume(vec![TokenType::Export]).unwrap();

        match self.tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Identifier(_, args)) => {
                if args.len() > 0 {
                    self.error("export statement identifier may not contain backtick arguments");
                }

                if let Statement::Definition { variable, value } = self.definition_statement() {
                    Statement::ExportVariable {
                        name: variable,
                        value,
                    }
                } else {
                    Statement::Invalid
                }
            }
            Some(TokenType::Func) => match self.function_definition() {
                Statement::FunctionDefinition {
                    name,
                    parameters,
                    body,
                    export: _,
                } => Statement::FunctionDefinition {
                    name,
                    parameters,
                    body,
                    export: true,
                },
                _ => Statement::Invalid,
            },
            Some(TokenType::Struct) => match self.struct_definition() {
                Statement::StructDefinition {
                    struct_type,
                    fields,
                    export: _,
                } => Statement::StructDefinition {
                    struct_type,
                    fields,
                    export: true,
                },
                _ => Statement::Invalid,
            },
            _ => {
                self.error_sync("Expected function definition, struct definition or an identifier after 'export' keyword");
                Statement::Invalid
            }
        }
    }

    pub fn import_statement(&mut self) -> Statement {
        self.tokens.consume(vec![TokenType::Import]).unwrap();

        let name = if let Some(identifier) = self
            .tokens
            .consume(vec![TokenType::Identifier(String::new(), Vec::new())])
        {
            identifier
        } else {
            self.error("Expected identifier after 'import' keyword");
            return Statement::Invalid;
        };

        if self.tokens.consume(vec![TokenType::Semicolon]).is_none() {
            self.error_sync("Expected ';' at the end of an import statement");
            Statement::Invalid
        } else {
            Statement::Import { name }
        }
    }

    pub fn conditional_statement(&mut self) -> Statement {
        if self.tokens.consume(vec![TokenType::If]).is_none() {
            self.error_sync("Expected 'if' keyword");
            return Statement::Invalid;
        }

        let condition = if let Some(expr) = self.expression(false).ok() {
            expr
        } else {
            self.synchronize();
            return Statement::Invalid;
        };

        let if_true = Box::new(self.block());

        let if_false = if self
            .tokens
            .peek()
            .map_or(false, |t| t.token_type == TokenType::Else)
        {
            self.tokens.next();
            Some(Box::new(match self.tokens.peek().map(|t| &t.token_type) {
                Some(TokenType::LeftBrace) => self.block(),
                Some(TokenType::If) => self.conditional_statement(),
                _ => {
                    self.error_sync("Expected a block or 'if' statement after 'else'");
                    return Statement::Invalid;
                }
            }))
        } else {
            None
        };

        Statement::Conditional {
            condition,
            if_true,
            if_false,
        }
    }

    pub fn loop_statement(&mut self) -> Statement {
        if self.tokens.consume(vec![TokenType::While]).is_none() {
            unreachable!();
        }

        let condition = if let Some(expr) = self.expression(false).ok() {
            expr
        } else {
            self.synchronize();
            return Statement::Invalid;
        };

        let body = Box::new(self.block());
        Statement::Loop { condition, body }
    }

    pub fn block(&mut self) -> Statement {
        if self.tokens.consume(vec![TokenType::LeftBrace]).is_none() {
            self.error_sync("Expected '{' at the start of the block");
            return Statement::Invalid;
        }

        let mut statements = Vec::new();
        while !self
            .tokens
            .peek()
            .map(|t| t.token_type == TokenType::RightBrace)
            .unwrap_or(true)
        {
            statements.push(self.statement());
        }

        if self.tokens.consume(vec![TokenType::RightBrace]).is_none() {
            self.error_sync("Expected '}' at the end of the block");
            return Statement::Invalid;
        }

        Statement::Block { statements }
    }

    pub fn function_definition(&mut self) -> Statement {
        if self.tokens.consume(vec![TokenType::Func]).is_none() {
            unreachable!();
        }

        if let Some(Token {
            token_type: TokenType::Identifier(name, parameters),
            ..
        }) = self.tokens.next()
        {
            let body = Box::new(self.block());
            Statement::FunctionDefinition {
                name,
                parameters,
                body,
                export: false,
            }
        } else {
            self.error_sync("Expected an function signature after `func` keyword");
            Statement::Invalid
        }
    }

    pub fn return_statement(&mut self) -> Statement {
        self.tokens.consume(vec![TokenType::Return]).unwrap();

        if let Some(..) = self.tokens.consume(vec![TokenType::Semicolon]) {
            Statement::Return { expression: None }
        } else if let Some(expression) = self.expression(true).ok() {
            if self.tokens.consume(vec![TokenType::Semicolon]).is_none() {
                self.error_sync("Expected ';' at the end of return statement");
                Statement::Invalid
            } else {
                Statement::Return {
                    expression: Some(expression),
                }
            }
        } else {
            self.synchronize();
            Statement::Invalid
        }
    }

    pub fn struct_definition(&mut self) -> Statement {
        self.tokens.consume(vec![TokenType::Struct]).unwrap();

        if let Some(Token {
            token_type: TokenType::Identifier(struct_type, fields),
            ..
        }) = self.tokens.next()
        {
            if self.tokens.consume(vec![TokenType::Semicolon]).is_none() {
                self.error_sync("Expected ';' at the end of return statement");
                Statement::Invalid
            } else {
                Statement::StructDefinition {
                    struct_type,
                    fields,
                    export: false,
                }
            }
        } else {
            self.error("Expected a struct definition after `struct` keyword");
            Statement::Invalid
        }
    }

    pub fn break_statement(&mut self) -> Statement {
        self.tokens.consume(vec![TokenType::Break]).unwrap();

        if self.tokens.consume(vec![TokenType::Semicolon]).is_none() {
            self.error("Expected ';' after 'break'");
            Statement::Invalid
        } else {
            Statement::Break
        }
    }

    pub fn continue_statement(&mut self) -> Statement {
        self.tokens.consume(vec![TokenType::Continue]).unwrap();

        if self.tokens.consume(vec![TokenType::Semicolon]).is_none() {
            self.error("Expected ';' after 'continue'");
            Statement::Invalid
        } else {
            Statement::Continue
        }
    }

    fn expression(&mut self, allow_bare_structs: bool) -> Result<Expression, ()> {
        self.assignment(allow_bare_structs)
    }

    fn assignment(&mut self, allow_bare_structs: bool) -> Result<Expression, ()> {
        let lvalue = self.logic_or(allow_bare_structs)?;

        if let Some(..) = self.tokens.consume(vec![TokenType::Assign]) {
            let rvalue = self.logic_or(allow_bare_structs)?;
            Ok(Expression::Assignment {
                target: Box::new(lvalue),
                value: Box::new(rvalue),
            })
        } else {
            Ok(lvalue)
        }
    }

    fn logic_or(&mut self, allow_bare_structs: bool) -> Result<Expression, ()> {
        let mut expr = self.logic_and(allow_bare_structs)?;

        while let Some(operator) = self.tokens.consume(vec![TokenType::Or]) {
            let right = self.logic_and(allow_bare_structs)?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                right: Box::new(right),
                operator,
            };
        }

        Ok(expr)
    }

    fn logic_and(&mut self, allow_bare_structs: bool) -> Result<Expression, ()> {
        let mut expr = self.equality(allow_bare_structs)?;

        while let Some(operator) = self.tokens.consume(vec![TokenType::And]) {
            let right = self.equality(allow_bare_structs)?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                right: Box::new(right),
                operator,
            };
        }

        Ok(expr)
    }

    fn equality(&mut self, allow_bare_structs: bool) -> Result<Expression, ()> {
        let mut expr = self.comparison(allow_bare_structs)?;

        while let Some(operator) = self
            .tokens
            .consume(vec![TokenType::NotEqual, TokenType::Equal])
        {
            let right = self.comparison(allow_bare_structs)?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                right: Box::new(right),
                operator,
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self, allow_bare_structs: bool) -> Result<Expression, ()> {
        let mut expr = self.term(allow_bare_structs)?;

        while let Some(operator) = self.tokens.consume(vec![
            TokenType::LessThan,
            TokenType::LessThanOrEqual,
            TokenType::GreaterThan,
            TokenType::GreaterThanOrEqual,
        ]) {
            let right = self.term(allow_bare_structs)?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                right: Box::new(right),
                operator,
            };
        }

        Ok(expr)
    }

    fn term(&mut self, allow_bare_structs: bool) -> Result<Expression, ()> {
        let mut expr = self.factor(allow_bare_structs)?;

        while let Some(operator) = self.tokens.consume(vec![TokenType::Plus, TokenType::Minus]) {
            let right = self.factor(allow_bare_structs)?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                right: Box::new(right),
                operator,
            };
        }

        Ok(expr)
    }

    fn factor(&mut self, allow_bare_structs: bool) -> Result<Expression, ()> {
        let mut expr = self.unary(allow_bare_structs)?;

        while let Some(operator) =
            self.tokens
                .consume(vec![TokenType::Star, TokenType::Slash, TokenType::Modulo])
        {
            let right = self.unary(allow_bare_structs)?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                right: Box::new(right),
                operator,
            };
        }

        Ok(expr)
    }

    fn unary(&mut self, allow_bare_structs: bool) -> Result<Expression, ()> {
        if let Some(operator) = self.tokens.consume(vec![TokenType::Minus, TokenType::Not]) {
            Ok(Expression::UnaryOp {
                right: Box::new(self.postfix(allow_bare_structs)?),
                operator,
            })
        } else {
            self.postfix(allow_bare_structs)
        }
    }

    fn postfix(&mut self, allow_bare_structs: bool) -> Result<Expression, ()> {
        let mut expression = if allow_bare_structs {
            self.struct_literal()?
        } else {
            self.primary()?
        };

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
                        arguments.push(self.expression(true)?);
                        if let None = self.tokens.consume(vec![TokenType::Comma]) {
                            break;
                        }
                    }
                    if self.tokens.consume(vec![TokenType::RightParen]).is_none() {
                        self.error_sync("Expected ')' at the end of the parameter list");
                        return Err(());
                    }
                    expression = Expression::FunctionCall {
                        callee: Box::new(expression),
                        arguments,
                    };
                }
                TokenType::LeftSquare => {
                    self.tokens.next();
                    let index = self.expression(true)?;
                    if self.tokens.consume(vec![TokenType::RightSquare]).is_none() {
                        self.error_sync("Expected ']' at the end of the array access");
                        return Err(());
                    }
                    expression = Expression::ArrayRef {
                        array: Box::new(expression),
                        index: Box::new(index),
                    };
                }
                TokenType::Dot => {
                    self.tokens.next();
                    let field_name = if let Some(Token {
                        token_type: TokenType::Identifier(name, parameters),
                        ..
                    }) = self.tokens.next()
                    {
                        if parameters.len() > 0 {
                            self.error_sync("Struct field names cannot be parameterized");
                            return Err(());
                        }
                        name
                    } else {
                        self.error_sync("Expected a field name after '.'");
                        return Err(());
                    };

                    expression = Expression::StructFieldRef {
                        target: Box::new(expression),
                        field_name,
                    }
                }
                _ => break,
            }
        }

        Ok(expression)
    }

    fn struct_literal(&mut self) -> Result<Expression, ()> {
        let expr = self.primary()?;

        if let Some(..) = self.tokens.consume(vec![TokenType::LeftBrace]) {
            let struct_type = if let Expression::Variable {
                name:
                    Token {
                        token_type: TokenType::Identifier(name, _parameters),
                        ..
                    },
            } = expr
            {
                name
            } else {
                self.error_sync("Expected a type name before '{'");
                return Err(());
            };

            let mut fields = HashMap::new();
            while self
                .tokens
                .peek()
                .map_or(false, |tok| tok.token_type != TokenType::RightBrace)
            {
                let field_name = if let Some(Token {
                    token_type: TokenType::Identifier(name, parameters),
                    ..
                }) = self.tokens.next()
                {
                    if parameters.len() > 0 {
                        self.error_sync("Struct field names cannot be parameterized");
                        return Err(());
                    }
                    name
                } else {
                    self.error_sync("Expected a field name in a struct literal");
                    return Err(());
                };

                if self.tokens.consume(vec![TokenType::Assign]).is_none() {
                    self.error_sync("Expected ':=' after a field identifier");
                    return Err(());
                }

                let field_value = self.expression(true)?;

                fields.insert(field_name, field_value);

                self.tokens.consume(vec![TokenType::Comma]);
            }

            if self.tokens.consume(vec![TokenType::RightBrace]).is_none() {
                self.error_sync("Expected '}' at the end of a struct literal");
                return Err(());
            }

            Ok(Expression::StructLiteral {
                struct_type,
                fields,
            })
        } else {
            Ok(expr)
        }
    }

    fn primary(&mut self) -> Result<Expression, ()> {
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
            let expr = self.expression(true)?;
            if let Some(..) = self.tokens.consume(vec![TokenType::RightParen]) {
                Ok(expr)
            } else {
                self.error_sync("Expected ')' after expression");
                Err(())
            }
        } else if let Some(_) = self.tokens.consume(vec![TokenType::LeftSquare]) {
            let mut elements = Vec::new();
            while self
                .tokens
                .peek()
                .map_or(false, |tok| tok.token_type != TokenType::RightSquare)
            {
                elements.push(self.expression(true)?);
                if let None = self.tokens.consume(vec![TokenType::Comma]) {
                    break;
                }
            }
            self.tokens
                .consume(vec![TokenType::RightSquare])
                .ok_or(self.error_sync("Expected ']' at the end of the array literal"))?;
            Ok(Expression::ArrayLiteral { elements })
        } else {
            self.error_sync("Expected a primary value");
            Err(())
        }
    }
}
