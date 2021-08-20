use core::fmt;
use std::{iter::Peekable, str::Chars};

#[derive(Debug, PartialEq)]
pub enum Token {
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,
    Assign,
    Semicolon,
    Comma,
    Number(String),
    Identifier(String, Vec<String>),
    Func,
    Let,
    Return,
    If,
    While,
    Eof,
    Plus,
    Minus,
    Star,
    Slash,
    And,
    Or,
    Not,
}

#[derive(Debug, Clone)]
pub struct LexerError {
    message: String,
    line: usize,
    column: usize,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} in line {}, col {}",
            self.message, self.line, self.column
        )
    }
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            chars: code.chars().peekable(),
            line: 1,
            column: 0,
        }
    }

    fn error(&self, message: String) -> LexerError {
        LexerError {
            message,
            line: self.line,
            column: self.column,
        }
    }

    fn peek(&mut self) -> char {
        *(self.chars.peek().unwrap_or(&char::default()))
    }

    fn advance(&mut self) -> char {
        let c = self.chars.next().unwrap_or_default();
        self.column += 1;

        if c == '\n' {
            self.line += 1;
            self.column = 0;
        }

        c
    }

    fn consume(&mut self, c: char) -> Result<(), LexerError> {
        if self.peek() == c {
            self.advance();
            Ok(())
        } else {
            Err(self.error(format!("Expected {}", c)))
        }
    }

    pub fn next(&mut self) -> Result<Token, LexerError> {
        while self.peek().is_whitespace() {
            self.advance();
        }

        if self.peek() == '{' {
            self.advance();
            Ok(Token::LeftBrace)
        } else if self.peek() == '}' {
            self.advance();
            Ok(Token::RightBrace)
        } else if self.peek() == '(' {
            self.advance();
            Ok(Token::LeftParen)
        } else if self.peek() == ')' {
            self.advance();
            Ok(Token::RightParen)
        } else if self.peek() == '[' {
            self.advance();
            Ok(Token::LeftSquare)
        } else if self.peek() == ']' {
            self.advance();
            Ok(Token::RightSquare)
        } else if self.peek() == ';' {
            self.advance();
            Ok(Token::Semicolon)
        } else if self.peek() == ':' {
            self.advance();
            self.consume('=')?;
            Ok(Token::Assign)
        } else if self.peek() == ',' {
            self.advance();
            Ok(Token::Comma)
        } else if self.peek() == '+' {
            self.advance();
            Ok(Token::Plus)
        } else if self.peek() == '-' {
            self.advance();
            Ok(Token::Minus)
        } else if self.peek() == '*' {
            self.advance();
            Ok(Token::Star)
        } else if self.peek() == '/' {
            self.advance();
            Ok(Token::Slash)
        } else if self.peek() == '&' {
            self.advance();
            self.consume('&')?;
            Ok(Token::And)
        } else if self.peek() == '|' {
            self.advance();
            self.consume('|')?;
            Ok(Token::Or)
        } else if self.peek() == '!' {
            self.advance();
            Ok(Token::Not)
        } else if self.peek().is_digit(10) {
            Ok(self.consume_number())
        } else if self.peek().is_alphabetic() {
            Ok(self.consume_naked_identifier())
        } else if self.peek() == '`' {
            Ok(self.consume_backtick_identifier())
        } else if self.peek() == char::default() {
            Ok(Token::Eof)
        } else {
            let peek = self.peek();
            Err(self.error(format!("Unexpected {}", peek)))
        }
    }

    fn consume_number(&mut self) -> Token {
        let mut value = String::new();
        while self.peek().is_digit(10) {
            value.push(self.advance());
        }
        Token::Number(value)
    }

    fn is_operator_char(c: char) -> bool {
        "{}()[]:;,+-*/&|".contains(c)
    }

    fn is_naked_identifier_char(c: char) -> bool {
        c != char::default() && !Self::is_operator_char(c)
    }

    fn keyword(value: &str) -> Option<Token> {
        match value.trim_end() {
            "return" => Some(Token::Return),
            "let" => Some(Token::Let),
            "func" => Some(Token::Func),
            "if" => Some(Token::If),
            "while" => Some(Token::While),
            _ => None,
        }
    }

    fn consume_naked_identifier(&mut self) -> Token {
        let mut value = String::new();
        let mut args = Vec::new();

        while Self::is_naked_identifier_char(self.peek()) {
            if self.peek().is_whitespace() {
                if let Some(keyword_token) = Self::keyword(&value) {
                    return keyword_token;
                }
            } else if self.peek() == '`' {
                let mut arg = String::new();

                value.push(self.advance());
                while self.peek() != '`' {
                    arg.push(self.advance());
                }

                value.push_str(&arg);
                args.push(arg);
            }

            value.push(self.advance())
        }

        if let Some(keyword_token) = Self::keyword(value.trim_end()) {
            keyword_token
        } else {
            Token::Identifier(value, args)
        }
    }

    fn consume_backtick_identifier(&mut self) -> Token {
        let mut value = String::new();
        self.advance();
        while self.peek() != '`' {
            value.push(self.advance())
        }
        self.advance();
        Token::Identifier(value, vec![])
    }
}
