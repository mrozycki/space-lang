use std::{iter::Peekable, str::Chars};

#[derive(Debug)]
pub enum Token {
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    Assign,
    Colon,
    Semicolon,
    Comma,
    Number(String),
    Identifier(String),
    Func,
    Let,
    Return,
    If,
    While,
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            chars: code.chars().peekable(),
        }
    }

    fn peek(&mut self) -> char {
        *(self.chars.peek().unwrap_or(&char::default()))
    }

    fn advance(&mut self) -> char {
        self.chars.next().unwrap_or_default()
    }

    pub fn next(&mut self) -> Option<Token> {
        while self.peek().is_whitespace() {
            self.advance();
        }

        if self.peek() == '{' {
            self.advance();
            Some(Token::LeftBrace)
        } else if self.peek() == '}' {
            self.advance();
            Some(Token::RightBrace)
        } else if self.peek() == '(' {
            self.advance();
            Some(Token::LeftParen)
        } else if self.peek() == ')' {
            self.advance();
            Some(Token::RightParen)
        } else if self.peek() == ';' {
            self.advance();
            Some(Token::Semicolon)
        } else if self.peek() == ':' {
            self.advance();
            if self.peek() == '=' {
                self.advance();
                Some(Token::Assign)
            } else {
                Some(Token::Colon)
            }
        } else if self.peek() == ',' {
            self.advance();
            Some(Token::Comma)
        } else if self.peek().is_digit(10) {
            Some(self.consume_number())
        } else if self.peek().is_alphabetic() {
            Some(self.consume_naked_identifier())
        } else if self.peek() == '`' {
            Some(self.consume_backtick_identifier())
        } else {
            None
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
        c == '{' || c == '}' || c == '(' || c == ')' || c == ':' || c == ';' || c == ','
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
        while Self::is_naked_identifier_char(self.peek()) {
            if self.peek().is_whitespace() {
                if let Some(keyword_token) = Self::keyword(&value) {
                    return keyword_token;
                }
            }
            value.push(self.advance())
        }

        if let Some(keyword_token) = Self::keyword(value.trim_end()) {
            keyword_token
        } else {
            Token::Identifier(value)
        }
    }

    fn consume_backtick_identifier(&mut self) -> Token {
        let mut value = String::new();
        self.advance();
        while self.peek() != '`' {
            value.push(self.advance())
        }
        self.advance();
        Token::Identifier(value)
    }
}
