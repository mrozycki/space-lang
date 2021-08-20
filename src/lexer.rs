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

#[derive(Debug, Clone, PartialEq)]
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

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(code: &str) -> Result<Vec<Token>, LexerError> {
        let mut lexer = Lexer::new(code);
        let mut tokens = Vec::new();
        loop {
            match lexer.next()? {
                Token::Eof => {
                    tokens.push(Token::Eof);
                    break;
                }
                token => {
                    tokens.push(token);
                }
            }
        }
        Ok(tokens)
    }

    #[test]
    fn a_whole_program_can_be_one_identifier() {
        let program = "why did the chicken cross the road? to get to the 0ther side!";

        assert_eq!(
            lex(program),
            Ok(vec![
                Token::Identifier(program.to_owned(), Vec::new()),
                Token::Eof
            ])
        );
    }

    #[test]
    fn naked_identifier_includes_whitespace_after_it() {
        let program = "Hello world! := 5;";

        assert_eq!(
            lex(program),
            Ok(vec![
                Token::Identifier("Hello world! ".to_owned(), Vec::new()),
                Token::Assign,
                Token::Number("5".to_owned()),
                Token::Semicolon,
                Token::Eof
            ])
        );
    }

    #[test]
    fn marked_identifier_can_contain_any_characters() {
        let program = "`Hello, world! := 5 - 2 < 3` := 5;";

        assert_eq!(
            lex(program),
            Ok(vec![
                Token::Identifier("Hello, world! := 5 - 2 < 3".to_owned(), Vec::new()),
                Token::Assign,
                Token::Number("5".to_owned()),
                Token::Semicolon,
                Token::Eof
            ])
        );
    }

    #[test]
    fn keyword_at_the_beginning_of_naked_identifier_is_a_keyword() {
        let program = "return hello world!;";

        assert_eq!(
            lex(program),
            Ok(vec![
                Token::Return,
                Token::Identifier("hello world!".to_owned(), Vec::new()),
                Token::Semicolon,
                Token::Eof
            ])
        );
    }

    #[test]
    fn keyword_within_identifier_is_part_of_identifier() {
        let program = "hello return world!;";

        assert_eq!(
            lex(program),
            Ok(vec![
                Token::Identifier("hello return world!".to_owned(), Vec::new()),
                Token::Semicolon,
                Token::Eof
            ])
        );
    }

    #[test]
    fn keyword_at_the_beginning_of_marked_identifier_is_part_of_identifier() {
        let program = "`return hello world!`;";

        assert_eq!(
            lex(program),
            Ok(vec![
                Token::Identifier("return hello world!".to_owned(), Vec::new()),
                Token::Semicolon,
                Token::Eof
            ])
        );
    }

    #[test]
    fn naked_identifier_can_contain_embedded_marked_identifiers_as_parameters() {
        let program = "func contains substring checks if `haystack` contains `needle` { }";

        assert_eq!(
            lex(program),
            Ok(vec![
                Token::Func,
                Token::Identifier(
                    "contains substring checks if `haystack` contains `needle` ".to_owned(),
                    vec!["haystack".to_owned(), "needle".to_owned()]
                ),
                Token::LeftBrace,
                Token::RightBrace,
                Token::Eof
            ])
        );
    }

    #[test]
    fn operators_break_up_naked_identifiers() {
        let program = "let Hello, world! := 42;";

        assert_eq!(
            lex(program),
            Ok(vec![
                Token::Let,
                Token::Identifier("Hello".to_owned(), Vec::new()),
                Token::Comma,
                Token::Identifier("world! ".to_owned(), Vec::new()),
                Token::Assign,
                Token::Number("42".to_owned()),
                Token::Semicolon,
                Token::Eof
            ])
        );
    }

    #[test]
    fn operators_do_not_break_up_marked_identifiers() {
        let program = "let `Hello, world!` := 42;";

        assert_eq!(
            lex(program),
            Ok(vec![
                Token::Let,
                Token::Identifier("Hello, world!".to_owned(), Vec::new()),
                Token::Assign,
                Token::Number("42".to_owned()),
                Token::Semicolon,
                Token::Eof
            ])
        );
    }

    #[test]
    fn malformed_operators_return_errors() {
        let program = "let x : 2;";

        assert_eq!(
            lex(program),
            Err(LexerError {
                message: "Expected =".to_owned(),
                line: 1,
                column: 7
            })
        );
    }

    #[test]
    fn lexer_reports_location_of_error() {
        let program = "let x := 2;\nlet y := 3;\n\nlet z := 1 | 2;\n\nlet w := 7;";

        assert_eq!(
            lex(program),
            Err(LexerError {
                message: "Expected |".to_owned(),
                line: 4,
                column: 12
            })
        );
    }
}
