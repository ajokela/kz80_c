//! C Lexer - tokenizes source code

use crate::token::Token;

pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
    line: usize,
    col: usize,
}

#[derive(Debug)]
pub struct LexError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    fn peek(&self) -> Option<u8> {
        self.input.get(self.pos).copied()
    }

    fn peek_next(&self) -> Option<u8> {
        self.input.get(self.pos + 1).copied()
    }

    fn advance(&mut self) -> Option<u8> {
        let ch = self.peek()?;
        self.pos += 1;
        if ch == b'\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_ascii_whitespace() {
                self.advance();
            } else if ch == b'/' && self.peek_next() == Some(b'/') {
                // Line comment
                while let Some(c) = self.peek() {
                    if c == b'\n' {
                        break;
                    }
                    self.advance();
                }
            } else if ch == b'/' && self.peek_next() == Some(b'*') {
                // Block comment
                self.advance(); // /
                self.advance(); // *
                while let Some(c) = self.peek() {
                    if c == b'*' && self.peek_next() == Some(b'/') {
                        self.advance(); // *
                        self.advance(); // /
                        break;
                    }
                    self.advance();
                }
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self) -> Result<Token, LexError> {
        let start = self.pos;

        // Check for hex
        if self.peek() == Some(b'0') && matches!(self.peek_next(), Some(b'x') | Some(b'X')) {
            self.advance(); // 0
            self.advance(); // x
            let mut value: i32 = 0;
            while let Some(ch) = self.peek() {
                if ch.is_ascii_hexdigit() {
                    let digit = if ch.is_ascii_digit() {
                        ch - b'0'
                    } else {
                        (ch.to_ascii_lowercase() - b'a') + 10
                    };
                    value = value * 16 + digit as i32;
                    self.advance();
                } else {
                    break;
                }
            }
            return Ok(Token::IntLit(value));
        }

        // Read integer part
        let mut value: i32 = 0;
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                value = value * 10 + (ch - b'0') as i32;
                self.advance();
            } else {
                break;
            }
        }

        // Check for decimal point (not followed by another dot, which would be range syntax)
        let is_float = self.peek() == Some(b'.') && self.peek_next().map_or(false, |c| c.is_ascii_digit());

        if is_float {
            // This is a float literal
            self.advance(); // consume '.'

            // Read fractional part
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }

            // Check for exponent
            if matches!(self.peek(), Some(b'e') | Some(b'E')) {
                self.advance(); // consume 'e'
                // Optional sign
                if matches!(self.peek(), Some(b'+') | Some(b'-')) {
                    self.advance();
                }
                // Exponent digits
                while let Some(ch) = self.peek() {
                    if ch.is_ascii_digit() {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }

            let float_str = String::from_utf8_lossy(&self.input[start..self.pos]).to_string();
            return Ok(Token::FloatLit(float_str));
        }

        // Check for exponent without decimal point (e.g., 1e5)
        if matches!(self.peek(), Some(b'e') | Some(b'E')) {
            self.advance(); // consume 'e'
            // Optional sign
            if matches!(self.peek(), Some(b'+') | Some(b'-')) {
                self.advance();
            }
            // Exponent digits
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
            let float_str = String::from_utf8_lossy(&self.input[start..self.pos]).to_string();
            return Ok(Token::FloatLit(float_str));
        }

        Ok(Token::IntLit(value))
    }

    fn read_ident(&mut self) -> Token {
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == b'_' {
                self.advance();
            } else {
                break;
            }
        }
        let name = String::from_utf8_lossy(&self.input[start..self.pos]).to_string();

        // Check for keywords
        match name.as_str() {
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "do" => Token::Do,
            "return" => Token::Return,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "int" => Token::Int,
            "char" => Token::Char,
            "void" => Token::Void,
            "float" => Token::Float,
            "struct" => Token::Struct,
            "sizeof" => Token::Sizeof,
            _ => Token::Ident(name),
        }
    }

    fn read_char(&mut self) -> Result<Token, LexError> {
        self.advance(); // opening '
        let ch = match self.peek() {
            Some(b'\\') => {
                self.advance();
                match self.peek() {
                    Some(b'n') => { self.advance(); b'\n' }
                    Some(b'r') => { self.advance(); b'\r' }
                    Some(b't') => { self.advance(); b'\t' }
                    Some(b'0') => { self.advance(); 0 }
                    Some(b'\\') => { self.advance(); b'\\' }
                    Some(b'\'') => { self.advance(); b'\'' }
                    Some(c) => { self.advance(); c }
                    None => return Err(self.error("Unexpected end of input in char literal")),
                }
            }
            Some(c) => { self.advance(); c }
            None => return Err(self.error("Unexpected end of input in char literal")),
        };
        if self.peek() != Some(b'\'') {
            return Err(self.error("Expected closing quote in char literal"));
        }
        self.advance(); // closing '
        Ok(Token::CharLit(ch))
    }

    fn read_string(&mut self) -> Result<Token, LexError> {
        self.advance(); // opening "
        let mut s = Vec::new();
        loop {
            match self.peek() {
                Some(b'"') => {
                    self.advance();
                    break;
                }
                Some(b'\\') => {
                    self.advance();
                    let ch = match self.peek() {
                        Some(b'n') => { self.advance(); b'\n' }
                        Some(b'r') => { self.advance(); b'\r' }
                        Some(b't') => { self.advance(); b'\t' }
                        Some(b'0') => { self.advance(); 0 }
                        Some(b'\\') => { self.advance(); b'\\' }
                        Some(b'"') => { self.advance(); b'"' }
                        Some(c) => { self.advance(); c }
                        None => return Err(self.error("Unexpected end of input in string")),
                    };
                    s.push(ch);
                }
                Some(c) => {
                    self.advance();
                    s.push(c);
                }
                None => return Err(self.error("Unexpected end of input in string")),
            }
        }
        Ok(Token::StringLit(String::from_utf8_lossy(&s).to_string()))
    }

    fn error(&self, message: &str) -> LexError {
        LexError {
            message: message.to_string(),
            line: self.line,
            col: self.col,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();

        let ch = match self.peek() {
            Some(c) => c,
            None => return Ok(Token::Eof),
        };

        // Numbers
        if ch.is_ascii_digit() {
            return self.read_number();
        }

        // Identifiers and keywords
        if ch.is_ascii_alphabetic() || ch == b'_' {
            return Ok(self.read_ident());
        }

        // Character literal
        if ch == b'\'' {
            return self.read_char();
        }

        // String literal
        if ch == b'"' {
            return self.read_string();
        }

        // Operators and delimiters
        self.advance();
        match ch {
            b'+' => {
                if self.peek() == Some(b'+') { self.advance(); Ok(Token::PlusPlus) }
                else if self.peek() == Some(b'=') { self.advance(); Ok(Token::PlusEq) }
                else { Ok(Token::Plus) }
            }
            b'-' => {
                if self.peek() == Some(b'-') { self.advance(); Ok(Token::MinusMinus) }
                else if self.peek() == Some(b'=') { self.advance(); Ok(Token::MinusEq) }
                else if self.peek() == Some(b'>') { self.advance(); Ok(Token::Arrow) }
                else { Ok(Token::Minus) }
            }
            b'*' => {
                if self.peek() == Some(b'=') { self.advance(); Ok(Token::StarEq) }
                else { Ok(Token::Star) }
            }
            b'/' => {
                if self.peek() == Some(b'=') { self.advance(); Ok(Token::SlashEq) }
                else { Ok(Token::Slash) }
            }
            b'%' => Ok(Token::Percent),
            b'&' => {
                if self.peek() == Some(b'&') { self.advance(); Ok(Token::AmpAmp) }
                else if self.peek() == Some(b'=') { self.advance(); Ok(Token::AmpEq) }
                else { Ok(Token::Amp) }
            }
            b'|' => {
                if self.peek() == Some(b'|') { self.advance(); Ok(Token::PipePipe) }
                else if self.peek() == Some(b'=') { self.advance(); Ok(Token::PipeEq) }
                else { Ok(Token::Pipe) }
            }
            b'^' => {
                if self.peek() == Some(b'=') { self.advance(); Ok(Token::CaretEq) }
                else { Ok(Token::Caret) }
            }
            b'~' => Ok(Token::Tilde),
            b'!' => {
                if self.peek() == Some(b'=') { self.advance(); Ok(Token::BangEq) }
                else { Ok(Token::Bang) }
            }
            b'<' => {
                if self.peek() == Some(b'<') { self.advance(); Ok(Token::LtLt) }
                else if self.peek() == Some(b'=') { self.advance(); Ok(Token::LtEq) }
                else { Ok(Token::Lt) }
            }
            b'>' => {
                if self.peek() == Some(b'>') { self.advance(); Ok(Token::GtGt) }
                else if self.peek() == Some(b'=') { self.advance(); Ok(Token::GtEq) }
                else { Ok(Token::Gt) }
            }
            b'=' => {
                if self.peek() == Some(b'=') { self.advance(); Ok(Token::EqEq) }
                else { Ok(Token::Eq) }
            }
            b'(' => Ok(Token::LParen),
            b')' => Ok(Token::RParen),
            b'{' => Ok(Token::LBrace),
            b'}' => Ok(Token::RBrace),
            b'[' => Ok(Token::LBracket),
            b']' => Ok(Token::RBracket),
            b';' => Ok(Token::Semicolon),
            b',' => Ok(Token::Comma),
            b'.' => Ok(Token::Dot),
            b':' => Ok(Token::Colon),
            b'?' => Ok(Token::Question),
            _ => Err(self.error(&format!("Unexpected character: {}", ch as char))),
        }
    }

    /// Tokenize entire input
    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token()?;
            if tok == Token::Eof {
                tokens.push(tok);
                break;
            }
            tokens.push(tok);
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple() {
        let mut lexer = Lexer::new("int x = 42;");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![
            Token::Int,
            Token::Ident("x".to_string()),
            Token::Eq,
            Token::IntLit(42),
            Token::Semicolon,
            Token::Eof,
        ]);
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("++ -- << >> && || == !=");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![
            Token::PlusPlus,
            Token::MinusMinus,
            Token::LtLt,
            Token::GtGt,
            Token::AmpAmp,
            Token::PipePipe,
            Token::EqEq,
            Token::BangEq,
            Token::Eof,
        ]);
    }

    #[test]
    fn test_hex() {
        let mut lexer = Lexer::new("0x1F 0xFF00");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![
            Token::IntLit(0x1F),
            Token::IntLit(0xFF00),
            Token::Eof,
        ]);
    }

    #[test]
    fn test_float_literals() {
        let mut lexer = Lexer::new("3.14 1.0 0.5 1e5 2.5e-3");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![
            Token::FloatLit("3.14".to_string()),
            Token::FloatLit("1.0".to_string()),
            Token::FloatLit("0.5".to_string()),
            Token::FloatLit("1e5".to_string()),
            Token::FloatLit("2.5e-3".to_string()),
            Token::Eof,
        ]);
    }

    #[test]
    fn test_float_keyword() {
        let mut lexer = Lexer::new("float x = 3.14;");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![
            Token::Float,
            Token::Ident("x".to_string()),
            Token::Eq,
            Token::FloatLit("3.14".to_string()),
            Token::Semicolon,
            Token::Eof,
        ]);
    }
}
