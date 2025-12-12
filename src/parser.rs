//! C Parser - recursive descent parser

use crate::token::Token;
use crate::ast::*;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
}

impl ParseError {
    fn new(msg: &str) -> Self {
        Self { message: msg.to_string() }
    }
}

type Result<T> = std::result::Result<T, ParseError>;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::Eof)
    }

    fn peek_next(&self) -> &Token {
        self.tokens.get(self.pos + 1).unwrap_or(&Token::Eof)
    }

    fn advance(&mut self) -> Token {
        let tok = self.peek().clone();
        self.pos += 1;
        tok
    }

    fn expect(&mut self, expected: Token) -> Result<()> {
        if self.peek() == &expected {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::new(&format!("Expected {:?}, got {:?}", expected, self.peek())))
        }
    }

    fn check(&self, tok: &Token) -> bool {
        self.peek() == tok
    }

    /// Parse a complete translation unit
    pub fn parse(&mut self) -> Result<TranslationUnit> {
        let mut decls = Vec::new();
        while !self.check(&Token::Eof) {
            decls.push(self.parse_decl()?);
        }
        Ok(TranslationUnit { decls })
    }

    /// Parse a top-level declaration
    fn parse_decl(&mut self) -> Result<Decl> {
        // Check for struct definition
        if self.check(&Token::Struct) && matches!(self.peek_next(), Token::Ident(_)) {
            // Could be struct definition or variable/function with struct type
            // Look ahead to see if there's a { for definition
            let pos = self.pos;
            self.advance(); // struct
            let name = self.parse_ident()?;
            if self.check(&Token::LBrace) {
                return self.parse_struct_def(name);
            } else {
                // It's a variable or function declaration with struct type
                self.pos = pos;
            }
        }

        // Parse type and name
        let ty = self.parse_type()?;
        let name = self.parse_ident()?;

        // Function or variable?
        if self.check(&Token::LParen) {
            self.parse_function(ty, name)
        } else {
            self.parse_global(ty, name)
        }
    }

    fn parse_struct_def(&mut self, name: String) -> Result<Decl> {
        self.expect(Token::LBrace)?;
        let mut fields = Vec::new();
        while !self.check(&Token::RBrace) {
            let ty = self.parse_type()?;
            let field_name = self.parse_ident()?;
            self.expect(Token::Semicolon)?;
            fields.push(Field { ty, name: field_name });
        }
        self.expect(Token::RBrace)?;
        self.expect(Token::Semicolon)?;
        Ok(Decl::Struct(StructDef { name, fields }))
    }

    fn parse_function(&mut self, return_type: Type, name: String) -> Result<Decl> {
        self.expect(Token::LParen)?;
        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                let ty = self.parse_type()?;
                // Parameter name is optional for prototypes
                let pname = if self.check(&Token::Comma) || self.check(&Token::RParen) {
                    String::new()
                } else {
                    self.parse_ident()?
                };
                params.push(Param { ty, name: pname });
                if !self.check(&Token::Comma) {
                    break;
                }
                self.advance();
            }
        }
        self.expect(Token::RParen)?;

        // Check for prototype (ends with semicolon) vs definition (has body)
        if self.check(&Token::Semicolon) {
            self.advance();
            // For prototypes, we still create a function but with empty body
            // The codegen will handle forward references
            return Ok(Decl::Function(Function {
                return_type,
                name,
                params,
                body: Vec::new(),
            }));
        }

        // Parse body
        let body = self.parse_block()?;

        Ok(Decl::Function(Function {
            return_type,
            name,
            params,
            body,
        }))
    }

    fn parse_global(&mut self, ty: Type, name: String) -> Result<Decl> {
        // Check for array declaration: type name[size]
        let ty = if self.check(&Token::LBracket) {
            self.advance();
            let size = match self.advance() {
                Token::IntLit(n) => n as usize,
                _ => return Err(ParseError::new("Expected array size")),
            };
            self.expect(Token::RBracket)?;
            Type::Array(Box::new(ty), size)
        } else {
            ty
        };

        let init = if self.check(&Token::Eq) {
            self.advance();
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.expect(Token::Semicolon)?;
        Ok(Decl::Global(Global { ty, name, init }))
    }

    fn parse_type(&mut self) -> Result<Type> {
        let base = match self.peek() {
            Token::Void => { self.advance(); Type::Void }
            Token::Char => { self.advance(); Type::Char }
            Token::Int => { self.advance(); Type::Int }
            Token::Float => { self.advance(); Type::Float }
            Token::Struct => {
                self.advance();
                let name = self.parse_ident()?;
                Type::Struct(name)
            }
            _ => return Err(ParseError::new(&format!("Expected type, got {:?}", self.peek()))),
        };

        // Handle pointers
        let mut ty = base;
        while self.check(&Token::Star) {
            self.advance();
            ty = Type::Pointer(Box::new(ty));
        }

        Ok(ty)
    }

    fn parse_ident(&mut self) -> Result<String> {
        match self.advance() {
            Token::Ident(name) => Ok(name),
            tok => Err(ParseError::new(&format!("Expected identifier, got {:?}", tok))),
        }
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>> {
        self.expect(Token::LBrace)?;
        let mut stmts = Vec::new();
        while !self.check(&Token::RBrace) {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(Token::RBrace)?;
        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        match self.peek() {
            // Block
            Token::LBrace => {
                Ok(Stmt::Block(self.parse_block()?))
            }
            // If statement
            Token::If => {
                self.advance();
                self.expect(Token::LParen)?;
                let cond = self.parse_expr()?;
                self.expect(Token::RParen)?;
                let then_branch = Box::new(self.parse_stmt()?);
                let else_branch = if self.check(&Token::Else) {
                    self.advance();
                    Some(Box::new(self.parse_stmt()?))
                } else {
                    None
                };
                Ok(Stmt::If(cond, then_branch, else_branch))
            }
            // While loop
            Token::While => {
                self.advance();
                self.expect(Token::LParen)?;
                let cond = self.parse_expr()?;
                self.expect(Token::RParen)?;
                let body = Box::new(self.parse_stmt()?);
                Ok(Stmt::While(cond, body))
            }
            // For loop
            Token::For => {
                self.advance();
                self.expect(Token::LParen)?;

                // Init
                let init = if self.check(&Token::Semicolon) {
                    self.advance();
                    None
                } else if self.peek().is_type() {
                    let stmt = self.parse_var_decl()?;
                    Some(Box::new(stmt))
                } else {
                    let expr = self.parse_expr()?;
                    self.expect(Token::Semicolon)?;
                    Some(Box::new(Stmt::Expr(expr)))
                };

                // Condition
                let cond = if self.check(&Token::Semicolon) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                self.expect(Token::Semicolon)?;

                // Update
                let update = if self.check(&Token::RParen) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                self.expect(Token::RParen)?;

                let body = Box::new(self.parse_stmt()?);
                Ok(Stmt::For(init, cond, update, body))
            }
            // Do-while
            Token::Do => {
                self.advance();
                let body = Box::new(self.parse_stmt()?);
                self.expect(Token::While)?;
                self.expect(Token::LParen)?;
                let cond = self.parse_expr()?;
                self.expect(Token::RParen)?;
                self.expect(Token::Semicolon)?;
                Ok(Stmt::DoWhile(body, cond))
            }
            // Return
            Token::Return => {
                self.advance();
                let expr = if self.check(&Token::Semicolon) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Return(expr))
            }
            // Break
            Token::Break => {
                self.advance();
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Break)
            }
            // Continue
            Token::Continue => {
                self.advance();
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Continue)
            }
            // Variable declaration or expression statement
            _ if self.peek().is_type() => {
                self.parse_var_decl()
            }
            // Expression statement
            _ => {
                let expr = self.parse_expr()?;
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_var_decl(&mut self) -> Result<Stmt> {
        let mut ty = self.parse_type()?;
        let name = self.parse_ident()?;

        // Check for array declaration: type name[size]
        if self.check(&Token::LBracket) {
            self.advance();
            let size = match self.advance() {
                Token::IntLit(n) => n as usize,
                _ => return Err(ParseError::new("Expected array size")),
            };
            self.expect(Token::RBracket)?;
            ty = Type::Array(Box::new(ty), size);
        }

        let init = if self.check(&Token::Eq) {
            self.advance();
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.expect(Token::Semicolon)?;
        Ok(Stmt::VarDecl(ty, name, init))
    }

    // Expression parsing with precedence climbing
    fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr> {
        let mut left = self.parse_ternary()?;

        let op = match self.peek() {
            Token::Eq => Some(BinOp::Assign),
            Token::PlusEq => Some(BinOp::AddAssign),
            Token::MinusEq => Some(BinOp::SubAssign),
            Token::StarEq => Some(BinOp::MulAssign),
            Token::SlashEq => Some(BinOp::DivAssign),
            Token::AmpEq => Some(BinOp::AndAssign),
            Token::PipeEq => Some(BinOp::OrAssign),
            Token::CaretEq => Some(BinOp::XorAssign),
            _ => None,
        };

        if let Some(op) = op {
            self.advance();
            let right = self.parse_assignment()?;
            left = Expr::Binary(op, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_ternary(&mut self) -> Result<Expr> {
        let cond = self.parse_log_or()?;
        if self.check(&Token::Question) {
            self.advance();
            let then_expr = self.parse_expr()?;
            self.expect(Token::Colon)?;
            let else_expr = self.parse_ternary()?;
            Ok(Expr::Ternary(Box::new(cond), Box::new(then_expr), Box::new(else_expr)))
        } else {
            Ok(cond)
        }
    }

    fn parse_log_or(&mut self) -> Result<Expr> {
        let mut left = self.parse_log_and()?;
        while self.check(&Token::PipePipe) {
            self.advance();
            let right = self.parse_log_and()?;
            left = Expr::Binary(BinOp::LogOr, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_log_and(&mut self) -> Result<Expr> {
        let mut left = self.parse_bit_or()?;
        while self.check(&Token::AmpAmp) {
            self.advance();
            let right = self.parse_bit_or()?;
            left = Expr::Binary(BinOp::LogAnd, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_bit_or(&mut self) -> Result<Expr> {
        let mut left = self.parse_bit_xor()?;
        while self.check(&Token::Pipe) {
            self.advance();
            let right = self.parse_bit_xor()?;
            left = Expr::Binary(BinOp::BitOr, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_bit_xor(&mut self) -> Result<Expr> {
        let mut left = self.parse_bit_and()?;
        while self.check(&Token::Caret) {
            self.advance();
            let right = self.parse_bit_and()?;
            left = Expr::Binary(BinOp::BitXor, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_bit_and(&mut self) -> Result<Expr> {
        let mut left = self.parse_equality()?;
        while self.check(&Token::Amp) {
            self.advance();
            let right = self.parse_equality()?;
            left = Expr::Binary(BinOp::BitAnd, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<Expr> {
        let mut left = self.parse_relational()?;
        loop {
            let op = match self.peek() {
                Token::EqEq => BinOp::Eq,
                Token::BangEq => BinOp::Ne,
                _ => break,
            };
            self.advance();
            let right = self.parse_relational()?;
            left = Expr::Binary(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_relational(&mut self) -> Result<Expr> {
        let mut left = self.parse_shift()?;
        loop {
            let op = match self.peek() {
                Token::Lt => BinOp::Lt,
                Token::Gt => BinOp::Gt,
                Token::LtEq => BinOp::Le,
                Token::GtEq => BinOp::Ge,
                _ => break,
            };
            self.advance();
            let right = self.parse_shift()?;
            left = Expr::Binary(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_shift(&mut self) -> Result<Expr> {
        let mut left = self.parse_additive()?;
        loop {
            let op = match self.peek() {
                Token::LtLt => BinOp::Shl,
                Token::GtGt => BinOp::Shr,
                _ => break,
            };
            self.advance();
            let right = self.parse_additive()?;
            left = Expr::Binary(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Expr> {
        let mut left = self.parse_multiplicative()?;
        loop {
            let op = match self.peek() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            left = Expr::Binary(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr> {
        let mut left = self.parse_unary()?;
        loop {
            let op = match self.peek() {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_unary()?;
            left = Expr::Binary(op, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr> {
        match self.peek() {
            Token::Minus => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Unary(UnOp::Neg, Box::new(expr)))
            }
            Token::Bang => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Unary(UnOp::Not, Box::new(expr)))
            }
            Token::Tilde => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Unary(UnOp::BitNot, Box::new(expr)))
            }
            Token::Star => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Unary(UnOp::Deref, Box::new(expr)))
            }
            Token::Amp => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Unary(UnOp::AddrOf, Box::new(expr)))
            }
            Token::PlusPlus => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Unary(UnOp::PreInc, Box::new(expr)))
            }
            Token::MinusMinus => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Unary(UnOp::PreDec, Box::new(expr)))
            }
            Token::Sizeof => {
                self.advance();
                self.expect(Token::LParen)?;
                let ty = self.parse_type()?;
                self.expect(Token::RParen)?;
                Ok(Expr::Sizeof(ty))
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Expr> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.peek() {
                Token::LBracket => {
                    self.advance();
                    let index = self.parse_expr()?;
                    self.expect(Token::RBracket)?;
                    expr = Expr::Index(Box::new(expr), Box::new(index));
                }
                Token::LParen => {
                    // Function call
                    if let Expr::Var(name) = expr {
                        self.advance();
                        let mut args = Vec::new();
                        if !self.check(&Token::RParen) {
                            loop {
                                args.push(self.parse_expr()?);
                                if !self.check(&Token::Comma) {
                                    break;
                                }
                                self.advance();
                            }
                        }
                        self.expect(Token::RParen)?;
                        expr = Expr::Call(name, args);
                    } else {
                        break;
                    }
                }
                Token::Dot => {
                    self.advance();
                    let member = self.parse_ident()?;
                    expr = Expr::Member(Box::new(expr), member);
                }
                Token::Arrow => {
                    self.advance();
                    let member = self.parse_ident()?;
                    expr = Expr::Arrow(Box::new(expr), member);
                }
                Token::PlusPlus => {
                    self.advance();
                    expr = Expr::Unary(UnOp::PostInc, Box::new(expr));
                }
                Token::MinusMinus => {
                    self.advance();
                    expr = Expr::Unary(UnOp::PostDec, Box::new(expr));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr> {
        match self.peek().clone() {
            Token::IntLit(n) => {
                self.advance();
                Ok(Expr::IntLit(n))
            }
            Token::FloatLit(s) => {
                self.advance();
                Ok(Expr::FloatLit(s))
            }
            Token::CharLit(c) => {
                self.advance();
                Ok(Expr::CharLit(c))
            }
            Token::StringLit(s) => {
                self.advance();
                Ok(Expr::StringLit(s))
            }
            Token::Ident(name) => {
                self.advance();
                Ok(Expr::Var(name))
            }
            Token::LParen => {
                self.advance();
                // Check for cast
                if self.peek().is_type() {
                    let ty = self.parse_type()?;
                    self.expect(Token::RParen)?;
                    let expr = self.parse_unary()?;
                    Ok(Expr::Cast(ty, Box::new(expr)))
                } else {
                    let expr = self.parse_expr()?;
                    self.expect(Token::RParen)?;
                    Ok(expr)
                }
            }
            _ => Err(ParseError::new(&format!("Unexpected token: {:?}", self.peek()))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse(code: &str) -> Result<TranslationUnit> {
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().map_err(|e| ParseError::new(&e.message))?;
        let mut parser = Parser::new(tokens);
        parser.parse()
    }

    #[test]
    fn test_simple_function() {
        let code = "int main() { return 0; }";
        let ast = parse(code).unwrap();
        assert_eq!(ast.decls.len(), 1);
    }

    #[test]
    fn test_factorial() {
        let code = r#"
            int factorial(int n) {
                if (n <= 1) return 1;
                return n * factorial(n - 1);
            }
        "#;
        let ast = parse(code).unwrap();
        assert_eq!(ast.decls.len(), 1);
    }

    #[test]
    fn test_float_decl() {
        let code = r#"
            float pi = 3.14;
            float compute(float x) {
                float y = x + 1.0;
                return y;
            }
        "#;
        let ast = parse(code).unwrap();
        assert_eq!(ast.decls.len(), 2);
    }
}
