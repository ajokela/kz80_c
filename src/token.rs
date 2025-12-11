//! Token types for the C lexer

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    IntLit(i32),
    CharLit(u8),
    StringLit(String),
    Ident(String),

    // Keywords
    If,
    Else,
    While,
    For,
    Do,
    Return,
    Break,
    Continue,
    Int,
    Char,
    Void,
    Struct,
    Sizeof,

    // Operators
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    Amp,        // &
    Pipe,       // |
    Caret,      // ^
    Tilde,      // ~
    Bang,       // !
    Lt,         // <
    Gt,         // >
    Eq,         // =

    // Compound operators
    PlusPlus,   // ++
    MinusMinus, // --
    LtLt,       // <<
    GtGt,       // >>
    AmpAmp,     // &&
    PipePipe,   // ||
    EqEq,       // ==
    BangEq,     // !=
    LtEq,       // <=
    GtEq,       // >=
    PlusEq,     // +=
    MinusEq,    // -=
    StarEq,     // *=
    SlashEq,    // /=
    AmpEq,      // &=
    PipeEq,     // |=
    CaretEq,    // ^=
    Arrow,      // ->

    // Delimiters
    LParen,     // (
    RParen,     // )
    LBrace,     // {
    RBrace,     // }
    LBracket,   // [
    RBracket,   // ]
    Semicolon,  // ;
    Comma,      // ,
    Dot,        // .
    Colon,      // :
    Question,   // ?

    // Special
    Eof,
}

impl Token {
    /// Check if this token is a type specifier
    pub fn is_type(&self) -> bool {
        matches!(self, Token::Int | Token::Char | Token::Void | Token::Struct)
    }
}
