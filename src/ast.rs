//! Abstract Syntax Tree types for C

/// A type in our C subset
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Char,                      // 8-bit
    Int,                       // 16-bit
    Float,                     // 6-byte BCD float
    Pointer(Box<Type>),        // pointer to type
    Array(Box<Type>, usize),   // array of type with size
    Struct(String),            // named struct
}

impl Type {
    /// Size of type in bytes
    pub fn size(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::Char => 1,
            Type::Int => 2,
            Type::Float => 6,       // 6-byte BCD float
            Type::Pointer(_) => 2,  // 16-bit pointers on Z80
            Type::Array(inner, count) => inner.size() * count,
            Type::Struct(_) => 0,   // Need to look up in symbol table
        }
    }

    /// Is this a float type?
    pub fn is_float(&self) -> bool {
        matches!(self, Type::Float)
    }

    /// Is this a pointer type?
    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Pointer(_))
    }
}

/// Binary operators
#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add, Sub, Mul, Div, Mod,
    BitAnd, BitOr, BitXor,
    Shl, Shr,
    Eq, Ne, Lt, Gt, Le, Ge,
    LogAnd, LogOr,
    Assign,
    AddAssign, SubAssign, MulAssign, DivAssign,
    AndAssign, OrAssign, XorAssign,
}

/// Unary operators
#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Neg,       // -
    Not,       // !
    BitNot,    // ~
    Deref,     // *
    AddrOf,    // &
    PreInc,    // ++x
    PreDec,    // --x
    PostInc,   // x++
    PostDec,   // x--
}

/// An expression
#[derive(Debug, Clone)]
pub enum Expr {
    /// Integer literal
    IntLit(i32),
    /// Float literal (stored as string to preserve exact representation)
    FloatLit(String),
    /// Character literal
    CharLit(u8),
    /// String literal (stored as label reference)
    StringLit(String),
    /// Variable reference
    Var(String),
    /// Binary operation
    Binary(BinOp, Box<Expr>, Box<Expr>),
    /// Unary operation
    Unary(UnOp, Box<Expr>),
    /// Function call
    Call(String, Vec<Expr>),
    /// Array subscript: arr[index]
    Index(Box<Expr>, Box<Expr>),
    /// Struct member access: expr.member
    Member(Box<Expr>, String),
    /// Pointer member access: expr->member
    Arrow(Box<Expr>, String),
    /// Ternary: cond ? then : else
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    /// Cast: (type)expr
    Cast(Type, Box<Expr>),
    /// Sizeof expression
    Sizeof(Type),
}

/// A statement
#[derive(Debug, Clone)]
pub enum Stmt {
    /// Expression statement
    Expr(Expr),
    /// Variable declaration with optional initializer
    VarDecl(Type, String, Option<Expr>),
    /// Block of statements
    Block(Vec<Stmt>),
    /// If statement
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    /// While loop
    While(Expr, Box<Stmt>),
    /// For loop: for (init; cond; update) body
    For(Option<Box<Stmt>>, Option<Expr>, Option<Expr>, Box<Stmt>),
    /// Do-while loop
    DoWhile(Box<Stmt>, Expr),
    /// Return statement
    Return(Option<Expr>),
    /// Break
    Break,
    /// Continue
    Continue,
}

/// A function parameter
#[derive(Debug, Clone)]
pub struct Param {
    pub ty: Type,
    pub name: String,
}

/// A function definition
#[derive(Debug, Clone)]
pub struct Function {
    pub return_type: Type,
    pub name: String,
    pub params: Vec<Param>,
    pub body: Vec<Stmt>,
}

/// A struct field
#[derive(Debug, Clone)]
pub struct Field {
    pub ty: Type,
    pub name: String,
}

/// A struct definition
#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<Field>,
}

/// A global variable
#[derive(Debug, Clone)]
pub struct Global {
    pub ty: Type,
    pub name: String,
    pub init: Option<Expr>,
}

/// Top-level declaration
#[derive(Debug, Clone)]
pub enum Decl {
    Function(Function),
    Struct(StructDef),
    Global(Global),
}

/// A complete translation unit (source file)
#[derive(Debug)]
pub struct TranslationUnit {
    pub decls: Vec<Decl>,
}
