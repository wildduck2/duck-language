// src/ast/expr.rs
//
// Expression nodes aligned with the grammar.
// Important: expression attributes exist, so Expr is a node that wraps ExprKind.

use diagnostic::Span;

use crate::ast::{
  Attribute, GenericArgs, MacroInvocation, Mutability, Path, Pattern, QSelf, Stmt, Type,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
  pub attributes: Vec<Attribute>,
  pub kind: ExprKind,
  pub span: Span,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
  Literal(Lit),
  Path {
    qself: Option<QSelf>,
    path: Path,
  },

  Group {
    expr: Box<Expr>,
  },

  Tuple {
    elements: Vec<Expr>,
  },

  Array {
    elements: Vec<Expr>,
    repeat: Option<Box<Expr>>,
  },

  Struct {
    path: Path,
    fields: Vec<FieldInit>,
    base: Option<Box<Expr>>,
  },

  Call {
    callee: Box<Expr>,
    args: Vec<Expr>,
  },

  MethodCall {
    receiver: Box<Expr>,
    method: String,
    turbofish: Option<GenericArgs>,
    args: Vec<Expr>,
  },

  Field {
    object: Box<Expr>,
    field: FieldAccess,
  },

  Index {
    object: Box<Expr>,
    index: Box<Expr>,
  },

  Await {
    expr: Box<Expr>,
  },

  Try {
    expr: Box<Expr>,
  },

  Unary {
    op: UnaryOp,
    expr: Box<Expr>,
  },

  Binary {
    left: Box<Expr>,
    op: BinaryOp,
    right: Box<Expr>,
  },

  Assign {
    target: Box<Expr>,
    value: Box<Expr>,
  },

  AssignOp {
    target: Box<Expr>,
    op: BinaryOp,
    value: Box<Expr>,
  },

  Cast {
    expr: Box<Expr>,
    ty: Type,
  },

  Range {
    start: Option<Box<Expr>>,
    end: Option<Box<Expr>>,
    kind: RangeExprKind,
  },

  Block {
    label: Option<String>,
    flavor: BlockFlavor,
    outer_attributes: Vec<Attribute>,
    stmts: Vec<Stmt>,
    tail: Option<Box<Expr>>,
  },

  If {
    condition: Box<Expr>,
    then_branch: Box<Expr>,         // always a block expression node
    else_branch: Option<Box<Expr>>, // block or if or if let
  },

  IfLet {
    pattern: Pattern,
    scrutinee: Box<Expr>,
    then_branch: Box<Expr>,
    else_branch: Option<Box<Expr>>,
  },

  Match {
    scrutinee: Box<Expr>,
    arms: Vec<MatchArm>,
  },

  Loop {
    label: Option<String>,
    body: Box<Expr>,
  },

  While {
    label: Option<String>,
    condition: Box<Expr>,
    body: Box<Expr>,
  },

  WhileLet {
    label: Option<String>,
    pattern: Pattern,
    scrutinee: Box<Expr>,
    body: Box<Expr>,
  },

  For {
    label: Option<String>,
    pattern: Pattern,
    iterator: Box<Expr>,
    body: Box<Expr>,
  },

  Break {
    label: Option<String>,
    value: Option<Box<Expr>>,
  },

  Continue {
    label: Option<String>,
  },

  Return {
    value: Option<Box<Expr>>,
  },

  Closure {
    is_move: bool,
    is_async: bool,
    params: Vec<ClosureParam>,
    return_type: Option<Type>,
    body: Box<Expr>,
  },

  Macro {
    mac: MacroInvocation,
  },
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
  Integer {
    value: i128,
    suffix: Option<String>,
  },
  Float {
    value: f64,
    suffix: Option<String>,
  },
  String {
    value: String,
    raw_hashes: Option<usize>,
  },
  Char(char),
  Byte(u8),
  ByteString {
    value: Vec<u8>,
    raw_hashes: Option<usize>,
  },
  Bool(bool),
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum BlockFlavor {
  Normal,
  Async,
  AsyncMove,
  Unsafe,
  Try, // nightly if you support it
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldInit {
  pub attributes: Vec<Attribute>,
  pub name: FieldName,
  pub value: Option<Expr>, // None means shorthand
  pub span: Span,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum FieldName {
  Ident(String),
  TupleIndex(usize),
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum FieldAccess {
  Named(String),
  Unnamed(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureParam {
  pub attributes: Vec<Attribute>,
  pub pattern: Pattern,
  pub ty: Option<Type>,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
  pub attributes: Vec<Attribute>, // outerAttr* on the arm
  pub pattern: Pattern,
  pub guard: Option<Expr>,
  pub body: Expr,
  pub span: Span,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum RangeExprKind {
  Full,
  From,
  To,
  ToInclusive,
  FromInclusive,
  Exclusive,
  Inclusive,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  BitAnd,
  BitOr,
  BitXor,
  Shl,
  Shr,
  Eq,
  NotEq,
  Less,
  LessEqual,
  Greater,
  GreaterEq,
  And,
  Or,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
  Neg,
  Not,
  Deref,
  Ref {
    mutability: Mutability,
    depth: usize,
  },
}
