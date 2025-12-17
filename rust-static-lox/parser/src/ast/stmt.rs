// src/ast/stmt.rs
//
// Block statement list items.
// Matches: statement -> ";" | item | letStatement | expressionStatement | macroInvocationSemi

use diagnostic::Span;

use crate::ast::{Attribute, Expr, Item, MacroInvocation, Pattern, Type};

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
  Empty {
    span: Span,
  },

  Item(Box<Item>),

  Let(LetStmt),

  Macro {
    mac: MacroInvocation,
    span: Span,
  },

  Expr {
    expr: Expr,
    has_semi: bool,
    span: Span,
  },
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
  pub attributes: Vec<Attribute>,
  pub pattern: Pattern,
  pub ty: Option<Type>,
  pub init: Option<Box<Expr>>,
  pub else_block: Option<Box<Expr>>,
  pub span: Span,
}
