// Impl blocks and items.
// Impl bodies can contain inner attributes.

use diagnostic::Span;

use crate::ast::{Attribute, FnDecl, GenericParams, Ident, Path, Type, Visibility, WhereClause};

#[derive(Debug, Clone)]
pub(crate) enum ImplPolarity {
  Positive,
  Negative,
}

#[derive(Debug, Clone)]
pub(crate) struct ImplBlock {
  pub attributes: Vec<Attribute>,
  pub is_unsafe: bool,
  pub is_const: bool,
  pub generics: Option<GenericParams>,
  pub polarity: ImplPolarity,
  pub trait_ref: Option<Path>,
  pub self_ty: Type,
  pub where_clause: Option<WhereClause>,

  pub inner_attributes: Vec<Attribute>,
  pub items: Vec<ImplItem>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum ImplItem {
  Method(FnDecl),

  Type {
    attributes: Vec<Attribute>,
    visibility: Visibility,
    name: String,
    generics: Option<GenericParams>,
    where_clause: Option<WhereClause>,
    ty: Type,
    span: Span,
  },

  Const {
    attributes: Vec<Attribute>,
    visibility: Visibility,
    name: Ident,
    ty: Type,
    value: crate::ast::Expr,
    span: Span,
  },

  Macro {
    mac: crate::ast::MacroInvocation,
  },
}
