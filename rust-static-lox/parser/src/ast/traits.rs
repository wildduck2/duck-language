// Trait declarations and associated items.
// Trait bodies can contain inner attributes.

use diagnostic::Span;

use crate::ast::{Attribute, FnDecl, GenericParams, Type, TypeBound, Visibility, WhereClause};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TraitDecl {
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub name: String,
  pub is_auto: bool,
  pub is_unsafe: bool,
  pub generics: Option<GenericParams>,
  pub supertraits: Vec<TypeBound>,
  pub where_clause: Option<WhereClause>,

  // These are the inner attributes inside the trait body: { #![..] ... }
  pub inner_attributes: Vec<Attribute>,
  pub items: Vec<TraitItem>,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TraitItem {
  Method(FnDecl),

  Type {
    attributes: Vec<Attribute>,
    name: String,
    generics: Option<GenericParams>,
    bounds: Vec<TypeBound>,
    where_clause: Option<WhereClause>,
    default: Option<Type>,
    span: Span,
  },

  Const {
    attributes: Vec<Attribute>,
    name: crate::ast::Ident,
    ty: Type,
    default: Option<crate::ast::Expr>,
    span: Span,
  },

  Macro {
    mac: crate::ast::MacroInvocation,
  },
}
