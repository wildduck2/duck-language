// Enum declarations and variants.

use diagnostic::Span;

use crate::ast::{Attribute, Expr, GenericParams, Ident, Visibility, WhereClause};

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
  pub name: Ident,
  pub generics: Option<GenericParams>,
  pub where_clause: Option<WhereClause>,
  pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub name: Ident,
  pub kind: EnumVariantKind,
  pub discriminant: Option<Expr>,
  pub span: Span,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariantKind {
  Unit,
  Tuple { fields: Vec<crate::ast::TupleField> },
  Struct { fields: Vec<crate::ast::FieldDecl> },
}
