// Enum declarations and variants.

use diagnostic::Span;

use crate::ast::{Attribute, Expr, GenericParams, Visibility, WhereClause};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct EnumDecl {
  pub name: String,
  pub generics: Option<GenericParams>,
  pub where_clause: Option<WhereClause>,
  pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct EnumVariant {
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub name: String,
  pub kind: EnumVariantKind,
  pub discriminant: Option<Expr>,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum EnumVariantKind {
  Unit,
  Tuple { fields: Vec<crate::ast::TupleField> },
  Struct { fields: Vec<crate::ast::FieldDecl> },
}
