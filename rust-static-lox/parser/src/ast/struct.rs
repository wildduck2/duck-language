// Struct declarations and fields.

use diagnostic::Span;

use crate::ast::{Attribute, GenericParams, Type, Visibility, WhereClause};

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
  pub name: String,
  pub generics: Option<GenericParams>,
  pub kind: StructKind,
  pub where_clause: Option<WhereClause>,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum StructKind {
  Named { fields: Vec<FieldDecl> },
  Tuple { fields: Vec<TupleField> },
  Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDecl {
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub name: String,
  pub ty: Type,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleField {
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub ty: Type,
  pub span: Span,
}
