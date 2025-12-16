// Unifies simplePath, typePath, and pathInExpression at the AST level.
// The parser enforces contextual restrictions (ex: no turbofish in typePath if you want).

use crate::ast::generic::GenericArgs;

#[derive(Debug, Clone)]
pub(crate) struct Path {
  pub leading_colon: bool,
  pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone)]
pub struct PathSegment {
  pub kind: PathSegmentKind,
  pub args: Option<GenericArgs>,
}

#[derive(Debug, Clone)]
pub enum PathSegmentKind {
  Ident(String),
  Super,
  Self_,
  Crate,
  DollarCrate,
  SelfType,
}

impl Path {
  pub fn new(leading_colon: bool, segments: Vec<PathSegment>) -> Self {
    Self {
      leading_colon,
      segments,
    }
  }

  pub fn from_ident(ident: String) -> Self {
    Self {
      leading_colon: false,
      segments: vec![PathSegment::new(PathSegmentKind::Ident(ident), None)],
    }
  }
}

impl PathSegment {
  pub fn new(kind: PathSegmentKind, args: Option<GenericArgs>) -> Self {
    Self { kind, args }
  }
}
