// extern blocks.
// Foreign module bodies can contain inner attributes.

use diagnostic::Span;

use crate::ast::{Attribute, FnSig, Ident, Mutability, Type, Visibility};

#[derive(Debug, Clone)]
pub(crate) struct ForeignModDecl {
  pub attributes: Vec<Attribute>,
  pub is_unsafe: bool,
  pub abi: Option<String>,

  pub inner_attributes: Vec<Attribute>,
  pub items: Vec<ForeignItem>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum ForeignItem {
  Function {
    attributes: Vec<Attribute>,
    visibility: Visibility,
    sig: FnSig,
    span: Span,
  },
  Static {
    attributes: Vec<Attribute>,
    visibility: Visibility,
    name: Ident,
    ty: Type,
    mutability: Mutability,
    span: Span,
  },
}
