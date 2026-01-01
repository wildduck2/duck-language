// extern blocks.
// Foreign module bodies can contain inner attributes.

use diagnostic::Span;

use crate::ast::{Attribute, FnSig, Ident, MacroInvocation, Mutability, Type, Visibility};

#[derive(Debug, Clone, PartialEq)]
pub struct ForeignModDecl {
  pub is_unsafe: bool,
  pub abi: Option<String>,

  pub inner_attributes: Vec<Attribute>,
  pub items: Vec<ForeignItem>,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum ForeignItem {
  Function {
    attributes: Vec<Attribute>, // outer attrs: #[...]
    visibility: Visibility,     // visibility?
    sig: FnSig,                 // functionSig + ';'
    span: Span,
  },

  Static {
    attributes: Vec<Attribute>, // outer attrs: #[...]
    visibility: Visibility,     // visibility?
    name: Ident,                // IDENT | _
    ty: Type,
    mutability: Mutability, // static mut?
    span: Span,
  },

  Type {
    attributes: Vec<Attribute>, // outer attrs: #[...]
    visibility: Visibility,     // visibility?
    name: Ident,                // type IDENT;
    span: Span,
  },

  MacroInvocationSemi {
    attributes: Vec<Attribute>, // outer attrs: #[...]
    invoc: MacroInvocation,     // simplePath ! delimTokenTree
    span: Span,
  },
}
