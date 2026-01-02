// Generics and where clauses.
// This mirrors the grammar sections: genericParams, whereClause, bounds, genericArgs.

use diagnostic::Span;

use crate::ast::{Attribute, Expr, Ident, Path, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct GenericParams {
  pub params: Vec<GenericParam>,
  pub span: Span,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum GenericParam {
  Type {
    attributes: Vec<Attribute>,
    name: Ident,
    bounds: Vec<TypeBound>,
    default: Option<Type>,
  },
  Lifetime {
    attributes: Vec<Attribute>,
    name: String, // "'a"
    bounds: Vec<String>,
  },
  Const {
    attributes: Vec<Attribute>,
    name: Ident,
    ty: Type,
    default: Option<Expr>,
  },
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhereClause {
  pub predicates: Vec<WherePredicate>,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum WherePredicate {
  Lifetime {
    lifetime: String,
    bounds: Vec<String>,
  },
  Type {
    for_lifetimes: Option<Vec<String>>,
    ty: Type,
    bounds: Vec<TypeBound>,
  },

  // Rust also has equality predicates (associated type equality).
  // If you parse them, keep them here even if your current grammar does not yet list them.
  Equality {
    ty: Type,
    equals: Type,
  },
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum TraitBoundModifier {
  None,
  Maybe,      // ?
  Const,      // ~const
  MaybeConst, // ?~const
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum TypeBound {
  Lifetime {
    name: String,
  },
  Trait {
    modifier: TraitBoundModifier,
    for_lifetimes: Option<Vec<String>>,
    path: Path, // typePath
  },
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum GenericArgs {
  AngleBracketed {
    args: Vec<GenericArg>,
  },
  Parenthesized {
    inputs: Vec<Type>,
    output: Option<Box<Type>>,
  },
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum GenericArg {
  Lifetime(String),
  Type(Type),
  Const(Expr),
  Binding {
    name: String,
    args: Option<GenericArgs>,
    ty: Type,
  },
  Constraint {
    name: String,
    args: Option<GenericArgs>,
    bounds: Vec<TypeBound>,
  },
}
