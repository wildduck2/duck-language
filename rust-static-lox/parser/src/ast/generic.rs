// Generics and where clauses.
// This mirrors the grammar sections: genericParams, whereClause, bounds, genericArgs.

use diagnostic::Span;

use crate::ast::{Attribute, Expr, Path, Type};

#[derive(Debug, Clone)]
pub(crate) struct GenericParams {
  pub params: Vec<GenericParam>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum GenericParam {
  Type {
    attributes: Vec<Attribute>,
    name: String,
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
    name: String,
    ty: Type,
    default: Option<Expr>,
  },
}

#[derive(Debug, Clone)]
pub(crate) struct WhereClause {
  pub predicates: Vec<WherePredicate>,
}

#[derive(Debug, Clone)]
pub(crate) enum WherePredicate {
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

#[derive(Debug, Clone)]
pub(crate) enum TraitBoundModifier {
  None,
  Maybe,      // ?
  Const,      // ~const
  MaybeConst, // ?~const
}

#[derive(Debug, Clone)]
pub(crate) enum TypeBound {
  Lifetime {
    name: String,
  },
  Trait {
    modifier: TraitBoundModifier,
    for_lifetimes: Option<Vec<String>>,
    path: Path, // typePath
  },
}

#[derive(Debug, Clone)]
pub(crate) enum GenericArgs {
  AngleBracketed {
    args: Vec<GenericArg>,
  },
  Parenthesized {
    inputs: Vec<Type>,
    output: Option<Box<Type>>,
  },
}

#[derive(Debug, Clone)]
pub(crate) enum GenericArg {
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
