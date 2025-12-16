// Type nodes that mirror the "Types" section.
// Note: Do not model "String" as a primitive. In Rust it is a path (std::string::String).

use diagnostic::Span;

use crate::ast::{Expr, GenericArgs, Path, TypeBound};

#[derive(Debug, Clone)]
pub(crate) enum Mutability {
  Mutable,
  Immutable,
}

#[derive(Debug, Clone)]
pub(crate) enum Safety {
  Safe,
  Unsafe,
}

#[derive(Debug, Clone)]
pub(crate) struct QSelf {
  pub self_ty: Box<Type>,
  pub as_trait: Option<Path>,
}

#[derive(Debug, Clone)]
pub(crate) enum Type {
  // Primitive types
  I8,
  I16,
  I32,
  I64,
  I128,
  Isize,
  U8,
  U16,
  U32,
  U64,
  U128,
  Usize,
  F32,
  F64,
  Bool,
  Char,
  Str,
  Never,
  Unit,
  SelfType,

  // Compound
  Tuple(Vec<Type>),
  Array {
    element: Box<Type>,
    size: Box<Expr>,
  },
  Slice(Box<Type>),

  Reference {
    lifetime: Option<String>,
    mutability: Mutability,
    inner: Box<Type>,
  },
  RawPointer {
    mutability: Mutability,
    inner: Box<Type>,
  },

  BareFn {
    for_lifetimes: Option<Vec<String>>,
    safety: Safety,
    abi: Option<String>,
    params: Vec<Type>,
    return_type: Option<Box<Type>>,
    is_variadic: bool,
  },

  // Paths
  Path(Path),
  QualifiedPath {
    qself: QSelf,
    path: Path,
  },

  // dyn Trait + bounds
  TraitObject {
    bounds: Vec<TypeBound>,
    lifetime: Option<String>,
    is_dyn: bool,
  },

  // impl Trait
  ImplTrait(Vec<TypeBound>),

  Infer,
  Paren(Box<Type>),

  // Macro in type position
  Macro {
    path: Path,
    args: Option<GenericArgs>,
    span: Span,
  },
}
