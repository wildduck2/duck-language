use crate::ast::*;

// NOTE: `GenericParams` are used in declarations (defining generics),
// while `GenericArgs` are used in expressions or instantiations (applying generics).
//
// Example:
//   Declaration:  struct Vec<T> { ... }        // defines generic param T
//   Instantiation: let x: Vec<u8> = Vec::new(); // applies generic arg u8

/// A collection of generic parameters attached to a declaration.
///
/// Appears on items such as structs, enums, traits, and functions:
/// ```rust
/// struct Vec<T> { ... }
/// fn foo<'a, T: Clone, const N: usize>() { ... }
/// ```
#[derive(Debug, Clone)]
pub(crate) struct GenericParams {
  /// All parameters declared within the `<...>` list.
  pub params: Vec<GenericParam>,
  /// Source span covering the full generic parameter section.
  pub span: Span,
}

/// Represents a single generic parameter - type, lifetime, or const.
///
/// Examples:
/// - `T: Clone` -> [`GenericParam::Type`]
/// - `'a: 'b` -> [`GenericParam::Lifetime`]
/// - `const N: usize = 3` -> [`GenericParam::Const`]
#[derive(Debug, Clone)]
pub(crate) enum GenericParam {
  /// A type parameter like `T: Clone` or `T = i32`.
  Type {
    /// Outer attributes attached to the parameter.
    attributes: Vec<Attribute>,
    /// The parameter name.
    name: String,
    /// Optional bounds after `:`.
    bounds: Vec<TypeBound>,
    /// Optional default type after `=`.
    default: Option<Type>,
  },

  /// A lifetime parameter like `'a` or `'a: 'b + 'c`.
  Lifetime {
    /// Outer attributes attached to the lifetime parameter.
    attributes: Vec<Attribute>,
    /// The lifetime name (e.g., `'a`).
    name: String,
    /// Optional lifetime bounds.
    bounds: Vec<TypeBound>,
  },

  /// A const parameter such as `const N: usize = 3`.
  Const {
    /// Outer attributes attached to the const parameter.
    attributes: Vec<Attribute>,
    /// The parameter name.
    name: String,
    /// The type of the const parameter (e.g., `usize`).
    ty: Type,
    /// Optional default constant value.
    default: Option<Expr>,
  },
}

/// A single type bound after `:`, appearing in generics or where clauses.
///
/// Examples:
/// ```rust
/// T: Clone + Default
/// for<'a> F: Fn(&'a str)
/// ```
#[derive(Debug, Clone)]
pub(crate) enum TypeBound {
  Lifetime {
    name: String, // 'a
  },
  Trait {
    modifier: TraitBoundModifier,
    path: Path,
    generics: Option<GenericArgs>,
    for_lifetimes: Option<Vec<String>>,
  },
}

/// Possible modifiers that alter a trait bound’s semantics.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TraitBoundModifier {
  None,       // Trait
  Maybe,      // ?Trait
  Const,      // ~const Trait
  MaybeConst, // ?~const Trait
}

/// A `where` clause listing additional type and lifetime constraints.
///
/// Example:
/// ```rust
/// where
///     T: Clone + Default,
///     'a: 'b,
///     U::Item = i32
/// ```
#[derive(Debug, Clone)]
pub(crate) struct WhereClause {
  /// The set of predicates inside the `where` clause.
  pub predicates: Vec<WherePredicate>,
}

/// A single predicate within a `where` clause.
///
/// Represents type bounds, lifetime outlives relationships, or equality constraints.
///
/// Examples:
/// - `T: Clone + Default` -> `Type`
/// - `'a: 'b + 'c` -> `Lifetime`
/// - `T::Item = i32` -> `Equality`
#[derive(Debug, Clone)]
pub(crate) enum WherePredicate {
  /// A type predicate like `T: Clone` or `for<'a> F: Fn(&'a str)`.
  Type {
    /// Optional `for<'a>` lifetimes bound to this predicate.
    for_lifetimes: Option<Vec<String>>,
    /// The subject type on the left-hand side of the `:`.
    ty: Type,
    /// The list of trait or lifetime bounds applied.
    bounds: Vec<TypeBound>,
  },

  /// A lifetime outlives predicate, e.g. `'a: 'b + 'c`.
  Lifetime {
    /// The lifetime being constrained.
    lifetime: String,
    /// Lifetimes that it must outlive.
    bounds: Vec<String>,
  },

  /// An equality predicate, e.g. `T::Item = i32`.
  Equality {
    /// The left-hand side type (usually an associated type).
    ty: Type,
    /// The right-hand side type or value being equated.
    equals: Type,
  },
}

/// Represents the generic arguments attached to a path segment.
///
/// Generic arguments provide additional information to parameterized types,
/// traits, or functions, such as concrete types, lifetimes, or const values.
///
/// ```rust
/// Vec::<u8>
/// Iterator::<Item = T>
/// Fn(i32, bool) -> String
/// ```
///
/// Rust supports two kinds of generic argument syntax:
#[derive(Debug, Clone)]
pub(crate) enum GenericArgs {
  /// Angle-bracketed arguments - `<...>` - used in most generic type and trait contexts.
  ///
  /// Example forms:
  /// - `Vec::<u8>`
  /// - `Iterator::<Item = T>`
  /// - `Array::<T, 32>`
  AngleBracketed {
    /// The list of generic arguments inside `<...>`.
    args: Vec<GenericArg>,
  },

  /// TODO: missing some variants of it the parnethesized
  /// Parenthesized arguments - `(T1, T2) -> T3` - used in function-like trait calls or `Fn` traits.
  ///
  /// Example forms:
  /// - `Fn(i32, bool) -> String`
  /// - `FnOnce(T) -> U`
  /// - `Callable()` (with no output type)
  Parenthesized {
    /// The list of input parameter types.
    inputs: Vec<Type>,
    /// The optional output type following `->`.
    output: Option<Type>,
  },
}

/// Represents a single argument inside a generic argument list (`<...>`).
///
/// Generic arguments can appear in many Rust constructs such as types,
/// trait bounds, and where clauses. This enum captures all possible forms:
///
/// ```rust
/// Vec::<u8>
/// HashMap::<K, V>
/// Array::<T, 32>
/// Iterator::<Item = T>
/// Foo::<Item: Clone>
/// ```
///
/// Each variant corresponds to a different kind of generic argument:
#[derive(Debug, Clone)]
pub(crate) enum GenericArg {
  /// A lifetime argument.
  ///
  /// Example: `'a` in `Foo<'a>`.
  Lifetime(String),

  /// A type argument.
  ///
  /// Example: `u8` or `T` in `Vec<u8>` or `Option<T>`.
  Type(Type),

  /// A constant expression argument used in const generics.
  ///
  /// Example: `32` in `Array<T, 32>` or `{N + 1}` in `Matrix<T, {N + 1}>`.
  Const(Expr),

  /// An associated type binding that assigns a concrete type to a generic parameter.
  ///
  /// Example: `Item = U` in `Iterator<Item = U>`.
  Binding {
    /// The name of the associated type.
    name: String,
    /// Optional nested generic parameters for the binding.
    /// Example: `Item<T>` in `Trait<Item<T> = U>`.
    generics: Option<GenericParams>,
    /// The type assigned to the binding.
    ty: Type,
  },

  /// An associated type constraint that restricts bounds on an associated type.
  ///
  /// Example: `Item: Clone + Debug` in `Iterator<Item: Clone + Debug>`.
  Constraint {
    /// The name of the associated type being constrained.
    name: String,
    /// Optional generic parameters for the associated type.
    /// Example: `Item<T>` in `Trait<Item<T>: Bound>`.
    generics: Option<GenericParams>,
    /// A list of bounds applied to the associated type.
    bounds: Vec<TypeBound>,
  },
}
