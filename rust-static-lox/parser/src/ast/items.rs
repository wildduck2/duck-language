use diagnostic::Span;

use crate::ast::{
  Attribute, EnumDecl, ForeignModDecl, GenericParams, ImplBlock, Macro2Decl, MacroInvocation,
  MacroRulesDecl, Mutability, StructDecl, TraitDecl, Type, Visibility, WhereClause,
};

// ----------------------------------------------------------------------------
// Item wrapper
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) enum Item {
  Vis(VisItem),
  Macro(MacroItem),
}

#[derive(Debug, Clone)]
pub(crate) struct VisItem {
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub kind: VisItemKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum VisItemKind {
  Function(FnDecl),
  Struct(StructDecl),
  Enum(EnumDecl),
  Trait(TraitDecl),
  Impl(ImplBlock),
  Const(ConstDecl),
  Static(StaticDecl),
  TypeAlias(TypeAliasDecl),
  Module(ModuleDecl),
  Use(UseDecl),
  ExternCrate(ExternCrateDecl),
  ForeignMod(ForeignModDecl),
  Union(UnionDecl),
  ExternType(ExternTypeDecl),
}

// ----------------------------------------------------------------------------
// Macro items
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct MacroItem {
  pub attributes: Vec<Attribute>,
  pub kind: MacroItemKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum MacroItemKind {
  Invocation(MacroInvocation),
  MacroRules(MacroRulesDecl),
  Macro2(Macro2Decl),
}

// ----------------------------------------------------------------------------
// Const and static
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct ConstDecl {
  pub name: crate::ast::Ident,
  pub ty: Type,
  pub value: crate::ast::Expr,
}

#[derive(Debug, Clone)]
pub(crate) struct StaticDecl {
  pub name: crate::ast::Ident,
  pub ty: Type,
  pub mutability: Mutability,
  pub value: Option<crate::ast::Expr>,
}

// ----------------------------------------------------------------------------
// Type alias
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct TypeAliasDecl {
  pub name: String,
  pub generics: Option<crate::ast::GenericParams>,
  pub bounds: Vec<crate::ast::TypeBound>,
  pub where_clause: Option<WhereClause>,
  pub ty: Type,
}

// ----------------------------------------------------------------------------
// Module
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct ModuleDecl {
  pub name: String,
  pub body: Option<ModuleBody>,
}

#[derive(Debug, Clone)]
pub(crate) struct ModuleBody {
  pub inner_attributes: Vec<Attribute>,
  pub items: Vec<Item>,
  pub span: Span,
}

// ----------------------------------------------------------------------------
// Use
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct UseDecl {
  pub tree: UseTree,
}

#[derive(Debug, Clone)]
pub(crate) enum UseTree {
  Path {
    prefix: String,
    suffix: Box<UseTree>,
  },
  Name(String),
  Rename {
    name: String,
    alias: crate::ast::Ident,
  },
  Glob,
  List(Vec<UseTree>),
}

// ----------------------------------------------------------------------------
// Extern crate and extern type
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct ExternCrateDecl {
  pub name: crate::ast::Ident,
  pub alias: Option<crate::ast::Ident>,
}

#[derive(Debug, Clone)]
pub(crate) struct ExternTypeDecl {
  pub name: String,
}

// ----------------------------------------------------------------------------
// Union
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct UnionDecl {
  pub name: String,
  pub generics: Option<crate::ast::GenericParams>,
  pub where_clause: Option<WhereClause>,
  pub fields: Vec<crate::ast::FieldDecl>,
}

// ----------------------------------------------------------------------------
// Functions
// ----------------------------------------------------------------------------
//
// FnDecl intentionally does NOT contain attributes or visibility.
// Those live on VisItem per grammar.

#[derive(Debug, Clone)]
pub(crate) struct FnDecl {
  pub sig: FnSig,

  // None for extern functions and trait methods without a body
  pub body: Option<crate::ast::Expr>,

  // qualifiers
  pub is_async: bool,
  pub is_const: bool,
  pub is_unsafe: bool,
  pub is_extern: bool,
  pub abi: Option<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct FnSig {
  pub name: String,
  pub generics: Option<GenericParams>,
  pub params: Vec<Param>,
  pub return_type: Option<Type>,
  pub where_clause: Option<WhereClause>,
  pub span: Span,
}

// ----------------------------------------------------------------------------
// Parameters
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct Param {
  pub attributes: Vec<Attribute>,
  pub kind: ParamKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum ParamKind {
  SelfParam(SelfParam),
  Normal {
    pattern: crate::ast::Pattern,
    type_annotation: Option<crate::ast::Type>,
  },
  Variadic,
}

#[derive(Debug, Clone)]
pub(crate) enum SelfParam {
  Shorthand {
    reference: Option<SelfRef>,
    mutability: crate::ast::Mutability,
  },
  Typed {
    mutability: crate::ast::Mutability,
    ty: crate::ast::Type,
  },
}

#[derive(Debug, Clone)]
pub(crate) struct SelfRef {
  pub lifetime: Option<String>,
}
