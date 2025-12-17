use diagnostic::Span;

use crate::ast::{
  Attribute, EnumDecl, ForeignModDecl, GenericParams, ImplBlock, Macro2Decl, MacroInvocation,
  MacroRulesDecl, Mutability, StructDecl, TraitDecl, Type, Visibility, WhereClause,
};

// ----------------------------------------------------------------------------
// Item wrapper
// ----------------------------------------------------------------------------

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
  Vis(VisItem),
  Macro(MacroItem),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VisItem {
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub kind: VisItemKind,
  pub span: Span,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum VisItemKind {
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

#[derive(Debug, Clone, PartialEq)]
pub struct MacroItem {
  pub attributes: Vec<Attribute>,
  pub kind: MacroItemKind,
  pub span: Span,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum MacroItemKind {
  Invocation(MacroInvocation),
  MacroRules(MacroRulesDecl),
  Macro2(Macro2Decl),
}

// ----------------------------------------------------------------------------
// Const and static
// ----------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub struct ConstDecl {
  pub name: crate::ast::Ident,
  pub ty: Type,
  pub value: crate::ast::Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticDecl {
  pub name: crate::ast::Ident,
  pub ty: Type,
  pub mutability: Mutability,
  pub value: Option<crate::ast::Expr>,
}

// ----------------------------------------------------------------------------
// Type alias
// ----------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAliasDecl {
  pub name: String,
  pub generics: Option<crate::ast::GenericParams>,
  pub bounds: Vec<crate::ast::TypeBound>,
  pub where_clause: Option<WhereClause>,
  pub ty: Type,
}

// ----------------------------------------------------------------------------
// Module
// ----------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleDecl {
  pub name: String,
  pub body: Option<ModuleBody>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleBody {
  pub inner_attributes: Vec<Attribute>,
  pub items: Vec<Item>,
  pub span: Span,
}

// ----------------------------------------------------------------------------
// Use
// ----------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub struct UseDecl {
  pub tree: UseTree,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum UseTree {
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

#[derive(Debug, Clone, PartialEq)]
pub struct ExternCrateDecl {
  pub name: crate::ast::Ident,
  pub alias: Option<crate::ast::Ident>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternTypeDecl {
  pub name: String,
}

// ----------------------------------------------------------------------------
// Union
// ----------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub struct UnionDecl {
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

#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
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

#[derive(Debug, Clone, PartialEq)]
pub struct FnSig {
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

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
  pub attributes: Vec<Attribute>,
  pub kind: ParamKind,
  pub span: Span,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum ParamKind {
  SelfParam(SelfParam),
  Normal {
    pattern: crate::ast::Pattern,
    type_annotation: Option<crate::ast::Type>,
  },
  Variadic,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum SelfParam {
  Shorthand {
    reference: Option<SelfRef>,
    mutability: crate::ast::Mutability,
  },
  Typed {
    mutability: crate::ast::Mutability,
    ty: crate::ast::Type,
  },
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelfRef {
  pub lifetime: Option<String>,
}
