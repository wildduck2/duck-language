// ============================================================================
// Complete Rust AST aligned with the recursive descent grammar
// ============================================================================

pub(crate) mod generic;
pub(crate) mod path;
pub(crate) mod pattern;
pub(crate) mod print;
pub(crate) mod r#struct;

use diagnostic::Span;
use generic::*;
use path::*;
use pattern::*;
use r#struct::*;

// ----------------------------------------------------------------------------
// Top level Items
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) enum Item {
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
  Macro(MacroDecl),
  Macro2(Macro2Decl),
  ForeignMod(ForeignModDecl),
  Union(UnionDecl),
  ExternType(ExternTypeDecl),
}

// ----------------------------------------------------------------------------
// Extern type
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct ExternTypeDecl {
  pub visibility: Visibility,
  pub name: String,
  pub span: Span,
}

// ----------------------------------------------------------------------------
// Const, static, type alias, module, use, extern crate
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct ConstDecl {
  pub visibility: Visibility,
  pub name: String,
  pub ty: Type,
  // grammar requires a value for free const items
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct StaticDecl {
  pub visibility: Visibility,
  pub name: String,
  pub ty: Type,
  pub mutability: Mutability,
  // grammar allows optional initializer for free static
  pub value: Option<Expr>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct TypeAliasDecl {
  pub visibility: Visibility,
  pub name: String,
  pub generics: Option<GenericParams>,
  pub bounds: Option<Vec<TypeBound>>,
  pub where_clause: Option<WhereClause>,
  // grammar requires "=" type for free type alias
  pub ty: Type,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct ModuleDecl {
  pub visibility: Visibility,
  pub name: String,
  // None for "mod foo;" and Some(items) for "mod foo { ... }"
  pub items: Option<Vec<Item>>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct UseDecl {
  pub visibility: Visibility,
  pub tree: UseTree,
  pub span: Span,
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
    alias: String,
  },
  Glob,
  List(Vec<UseTree>),
}

#[derive(Debug, Clone)]
pub(crate) struct ExternCrateDecl {
  pub visibility: Visibility,
  pub name: String,
  pub alias: Option<String>,
  pub span: Span,
}

// ----------------------------------------------------------------------------
// – Macros
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct MacroDecl {
  pub name: String,
  pub rules: Vec<MacroRule>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Macro2Decl {
  pub visibility: Visibility,
  pub name: String,
  pub params: Vec<MacroParam>,
  pub body: Vec<TokenTree>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct MacroParam {
  pub name: String,
  pub kind: MacroParamKind,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum MacroParamKind {
  Block,
  Expr,
  Ident,
  Item,
  Lifetime,
  Literal,
  Meta,
  Pat,
  PatParam,
  Path,
  Stmt,
  Tt,
  Ty,
  Vis,
}

#[derive(Debug, Clone)]
pub(crate) struct MacroRule {
  pub matcher: Vec<TokenTree>,
  pub transcriber: Vec<TokenTree>,
}

#[derive(Debug, Clone)]
pub(crate) enum TokenTree {
  Token(String),
  Delimited {
    delimiter: Delimiter,
    tokens: Vec<TokenTree>,
  },
  Repeat {
    tokens: Vec<TokenTree>,
    separator: Option<String>,
    kind: RepeatKind,
  },
  MetaVar {
    name: String,
    kind: String,
  },
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Delimiter {
  Paren,
  Brace,
  Bracket,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum RepeatKind {
  ZeroOrMore,
  OneOrMore,
  ZeroOrOne,
}

// ----------------------------------------------------------------------------
// Foreign items and unions
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct ForeignModDecl {
  pub is_unsafe: bool,
  pub abi: Option<String>,
  pub items: Vec<ForeignItem>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum ForeignItem {
  Function {
    visibility: Visibility,
    name: String,
    generics: Option<GenericParams>,
    params: Vec<Param>,
    return_type: Option<Type>,
    is_variadic: bool,
    span: Span,
  },
  Static {
    visibility: Visibility,
    name: String,
    ty: Type,
    mutability: Mutability,
    span: Span,
  },
}

#[derive(Debug, Clone)]
pub(crate) struct UnionDecl {
  pub visibility: Visibility,
  pub name: String,
  pub generics: Option<GenericParams>,
  pub fields: Vec<FieldDecl>,
  pub where_clause: Option<WhereClause>,
  pub span: Span,
}

// ----------------------------------------------------------------------------
// Functions
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct FnDecl {
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub name: String,
  pub generics: Option<GenericParams>,
  pub params: Vec<Param>,
  pub return_type: Option<Type>,
  pub where_clause: Option<WhereClause>,
  // None for trait methods without body or extern functions
  pub body: Option<Expr>,
  pub is_async: bool,
  pub is_const: bool,
  pub is_unsafe: bool,
  pub is_extern: bool,
  pub abi: Option<String>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Param {
  pub attributes: Vec<Attribute>,
  pub kind: ParamKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum ParamKind {
  Normal {
    pattern: Pattern,
    type_annotation: Option<Type>,
    is_self: bool,
  },
  Variadic,
}

// ----------------------------------------------------------------------------
// Attributes
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct Attribute {
  pub style: AttrStyle,
  pub kind: AttrKind,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum AttrStyle {
  Outer,
  Inner,
}

#[derive(Debug, Clone)]
pub(crate) enum AttrKind {
  Normal {
    path: Path,
    tokens: Vec<TokenTree>,
  },
  DocComment {
    is_inner: bool,
    content: String,
  },
  Cfg(MetaItem),
  CfgAttr {
    condition: MetaItem,
    attrs: Vec<Attribute>,
  },
}

// ----------------------------------------------------------------------------
// Meta items
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) enum MetaItem {
  Word(String),
  NameValue(String, MetaItemValue),
  List(String, Vec<MetaItem>),
}

#[derive(Debug, Clone)]
pub(crate) enum MetaItemValue {
  Str(String),
  Int(i128),
  Bool(bool),
}

// ----------------------------------------------------------------------------
// Enums
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct EnumDecl {
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub name: String,
  pub generics: Option<GenericParams>,
  pub variants: Vec<EnumVariant>,
  pub where_clause: Option<WhereClause>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct EnumVariant {
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub name: String,
  pub kind: EnumVariantKind,
  pub discriminant: Option<Expr>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum EnumVariantKind {
  Unit,
  Tuple(Vec<TupleField>),
  Struct(Vec<FieldDecl>),
}

// ----------------------------------------------------------------------------
// Traits
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct TraitDecl {
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub name: String,
  pub is_auto: bool,
  pub is_unsafe: bool,
  pub generics: Option<GenericParams>,
  pub supertraits: Option<Vec<TypeBound>>,
  pub items: Vec<TraitItem>,
  pub where_clause: Option<WhereClause>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum TraitItem {
  Method(FnDecl),
  Type {
    attributes: Vec<Attribute>,
    name: String,
    generics: Option<GenericParams>,
    bounds: Option<Vec<TypeBound>>,
    default: Option<Type>,
  },
  Const {
    attributes: Vec<Attribute>,
    name: String,
    ty: Type,
    default: Option<Expr>,
  },
  Macro {
    mac: MacroInvocation,
  },
}

// ----------------------------------------------------------------------------
// Macro invocation as expression
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct MacroInvocation {
  pub qself: Option<Box<Type>>,
  pub path: Path,
  pub delimiter: Delimiter,
  pub tokens: Vec<TokenTree>,
  pub span: Span,
}

// ----------------------------------------------------------------------------
// Impl blocks
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct ImplBlock {
  pub attributes: Vec<Attribute>,
  pub is_unsafe: bool,
  pub is_const: bool,
  pub generics: Option<GenericParams>,
  pub polarity: ImplPolarity,
  pub trait_ref: Option<Path>,
  pub self_ty: Type,
  pub items: Vec<ImplItem>,
  pub where_clause: Option<WhereClause>,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ImplPolarity {
  Positive,
  Negative,
}

#[derive(Debug, Clone)]
pub(crate) enum ImplItem {
  Method(FnDecl),
  Type {
    attributes: Vec<Attribute>,
    visibility: Visibility,
    name: String,
    generics: Option<GenericParams>,
    ty: Type,
  },
  Const {
    attributes: Vec<Attribute>,
    visibility: Visibility,
    name: String,
    ty: Type,
    value: Expr,
  },
  Macro {
    mac: MacroInvocation,
  },
}

// ----------------------------------------------------------------------------
// Visibility
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum Visibility {
  Lic,
  LicCrate,
  LicSuper,
  LicSelf,
  LicIn(Path),
  Private,
}

// ----------------------------------------------------------------------------
// Types
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct QSelfHeader {
  pub self_ty: Box<Type>,
  pub trait_ref: Option<Path>,
}

#[derive(Debug, Clone)]
pub(crate) enum Type {
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
  F128,
  Bool,
  Char,
  Str,
  String,
  Never,

  SelfType,
  Unit,

  Array {
    element: Box<Type>,
    size: Box<Expr>,
  },
  Slice(Box<Type>),
  Tuple(Vec<Type>),

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
    params: Vec<BareFnParam>,
    return_type: Option<Box<Type>>,
    is_variadic: bool,
  },

  Path(Path),

  QPath {
    self_ty: Box<Type>,
    trait_ref: Option<Path>,
    name: String,
    generics: Option<Box<GenericArgs>>,
  },

  TraitObject {
    bounds: Vec<TypeBound>,
    lifetime: Option<String>,
    is_dyn: bool,
  },

  ImplTrait(Vec<TypeBound>),

  Infer,
  Paren(Box<Type>),
  Macro(Box<MacroInvocation>),
  Typeof(Box<Expr>),
}

#[derive(Debug, Clone)]
pub(crate) struct BareFnParam {
  pub attributes: Vec<Attribute>,
  pub name: Option<String>,
  pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Safety {
  Safe,
  Unsafe,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Mutability {
  Mutable,
  Immutable,
}

// ----------------------------------------------------------------------------
// Statements
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
  Expr(Expr),     // used if you want to keep non tail exprs distinct
  Semi(Expr),     // expression followed by semicolon
  TailExpr(Expr), // last expression in a block without semicolon
  Let {
    attributes: Vec<Attribute>,
    pattern: Pattern,
    ty: Option<Type>,
    init: Option<Box<Expr>>,
    else_block: Option<Box<Expr>>,
    span: Span,
  },
  Macro {
    mac: MacroInvocation,
    span: Span,
  },
  Item(Box<Item>),
  Empty,
}

// ----------------------------------------------------------------------------
// Match arms
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct MatchArm {
  pub attributes: Vec<Attribute>,
  pub pattern: Pattern,
  pub guard: Option<Expr>,
  pub body: Expr,
  pub span: Span,
}

// ----------------------------------------------------------------------------
// Expressions
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) enum Expr {
  Integer {
    value: i128,
    suffix: Option<String>,
    span: Span,
  },
  Float {
    value: f64,
    suffix: Option<String>,
    span: Span,
  },
  String {
    value: String,
    kind: StrKind,
    span: Span,
  },
  Char {
    value: char,
    span: Span,
  },
  ByteString {
    value: Vec<u8>,
    kind: ByteStrKind,
    span: Span,
  },
  Byte {
    value: u8,
    span: Span,
  },
  Bool {
    value: bool,
    span: Span,
  },

  Ident {
    name: String,
    span: Span,
  },

  Path(Path),

  Binary {
    left: Box<Expr>,
    op: BinaryOp,
    right: Box<Expr>,
    span: Span,
  },

  Unary {
    op: UnaryOp,
    expr: Box<Expr>,
    span: Span,
  },

  Group {
    expr: Box<Expr>,
    span: Span,
  },

  Tuple {
    elements: Vec<Expr>,
    span: Span,
  },

  Assign {
    target: Box<Expr>,
    value: Box<Expr>,
    span: Span,
  },

  AssignOp {
    target: Box<Expr>,
    op: BinaryOp,
    value: Box<Expr>,
    span: Span,
  },

  Field {
    object: Box<Expr>,
    field: FieldAccess,
    span: Span,
  },

  MethodCall {
    receiver: Box<Expr>,
    method: String,
    turbofish: Option<GenericArgs>,
    args: Vec<Expr>,
    span: Span,
  },

  Call {
    callee: Box<Expr>,
    args: Vec<Expr>,
    span: Span,
  },

  Index {
    object: Box<Expr>,
    index: Box<Expr>,
    span: Span,
  },

  Unit(Span),

  Range {
    start: Option<Box<Expr>>,
    end: Option<Box<Expr>>,
    kind: RangeKind,
    span: Span,
  },

  Array {
    elements: Vec<Expr>,
    repeat: Option<Box<Expr>>,
    span: Span,
  },

  Struct {
    path: Path,
    fields: Vec<FieldInit>,
    base: Option<Box<Expr>>,
    span: Span,
  },

  If {
    condition: Box<Expr>,
    then_branch: Box<Expr>,
    else_branch: Option<Box<Expr>>,
    span: Span,
  },

  IfLet {
    pattern: Pattern,
    scrutinee: Box<Expr>,
    then_branch: Box<Expr>,
    else_branch: Option<Box<Expr>>,
    span: Span,
  },

  Match {
    scrutinee: Box<Expr>,
    arms: Vec<MatchArm>,
    span: Span,
  },

  Loop {
    body: Box<Expr>,
    label: Option<String>,
    span: Span,
  },

  While {
    condition: Box<Expr>,
    body: Box<Expr>,
    label: Option<String>,
    span: Span,
  },

  WhileLet {
    pattern: Pattern,
    scrutinee: Box<Expr>,
    body: Box<Expr>,
    label: Option<String>,
    span: Span,
  },

  For {
    pattern: Pattern,
    iterator: Box<Expr>,
    body: Box<Expr>,
    label: Option<String>,
    span: Span,
  },

  Return {
    value: Option<Box<Expr>>,
    span: Span,
  },

  Break {
    label: Option<String>,
    value: Option<Box<Expr>>,
    span: Span,
  },

  Continue {
    label: Option<String>,
    span: Span,
  },

  Yield {
    value: Option<Box<Expr>>,
    span: Span,
  },

  Become {
    expr: Box<Expr>,
    span: Span,
  },

  Closure {
    capture: CaptureKind,
    is_async: bool,
    is_move: bool,
    params: Vec<ClosureParam>,
    return_type: Option<Type>,
    body: Box<Expr>,
    span: Span,
  },

  // Unified flavored block, matches blockExpr / asyncBlockExpr / unsafeBlockExpr / tryBlockExpr
  Block {
    outer_attributes: Vec<Attribute>,
    inner_attributes: Vec<Attribute>,
    stmts: Vec<Stmt>,
    label: Option<String>,
    flavor: BlockFlavor,
    span: Span,
  },

  Await {
    expr: Box<Expr>,
    span: Span,
  },

  Try {
    expr: Box<Expr>,
    span: Span,
  },

  Cast {
    expr: Box<Expr>,
    ty: Type,
    span: Span,
  },

  Type {
    expr: Box<Expr>,
    ty: Type,
    span: Span,
  },

  Let {
    pattern: Pattern,
    expr: Box<Expr>,
    span: Span,
  },

  Box {
    expr: Box<Expr>,
    span: Span,
  },

  Underscore {
    span: Span,
  },

  Macro {
    mac: MacroInvocation,
    span: Span,
  },

  Paren {
    expr: Box<Expr>,
    span: Span,
  },

  InlineAsm {
    template: String,
    operands: Vec<AsmOperand>,
    options: Vec<String>,
    span: Span,
  },

  FormatString {
    template: String,
    args: Vec<FormatArg>,
    span: Span,
  },
}

// Flavor for blockExpr / asyncBlockExpr / unsafeBlockExpr / tryBlockExpr
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BlockFlavor {
  Normal,    // "{ ... }"
  Async,     // "async { ... }"
  AsyncMove, // "async move { ... }"
  Unsafe,    // "unsafe { ... }"
  Try,       // "try { ... }" (nightly)
}

// ----------------------------------------------------------------------------
// Expression support types
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(crate) struct FormatArg {
  pub name: Option<String>,
  pub expr: Expr,
  pub format_spec: Option<FormatSpec>,
}

#[derive(Debug, Clone)]
pub(crate) struct FormatSpec {
  pub fill: Option<char>,
  pub align: Option<FormatAlign>,
  pub sign: Option<FormatSign>,
  pub alternate: bool,
  pub zero_pad: bool,
  pub width: Option<FormatCount>,
  pub precision: Option<FormatCount>,
  pub ty: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum FormatAlign {
  Left,
  Center,
  Right,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum FormatSign {
  Plus,
  Minus,
}

#[derive(Debug, Clone)]
pub(crate) enum FormatCount {
  Integer(usize),
  Argument(String),
  Asterisk,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum StrKind {
  Normal,
  Raw(usize),
  C,
  RawC(usize),
  Byte,
  RawByte(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ByteStrKind {
  Normal,
  Raw(usize),
}

#[derive(Debug, Clone)]
pub(crate) enum FieldAccess {
  Named(String),
  Unnamed(usize),
}

#[derive(Debug, Clone)]
pub(crate) struct AsmOperand {
  pub kind: AsmOperandKind,
  pub constraint: String,
  pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum AsmOperandKind {
  In,
  Out,
  InOut,
  SplitInOut,
  Const,
  Sym,
}

#[derive(Debug, Clone)]
pub(crate) struct FieldInit {
  pub attributes: Vec<Attribute>,
  pub name: String,
  pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub(crate) enum RangeKind {
  Full,
  From,
  FromInclusive,
  To,
  ToInclusive,
  Exclusive,
  Inclusive,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum CaptureKind {
  Default,
  Move,
}

#[derive(Debug, Clone)]
pub(crate) struct ClosureParam {
  pub attributes: Vec<Attribute>,
  pub pattern: Pattern,
  pub ty: Option<Type>,
}

// ----------------------------------------------------------------------------
// Binary and unary operators
// ----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  BitAnd,
  BitOr,
  BitXor,
  Shl,
  Shr,
  Eq,
  NotEq,
  Less,
  LessEq,
  Greater,
  GreaterEq,
  And,
  Or,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum UnaryOp {
  Neg,
  Not,
  Deref,
  Ref { mutable: bool, depth: usize },
}
