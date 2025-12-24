#![cfg(test)]

use crate::{
  ast::{
    expr::{BinaryOp, ExprKind, UnaryOp},
    path::PathSegmentKind,
    ty::Type,
    Attribute, Item, Lit, Path,
  },
  parser_utils::ParserContext,
  Parser,
};
use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
use lexer::Lexer;
use std::{cell::RefCell, path::PathBuf, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct SimplePath {
  pub(crate) leading_colon: bool,
  pub(crate) segments: Vec<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum SimpleFieldName {
  Ident(String),
  TupleIndex(usize),
}

#[derive(Debug, PartialEq)]
pub(crate) struct SimpleField {
  pub(crate) name: SimpleFieldName,
  pub(crate) value: SimpleExpr,
}

#[derive(Debug, PartialEq)]
pub(crate) enum SimpleExpr {
  Int(i128),
  Bool(bool),
  Path(SimplePath),
  Unary {
    op: UnaryOp,
    expr: Box<SimpleExpr>,
  },
  Cast {
    expr: Box<SimpleExpr>,
    ty: &'static str,
  },
  Binary {
    op: BinaryOp,
    left: Box<SimpleExpr>,
    right: Box<SimpleExpr>,
  },
  Group(Box<SimpleExpr>),
  Tuple(Vec<SimpleExpr>),
  Array {
    elements: Vec<SimpleExpr>,
    repeat: Option<Box<SimpleExpr>>,
  },
  Struct {
    path: SimplePath,
    fields: Vec<SimpleField>,
    base: Option<Box<SimpleExpr>>,
  },
  Call {
    callee: Box<SimpleExpr>,
    args: Vec<SimpleExpr>,
  },
}

pub(crate) trait BinaryExpr: Sized {
  fn binary(op: BinaryOp, left: Self, right: Self) -> Self;
}

pub(crate) fn bin<E: BinaryExpr>(op: BinaryOp, left: E, right: E) -> E {
  E::binary(op, left, right)
}

pub(crate) trait IntExpr: Sized {
  fn int(value: i128) -> Self;
}

pub(crate) fn int<E: IntExpr>(value: i128) -> E {
  E::int(value)
}

pub(crate) trait BoolExpr: Sized {
  fn boolean(value: bool) -> Self;
}

pub(crate) fn boolean<E: BoolExpr>(value: bool) -> E {
  E::boolean(value)
}

pub(crate) trait PathExpr: Sized {
  fn path(name: &'static str) -> Self;
}

pub(crate) fn path<E: PathExpr>(name: &'static str) -> E {
  E::path(name)
}

pub(crate) trait UnaryExpr: Sized {
  fn unary(op: UnaryOp, expr: Self) -> Self;
}

pub(crate) fn unary<E: UnaryExpr>(op: UnaryOp, expr: E) -> E {
  E::unary(op, expr)
}

pub(crate) trait CastExpr: Sized {
  fn cast(expr: Self, ty: &'static str) -> Self;
}

pub(crate) fn cast<E: CastExpr>(expr: E, ty: &'static str) -> E {
  E::cast(expr, ty)
}

pub(crate) trait GroupExpr: Sized {
  fn group(expr: Self) -> Self;
}

pub(crate) fn group<E: GroupExpr>(expr: E) -> E {
  E::group(expr)
}

pub(crate) trait TupleExpr: Sized {
  fn tuple(elements: Vec<Self>) -> Self;
}

pub(crate) fn tuple<E: TupleExpr>(elements: Vec<E>) -> E {
  E::tuple(elements)
}

pub(crate) trait ArrayExpr: Sized {
  fn array(elements: Vec<Self>) -> Self;
  fn repeat_array(value: Self, repeat: Self) -> Self;
}

pub(crate) fn array<E: ArrayExpr>(elements: Vec<E>) -> E {
  E::array(elements)
}

pub(crate) fn repeat_array<E: ArrayExpr>(value: E, repeat: E) -> E {
  E::repeat_array(value, repeat)
}

pub(crate) fn simple_path<I: Into<String>>(segments: impl IntoIterator<Item = I>) -> SimplePath {
  SimplePath {
    leading_colon: false,
    segments: segments.into_iter().map(Into::into).collect(),
  }
}

pub(crate) fn simple_path_leading_colon<I: Into<String>>(
  segments: impl IntoIterator<Item = I>,
) -> SimplePath {
  SimplePath {
    leading_colon: true,
    segments: segments.into_iter().map(Into::into).collect(),
  }
}

pub(crate) fn path_expr(path: SimplePath) -> SimpleExpr {
  SimpleExpr::Path(path)
}

pub(crate) fn field(name: SimpleFieldName, value: SimpleExpr) -> SimpleField {
  SimpleField { name, value }
}

pub(crate) fn struct_expr(
  path: SimplePath,
  fields: Vec<SimpleField>,
  base: Option<SimpleExpr>,
) -> SimpleExpr {
  SimpleExpr::Struct {
    path,
    fields,
    base: base.map(Box::new),
  }
}

pub(crate) fn call(callee: SimpleExpr, args: Vec<SimpleExpr>) -> SimpleExpr {
  SimpleExpr::Call {
    callee: Box::new(callee),
    args,
  }
}

pub(crate) fn simplify_path(path: &crate::ast::path::Path) -> SimplePath {
  use crate::ast::path::PathSegmentKind;

  let segments = path
    .segments
    .iter()
    .map(|segment| match &segment.kind {
      PathSegmentKind::Ident(name) => name.clone(),
      PathSegmentKind::Self_ => "self".to_string(),
      PathSegmentKind::SelfType => "Self".to_string(),
      PathSegmentKind::Super => "super".to_string(),
      PathSegmentKind::Crate => "crate".to_string(),
      PathSegmentKind::DollarCrate => "$crate".to_string(),
    })
    .collect();

  SimplePath {
    leading_colon: path.leading_colon,
    segments,
  }
}

impl IntExpr for SimpleExpr {
  fn int(value: i128) -> Self {
    SimpleExpr::Int(value)
  }
}

impl BoolExpr for SimpleExpr {
  fn boolean(value: bool) -> Self {
    SimpleExpr::Bool(value)
  }
}

impl PathExpr for SimpleExpr {
  fn path(name: &'static str) -> Self {
    path_expr(simple_path([name]))
  }
}

impl UnaryExpr for SimpleExpr {
  fn unary(op: UnaryOp, expr: Self) -> Self {
    SimpleExpr::Unary {
      op,
      expr: Box::new(expr),
    }
  }
}

impl CastExpr for SimpleExpr {
  fn cast(expr: Self, ty: &'static str) -> Self {
    SimpleExpr::Cast {
      expr: Box::new(expr),
      ty,
    }
  }
}

impl BinaryExpr for SimpleExpr {
  fn binary(op: BinaryOp, left: Self, right: Self) -> Self {
    SimpleExpr::Binary {
      op,
      left: Box::new(left),
      right: Box::new(right),
    }
  }
}

impl GroupExpr for SimpleExpr {
  fn group(expr: Self) -> Self {
    SimpleExpr::Group(Box::new(expr))
  }
}

impl TupleExpr for SimpleExpr {
  fn tuple(elements: Vec<Self>) -> Self {
    SimpleExpr::Tuple(elements)
  }
}

impl ArrayExpr for SimpleExpr {
  fn array(elements: Vec<Self>) -> Self {
    SimpleExpr::Array {
      elements,
      repeat: None,
    }
  }

  fn repeat_array(value: Self, repeat: Self) -> Self {
    SimpleExpr::Array {
      elements: vec![value],
      repeat: Some(Box::new(repeat)),
    }
  }
}

pub(crate) fn simplify_expr(expr: &ExprKind) -> SimpleExpr {
  simplify_expr_inner(expr, true)
}

pub(crate) fn simplify_expr_ungrouped(expr: &ExprKind) -> SimpleExpr {
  simplify_expr_inner(expr, false)
}

fn simplify_expr_inner(expr: &ExprKind, keep_groups: bool) -> SimpleExpr {
  match expr {
    ExprKind::Literal(Lit::Integer { value, .. }) => int(*value),
    ExprKind::Literal(Lit::Bool(value)) => boolean(*value),

    ExprKind::Path { path, .. } => path_expr(simplify_path(path)),

    ExprKind::Unary { op, expr } => unary(op.clone(), simplify_expr_inner(&expr.kind, keep_groups)),

    ExprKind::Cast { expr, ty } => cast(
      simplify_expr_inner(&expr.kind, keep_groups),
      simplify_type_name(ty),
    ),

    ExprKind::Binary { left, op, right } => bin(
      *op,
      simplify_expr_inner(&left.kind, keep_groups),
      simplify_expr_inner(&right.kind, keep_groups),
    ),

    ExprKind::Group { expr } => {
      if keep_groups {
        group(simplify_expr_inner(&expr.kind, keep_groups))
      } else {
        simplify_expr_inner(&expr.kind, keep_groups)
      }
    },

    ExprKind::Tuple { elements } => tuple(
      elements
        .iter()
        .map(|e| simplify_expr_inner(&e.kind, keep_groups))
        .collect(),
    ),

    ExprKind::Array { elements, repeat } => {
      let elements: Vec<_> = elements
        .iter()
        .map(|e| simplify_expr_inner(&e.kind, keep_groups))
        .collect();

      match repeat {
        Some(expr) => {
          let value = elements
            .into_iter()
            .next()
            .expect("repeat array should have a value element");
          repeat_array(value, simplify_expr_inner(&expr.kind, keep_groups))
        },
        None => array(elements),
      }
    },

    ExprKind::Struct { path, fields, base } => {
      let fields = fields
        .iter()
        .map(|f| {
          let name = match &f.name {
            crate::ast::expr::FieldName::Ident(n) => SimpleFieldName::Ident(n.clone()),
            crate::ast::expr::FieldName::TupleIndex(i) => SimpleFieldName::TupleIndex(*i),
          };

          let value = match f.value.as_ref() {
            Some(v) => simplify_expr_inner(&v.kind, keep_groups),
            None => match &name {
              SimpleFieldName::Ident(n) => path_expr(simple_path([n.clone()])),
              SimpleFieldName::TupleIndex(_) => {
                panic!("tuple index shorthand should not exist")
              },
            },
          };

          field(name, value)
        })
        .collect();

      struct_expr(
        simplify_path(path),
        fields,
        base
          .as_ref()
          .map(|e| simplify_expr_inner(&e.kind, keep_groups)),
      )
    },

    ExprKind::Call { callee, args } => call(
      simplify_expr_inner(&callee.kind, keep_groups),
      args
        .iter()
        .map(|a| simplify_expr_inner(&a.kind, keep_groups))
        .collect(),
    ),

    other => panic!("unexpected expression in tests: {:?}", other),
  }
}

fn simplify_type_name(ty: &Type) -> &'static str {
  match ty {
    // Builtin primitive types
    Type::I8 => "i8",
    Type::I16 => "i16",
    Type::I32 => "i32",
    Type::I64 => "i64",
    Type::I128 => "i128",
    Type::Isize => "isize",
    Type::U8 => "u8",
    Type::U16 => "u16",
    Type::U32 => "u32",
    Type::U64 => "u64",
    Type::U128 => "u128",
    Type::Usize => "usize",
    Type::F32 => "f32",
    Type::F64 => "f64",
    Type::Bool => "bool",
    Type::Char => "char",
    Type::Str => "str",

    // Path types: Foo, std::vec::Vec, etc
    Type::Path(Path { segments, .. }) => {
      let seg = &segments[0];
      match &seg.kind {
        PathSegmentKind::Ident(name) => Box::leak(name.clone().into_boxed_str()),
        _ => panic!("unexpected type segment"),
      }
    },

    other => panic!("unexpected cast type in tests: {:?}", other),
  }
}

fn test_file_path(stem: &str) -> String {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("files")
    .join(format!("{stem}.lox"))
    .to_string_lossy()
    .into_owned()
}

pub(crate) fn run_parser<F, T>(input: &str, file_stem: &str, mut parse_fn: F) -> Result<T, ()>
where
  F: FnMut(&mut Parser) -> Result<T, ()>,
{
  let engine = Rc::new(RefCell::new(DiagnosticEngine::new()));
  let mut source_map = SourceMap::new();

  let path_str = test_file_path(file_stem);
  let source_file = SourceFile::new(path_str.clone(), input.to_string());
  source_map.add_file(&path_str, input);
  engine.borrow_mut().add_file(&path_str, input);

  let mut lexer = Lexer::new(source_file.clone(), engine.clone());
  let _ = lexer.scan_tokens();

  if engine.borrow().has_errors() {
    return Err(());
  }

  let mut parser = Parser::new(lexer.tokens, source_file, engine.clone());
  let result = parse_fn(&mut parser)?;

  if engine.borrow().has_errors() {
    return Err(());
  }

  if !parser.is_eof() {
    return Err(());
  }

  Ok(result)
}

pub(crate) fn parse_primary_expr(
  input: &str,
  file_stem: &str,
  context: ParserContext,
) -> Result<ExprKind, ()> {
  run_parser(input, file_stem, |parser| {
    parser.parse_primary(context).map(|expr| expr.kind)
  })
}

pub(crate) fn parse_expression(
  input: &str,
  file_stem: &str,
  context: ParserContext,
) -> Result<ExprKind, ()> {
  run_parser(input, file_stem, |parser| {
    parser
      .parse_expression(Vec::<Attribute>::new(), context)
      .map(|expr| expr.kind)
  })
}

pub(crate) fn parse_item(input: &str, file_stem: &str, context: ParserContext) -> Result<Item, ()> {
  run_parser(input, file_stem, |parser| {
    let attributes = parser.parse_outer_attributes(context)?;
    let visibility = parser.parse_visibility(context)?;
    parser.parse_item(attributes, visibility, context)
  })
}
