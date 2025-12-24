#![cfg(test)]

use crate::{
  ast::{expr::ExprKind, Attribute, Item},
  parser_utils::ParserContext,
  Parser,
};
use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
use lexer::Lexer;
use std::{cell::RefCell, path::PathBuf, rc::Rc};

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
