#![cfg(test)]

use crate::{
  ast::{expr::ExprKind, Attribute},
  parser_utils::ExprContext,
  Parser,
};
use diagnostic::{DiagnosticEngine, SourceFile, SourceMap};
use lexer::Lexer;
use std::path::PathBuf;

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
  F: FnMut(&mut Parser, &mut DiagnosticEngine) -> Result<T, ()>,
{
  let mut engine = DiagnosticEngine::new();
  let mut source_map = SourceMap::new();

  let path_str = test_file_path(file_stem);
  let source_file = SourceFile::new(path_str.clone(), input.to_string());
  source_map.add_file(&path_str, input);
  engine.add_file(&path_str, input);

  let mut lexer = Lexer::new(source_file.clone());
  let _ = lexer.scan_tokens(&mut engine);

  if engine.has_errors() {
    return Err(());
  }

  let mut parser = Parser::new(lexer.tokens, source_file);
  let result = parse_fn(&mut parser, &mut engine)?;

  if engine.has_errors() {
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
  context: ExprContext,
) -> Result<ExprKind, ()> {
  run_parser(input, file_stem, |parser, engine| {
    parser.parse_primary(context, engine).map(|expr| expr.kind)
  })
}

pub(crate) fn parse_expression_expr(
  input: &str,
  file_stem: &str,
  context: ExprContext,
) -> Result<ExprKind, ()> {
  run_parser(input, file_stem, |parser, engine| {
    parser
      .parse_expression(Vec::<Attribute>::new(), context, engine)
      .map(|expr| expr.kind)
  })
}
