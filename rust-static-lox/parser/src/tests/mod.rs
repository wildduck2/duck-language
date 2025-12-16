use diagnostic::{DiagnosticEngine, SourceMap};
use lexer::Lexer;

use crate::Parser;

mod primary;

pub fn prepare(path: &str) -> Result<(DiagnosticEngine, Parser), std::io::Error> {
  let mut engine = DiagnosticEngine::new();

  let mut source_map = SourceMap::new();
  source_map.add_wd("./tests/files/")?;

  let source_file = source_map.files.get(path).unwrap();
  engine.add_file(source_file.path.as_str(), source_file.src.as_str());

  let mut lexer = Lexer::new(source_file.clone());
  lexer.scan_tokens(&mut engine);

  if engine.has_errors() {
    engine.print_diagnostics();
    return Err(std::io::Error::other("lexing error"));
  }

  let parser = Parser::new(lexer.tokens, source_file.clone());
  Ok((engine, parser))
}
