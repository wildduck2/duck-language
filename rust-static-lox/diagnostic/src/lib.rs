use crate::{code::DiagnosticCode, diagnostic::Diagnostic};

pub mod code;
pub mod diagnostic;
pub mod source_map;
pub mod types;

// Re-exports for convenience
pub use source_map::{SourceFile, SourceMap, Span};

#[derive(Debug, Default)]
pub struct DiagnosticEngine {
  source: String,
  source_map: SourceMap,
  diagnostics: Vec<Diagnostic>,
  error_count: usize,
  warning_count: usize,
}

impl DiagnosticEngine {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn insert_source(&mut self, source: String) {
    self.source = source;
  }

  pub fn add_file(&mut self, path: &str, src: &str) {
    self.source_map.add_file(path, src);
  }

  pub fn add(&mut self, diagnostic: Diagnostic) {
    match diagnostic.code {
      DiagnosticCode::Error(_) => {
        self.error_count += 1;
      },
      DiagnosticCode::Warning(_) => {
        self.warning_count += 1;
      },
    }

    self.diagnostics.push(diagnostic);
  }

  pub fn print_diagnostics(&self) {
    for diagnostic in &self.diagnostics {
      let _ = diagnostic.print(&self.source_map);
    }
  }

  pub fn error_count(&self) -> usize {
    self.error_count
  }

  pub fn warning_count(&self) -> usize {
    self.warning_count
  }

  pub fn has_errors(&self) -> bool {
    self.error_count > 0
  }
  pub fn has_warnings(&self) -> bool {
    self.warning_count > 0
  }
}
