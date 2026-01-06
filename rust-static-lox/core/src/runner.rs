use std::{cell::RefCell, rc::Rc};

use diagnostic::{DiagnosticEngine, SourceMap};
use lexer::Lexer;
use parser::Parser;

pub struct Runner {}

impl Runner {
  pub fn new() -> Self {
    Self {}
  }

  pub fn run_interactive_mode(&mut self, engine: &mut DiagnosticEngine) {}

  pub fn run_file(
    &mut self,
    path: &str,
    engine: Rc<RefCell<DiagnosticEngine>>,
  ) -> Result<(), std::io::Error> {
    let mut source_map = SourceMap::new();
    source_map.add_wd(path)?;

    for source_file in source_map.files.values() {
      engine
        .borrow_mut()
        .add_file(source_file.path.as_str(), source_file.src.as_str());
      println!("\n============== READ =================\n");

      println!("{}", &source_file.src);

      println!("\n============= SCANNED ===============\n");

      let mut lexer = Lexer::new(source_file.clone(), engine.clone());
      lexer.scan_tokens()?;

      if engine.borrow().has_errors() {
        engine.borrow().print_diagnostics();
        return Err(std::io::Error::other("lexing error"));
      }

      println!("{:?}", lexer.tokens);
      println!("TooLongVecOfTokens[{}]", lexer.tokens.len());

      println!("\n============= PARSED ===============\n");

      let mut parser = Parser::new(lexer.tokens, source_file.clone(), engine.clone());
      parser.parse();

      if engine.borrow().has_errors() {
        engine.borrow().print_diagnostics();
        return Err(std::io::Error::other("parsing error"));
      }
    }

    Ok(())
  }
}
