use std::{cell::RefCell, rc::Rc};

use diagnostic::{
  code::DiagnosticCode, diagnostic::Diagnostic, types::error::DiagnosticError, DiagnosticEngine,
};

use crate::runner::Runner;
use colored::*;

mod runner;

fn main() -> Result<(), std::io::Error> {
  let args = std::env::args().collect::<Vec<_>>();
  let mut runner = Runner::new();

  let diagnostic_engine = Rc::new(RefCell::new(DiagnosticEngine::new()));

  match args.len() {
    1 => {
      println!("Running the interactive mode");
      // runner.run_interactive_mode(&mut diagnostic_engine);
    },
    2 => {
      println!("{}", format!("Running file: {}", args[1]).cyan().bold());
      match runner.run_file(args[1].as_str(), diagnostic_engine.clone()) {
        Ok(_) => {
          println!("\n{}", "Compiled Successfully [0] ".green().bold());
        },
        Err(e) => {
          println!("{}", e);
          std::process::exit(64);
        },
      };
    },
    _ => {
      // Error: Invalid arguments
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::InvalidArguments),
        "invalid number of arguments".to_string(),
        "demo.lox".to_string(),
      )
      .with_help("Usage: lox [script]".to_string());

      diagnostic_engine.borrow_mut().add(diagnostic);
      diagnostic_engine.borrow().print_diagnostics();
      std::process::exit(64);
    },
  }

  // Check if compilation had errors
  if diagnostic_engine.borrow().has_errors() {
    std::process::exit(65);
  }

  Ok(())
}
