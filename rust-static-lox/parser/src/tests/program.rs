#[cfg(test)]
mod program_tests {
  use crate::{
    ast::{Item, VisItemKind},
    tests::support::run_parser,
  };

  fn parse_program_items(input: &str, file_stem: &str) -> Result<Vec<Item>, ()> {
    run_parser(input, file_stem, |parser| {
      parser.parse_program();
      Ok(parser.ast.clone())
    })
  }

  #[test]
  fn parses_program_with_shebang_and_item() {
    let items = parse_program_items("#!/usr/bin/env rlox\nfn foo() {}", "program_shebang")
      .unwrap();
    assert_eq!(items.len(), 1);
    match &items[0] {
      Item::Vis(vis) => match &vis.kind {
        VisItemKind::Function(_) => {},
        other => panic!("expected function item, got: {:?}", other),
      },
      other => panic!("expected visible item, got: {:?}", other),
    }
  }

  #[test]
  fn parses_program_with_inner_attributes_only() {
    let items = parse_program_items("#![no_std]", "program_inner_only").unwrap();
    assert!(items.is_empty());
  }

  #[test]
  fn program_reports_invalid_item() {
    let result = run_parser("@", "program_invalid_item", |parser| {
      parser.parse_program();
      Ok(())
    });
    assert!(result.is_err());
  }
}
