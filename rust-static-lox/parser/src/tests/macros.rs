#[cfg(test)]
mod macros_tests {
  use crate::{
    ast::{Delimiter, ExprKind, Item, MacroItemKind, RepeatKind, Stmt, TokenTree},
    parser_utils::ParserContext,
    tests::support::{parse_expression, parse_item, run_parser},
  };

  fn parse_expr(input: &str) -> Result<ExprKind, ()> {
    parse_expression(input, "macro_expr_test_temp", ParserContext::Default)
  }

  fn parse_stmt(input: &str) -> Result<Stmt, ()> {
    run_parser(input, "macro_stmt_test_temp", |parser| {
      parser.parse_stmt(ParserContext::Default)
    })
  }

  fn parse_item_decl(input: &str) -> Result<Item, ()> {
    parse_item(input, "macro_item_test_temp", ParserContext::Default)
  }

  #[test]
  fn parses_macro_invocation_paren() {
    let expr = parse_expr("foo!(x, 1, true)").unwrap();
    match expr {
      ExprKind::Macro { mac } => {
        assert_eq!(mac.delimiter, Delimiter::Paren);
        assert_eq!(
          mac.tokens,
          vec![
            TokenTree::Token("x".to_string()),
            TokenTree::Token("1".to_string()),
            TokenTree::Token("true".to_string()),
          ]
        );
      },
      other => panic!("expected macro expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_invocation_bracket() {
    let expr = parse_expr("foo![x]").unwrap();
    match expr {
      ExprKind::Macro { mac } => {
        assert_eq!(mac.delimiter, Delimiter::Bracket);
        assert_eq!(mac.tokens, vec![TokenTree::Token("x".to_string())]);
      },
      other => panic!("expected macro expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_invocation_brace() {
    let expr = parse_expr("foo!{x}").unwrap();
    match expr {
      ExprKind::Macro { mac } => {
        assert_eq!(mac.delimiter, Delimiter::Brace);
        assert_eq!(mac.tokens, vec![TokenTree::Token("x".to_string())]);
      },
      other => panic!("expected macro expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_nested_macro_token_trees() {
    let expr = parse_expr("foo!({x}, [y])").unwrap();
    match expr {
      ExprKind::Macro { mac } => {
        assert_eq!(mac.tokens.len(), 2);
        match &mac.tokens[0] {
          TokenTree::Delimited { delimiter, tokens } => {
            assert_eq!(*delimiter, Delimiter::Brace);
            assert_eq!(tokens, &vec![TokenTree::Token("x".to_string())]);
          },
          other => panic!("expected delimited token, got: {:?}", other),
        }
        match &mac.tokens[1] {
          TokenTree::Delimited { delimiter, tokens } => {
            assert_eq!(*delimiter, Delimiter::Bracket);
            assert_eq!(tokens, &vec![TokenTree::Token("y".to_string())]);
          },
          other => panic!("expected delimited token, got: {:?}", other),
        }
      },
      other => panic!("expected macro expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_invocation_statement() {
    let stmt = parse_stmt("foo!(x)").unwrap();
    match stmt {
      Stmt::Macro { mac, .. } => {
        assert_eq!(mac.delimiter, Delimiter::Paren);
        assert_eq!(mac.tokens, vec![TokenTree::Token("x".to_string())]);
      },
      other => panic!("expected macro statement, got: {:?}", other),
    }
  }

  #[test]
  fn rejects_missing_macro_delimiter() {
    assert!(parse_expr("foo!x").is_err());
  }

  #[test]
  fn rejects_invalid_macro_token_tree() {
    assert!(parse_expr("foo!(@)").is_err());
  }

  #[test]
  fn rejects_repeat_syntax_without_body() {
    assert!(parse_expr("foo!(.. =)").is_err());
  }

  #[test]
  #[ignore = "macro metavars are not implemented yet"]
  fn parses_macro_metavar() {
    let expr = parse_expr("foo!($x:expr)").unwrap();
    match expr {
      ExprKind::Macro { mac } => {
        assert_eq!(
          mac.tokens,
          vec![TokenTree::MetaVar {
            name: "x".to_string(),
            frag: "expr".to_string(),
          }]
        );
      },
      other => panic!("expected macro expression, got: {:?}", other),
    }
  }

  #[test]
  #[ignore = "macro repetition parsing is not implemented yet"]
  fn parses_macro_repetition() {
    let expr = parse_expr("foo!($(x),*)").unwrap();
    match expr {
      ExprKind::Macro { mac } => {
        assert_eq!(mac.tokens.len(), 1);
        match &mac.tokens[0] {
          TokenTree::Repeat {
            tokens,
            separator,
            kind,
          } => {
            assert_eq!(*kind, RepeatKind::ZeroOrMore);
            assert_eq!(separator.as_deref(), Some(","));
            assert_eq!(tokens, &vec![TokenTree::Token("x".to_string())]);
          },
          other => panic!("expected repeat token tree, got: {:?}", other),
        }
      },
      other => panic!("expected macro expression, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro_rules_item() {
    let item = parse_item_decl("macro_rules! m { () => {} }").unwrap();
    match item {
      Item::Macro(mac) => match mac.kind {
        MacroItemKind::MacroRules(_) => {},
        other => panic!("expected macro_rules item, got: {:?}", other),
      },
      other => panic!("expected macro item, got: {:?}", other),
    }
  }

  #[test]
  fn parses_macro2_item() {
    let item = parse_item_decl("macro m(x, y,) { }").unwrap();
    match item {
      Item::Macro(mac) => match mac.kind {
        MacroItemKind::Macro2(decl) => {
          assert_eq!(decl.name, "m");
          assert_eq!(decl.params, vec!["x".to_string(), "y".to_string()]);
          match decl.body {
            TokenTree::Delimited { delimiter, tokens } => {
              assert_eq!(delimiter, Delimiter::Brace);
              assert!(tokens.is_empty());
            },
            other => panic!("expected macro2 body token tree, got: {:?}", other),
          }
        },
        other => panic!("expected macro2 item, got: {:?}", other),
      },
      other => panic!("expected macro item, got: {:?}", other),
    }
  }
}
