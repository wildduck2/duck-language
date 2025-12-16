#[cfg(test)]
mod literal_tests {

  use crate::{
    ast::expr::{ExprKind, Lit},
    parser_utils::ExprContext,
    tests::prepare,
  };

  #[test]
  fn literal_expressions_cover_all_variants() {
    let (mut engine, mut parser) = prepare("./tests/files/literal.lox").unwrap();

    let mut ast = vec![];
    while !parser.is_eof() {
      match parser.parse_primary(ExprContext::Default, &mut engine) {
        Ok(item) => {
          ast.push(item);
        },
        Err(_) => parser.synchronize(&mut engine),
      }
    }

    let cases = [
      (0, ExprKind::Literal(Lit::Char('a'))),
      (1, ExprKind::Literal(Lit::Char('\n'))),
      (
        2,
        ExprKind::Literal(Lit::String {
          value: "hello".to_string(),
          raw_hashes: None,
        }),
      ),
      (
        3,
        ExprKind::Literal(Lit::String {
          value: "escaped \" quote".to_string(),
          raw_hashes: None,
        }),
      ),
      (
        4,
        ExprKind::Literal(Lit::String {
          value: r"raw string \n not escaped".to_string(),
          raw_hashes: Some(0),
        }),
      ),
      (5, ExprKind::Literal(Lit::Byte(97))),
      (
        6,
        ExprKind::Literal(Lit::ByteString {
          value: vec![98, 121, 116, 101, 32, 115, 116, 114, 105, 110, 103],
          raw_hashes: None,
        }),
      ),
      (
        7,
        ExprKind::Literal(Lit::ByteString {
          value: vec![34],
          raw_hashes: None,
        }),
      ),
      (
        8,
        ExprKind::Literal(Lit::ByteString {
          value: vec![
            114, 97, 119, 32, 98, 121, 116, 101, 32, 115, 116, 114, 105, 110, 103, 32, 92, 110, 32,
            110, 111, 116, 32, 101, 115, 99, 97, 112, 101, 100,
          ],
          raw_hashes: Some(0),
        }),
      ),
      (
        9,
        ExprKind::Literal(Lit::String {
          value: "c string".to_string(),
          raw_hashes: None,
        }),
      ),
      (
        10,
        ExprKind::Literal(Lit::String {
          value: r"raw c string \n not escaped".to_string(),
          raw_hashes: Some(0),
        }),
      ),
      (
        11,
        ExprKind::Literal(Lit::Integer {
          value: 0,
          suffix: None,
        }),
      ),
      (
        12,
        ExprKind::Literal(Lit::Integer {
          value: 42,
          suffix: None,
        }),
      ),
      (
        13,
        ExprKind::Literal(Lit::Integer {
          value: 1_000_000,
          suffix: None,
        }),
      ),
      (
        14,
        ExprKind::Literal(Lit::Integer {
          value: 255,
          suffix: None,
        }),
      ),
      (
        15,
        ExprKind::Literal(Lit::Integer {
          value: 42,
          suffix: None,
        }),
      ),
      (
        16,
        ExprKind::Literal(Lit::Integer {
          value: 493,
          suffix: None,
        }),
      ),
      (
        17,
        ExprKind::Literal(Lit::Float {
          value: (std::f64::consts::PI * 100.0).round() / 100.0,
          suffix: None,
        }),
      ),
      (
        18,
        ExprKind::Literal(Lit::Float {
          value: 0.5,
          suffix: None,
        }),
      ),
      (
        19,
        ExprKind::Literal(Lit::Float {
          value: 1e10,
          suffix: None,
        }),
      ),
      (
        20,
        ExprKind::Literal(Lit::Float {
          value: 2.5e-3,
          suffix: None,
        }),
      ),
      (21, ExprKind::Literal(Lit::Bool(true))),
      (22, ExprKind::Literal(Lit::Bool(false))),
    ];

    for case in cases {
      let (index, kind) = case;
      assert_eq!(ast[index].kind, kind);
    }
  }
}
