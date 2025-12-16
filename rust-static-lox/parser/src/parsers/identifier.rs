// use diagnostic::DiagnosticEngine;
//
// use crate::{
//   ast::{Expr, ExprKind, PathSegment, PathSegmentKind},
//   Parser,
// };
//
// impl Parser {
//   pub(crate) fn parser_ident(&mut self, engine: &mut DiagnosticEngine) -> Result<Expr, ()> {
//     let mut token = self.current_token();
//     let name = self
//       .source_file
//       .src
//       .get(token.span.start..token.span.end)
//       .unwrap()
//       .to_string();
//
//     // consume the identifier
//     self.advance(engine);
//
//     Ok(Expr::Ident {
//       name,
//       span: *token.span.merge(self.current_token().span),
//     })
//   }
//
//   pub(crate) fn parse_keyword_ident(&mut self, engine: &mut DiagnosticEngine) -> Result<Expr, ()> {
//     let mut token = self.current_token();
//     let path = self.parse_path(true, engine)?;
//
//     // consume the keyword token
//     self.advance(engine);
//
//     Ok(Expr {
//       attributes: vec![],
//       kind: ExprKind::Path { qself: None, path },
//
//       span: *token.span.merge(self.current_token().span),
//     })
//   }
// }
