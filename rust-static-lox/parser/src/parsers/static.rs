use crate::{
  ast::{
    attrs::Attribute,
    items::{Item, StaticDecl, VisItem, VisItemKind},
    visibility::Visibility,
  },
  parser_utils::ExprContext,
  Parser,
};
use diagnostic::DiagnosticEngine;
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_static_decl(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    engine: &mut DiagnosticEngine,
  ) -> Result<Item, ()> {
    let mut token = self.current_token();
    self.advance(engine); // consume the "static"

    let mutability = self.parse_mutability(engine)?;
    let name = self.parse_name(false, engine)?;
    let ty = self.parse_type(engine)?;
    let value = if matches!(self.current_token().kind, TokenKind::Eq) {
      self.advance(engine); // consume '='
      Some(self.parse_expression(vec![], ExprContext::Default, engine)?)
    } else {
      None
    };

    Err(())

    // Ok(Item::Vis(VisItem {
    //   attributes,
    //   visibility,
    //   kind: VisItemKind::Static(StaticDecl {
    //     name,
    //     ty,
    //     mutability,
    //     value,
    //   }),
    //   span: *token.span.merge(self.current_token().span),
    // }))
  }
}
