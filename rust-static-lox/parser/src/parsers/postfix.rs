use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
};
use lexer::token::{LiteralKind, TokenKind};

use crate::{
  ast::{Expr, ExprKind, FieldAccess},
  parser_utils::ExprContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_postfix(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let expr = self.parse_primary(context, engine)?;

    self.parse_postfix_chain(expr, context, engine)
  }

  /// Continues parsing postfix operators for an already parsed expression.
  pub(crate) fn parse_postfix_chain(
    &mut self,
    mut expr: Expr,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    loop {
      match self.current_token().kind {
        TokenKind::OpenParen => {
          expr = self.parse_call(context, expr, engine)?;
        },
        TokenKind::Dot => {
          if self.peek(1).kind == TokenKind::KwAwait {
            expr = self.parse_await(expr, engine)?;
          } else {
            expr = self.parse_field_or_method(context, expr, engine)?;
          }
        },
        TokenKind::OpenBracket => {
          expr = self.parse_index(context, expr, engine)?;
        },
        TokenKind::Question => {
          expr = self.parse_try(expr, engine)?;
        },
        _ => break,
      }
    }

    Ok(expr)
  }

  pub(crate) fn parse_await(
    &mut self,
    expr: Expr,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    self.expect(TokenKind::Dot, engine)?;
    let mut token = self.current_token();
    self.advance(engine); // consume `await`

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Await {
        expr: Box::new(expr),
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_try(&mut self, expr: Expr, engine: &mut DiagnosticEngine) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.expect(TokenKind::Question, engine)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Try {
        expr: Box::new(expr),
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_index(
    &mut self,
    context: ExprContext,
    object: Expr,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.expect(TokenKind::OpenBracket, engine)?;
    let index = self.parse_expression(vec![], context, engine)?;
    self.expect(TokenKind::CloseBracket, engine)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Index {
        object: Box::new(object),
        index: Box::new(index),
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_field_or_method(
    &mut self,
    context: ExprContext,
    object: Expr,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    self.expect(TokenKind::Dot, engine)?;
    let mut token = self.current_token();

    match &token.kind {
      // Named field or method access
      TokenKind::Ident => {
        let name = self.get_token_lexeme(&token);
        self.advance(engine); // consume identifier

        // `.method(args)`
        if self.current_token().kind == TokenKind::OpenParen {
          self.expect(TokenKind::OpenParen, engine)?;
          let args = self.parse_call_params(context, engine)?;
          self.expect(TokenKind::CloseParen, engine)?;

          return Ok(Expr {
            attributes: vec![],
            kind: ExprKind::MethodCall {
              receiver: Box::new(object),
              method: name,
              turbofish: None,
              args,
            },
            span: *token.span.merge(self.current_token().span),
          });
        }

        // `.field`
        Ok(Expr {
          attributes: vec![],
          kind: ExprKind::Field {
            object: Box::new(object),
            field: FieldAccess::Named(name),
          },
          span: *token.span.merge(self.current_token().span),
        })
      },

      // Tuple field access: `.0`, `.1`, etc.
      TokenKind::Literal {
        kind: LiteralKind::Integer { .. },
      } => {
        let value_str = self.get_token_lexeme(&token);
        let index = value_str.parse::<usize>().unwrap_or(0);
        self.advance(engine);

        Ok(Expr {
          attributes: vec![],
          kind: ExprKind::Field {
            object: Box::new(object),
            field: FieldAccess::Unnamed(index),
          },
          span: *token.span.merge(self.current_token().span),
        })
      },

      // Invalid token after `.`
      _ => {
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          format!("invalid token `{lexeme}` after `.`"),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some("expected an identifier, tuple index, or method call target".to_string()),
          LabelStyle::Primary,
        )
        .with_help("Examples: `.foo`, `.0`, `.await`, or `.method(args)`.".to_string());
        engine.add(diagnostic);
        Err(())
      },
    }
  }

  fn parse_call(
    &mut self,
    context: ExprContext,
    callee: Expr,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.expect(TokenKind::OpenParen, engine)?;
    let args = self.parse_call_params(context, engine)?;
    self.expect(TokenKind::CloseParen, engine)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Call {
        callee: Box::new(callee),
        args,
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_call_params(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<Expr>, ()> {
    let mut args = vec![];

    while !self.is_eof() && self.current_token().kind != TokenKind::CloseParen {
      let expr = self.parse_expression(vec![], context, engine)?;
      args.push(expr);

      if matches!(self.current_token().kind, TokenKind::Comma) {
        self.advance(engine); // consume comma
      } else {
        break;
      }
    }

    Ok(args)
  }
}
