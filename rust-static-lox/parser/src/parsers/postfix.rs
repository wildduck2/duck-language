use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::{LiteralKind, TokenKind};

use crate::{
  ast::{Expr, ExprKind, FieldAccess},
  parser_utils::ExprContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_postfix(&mut self, context: ExprContext) -> Result<Expr, ()> {
    let expr = self.parse_primary(context)?;
    let expr = self.parse_postfix_chain(expr, context)?;
    self.parse_try_suffix(expr)
  }

  /// Continues parsing postfix operators for an already parsed expression.
  pub(crate) fn parse_postfix_chain(
    &mut self,
    mut expr: Expr,
    context: ExprContext,
  ) -> Result<Expr, ()> {
    loop {
      match self.current_token().kind {
        TokenKind::LParen => {
          expr = self.parse_call(context, expr)?;
        },
        TokenKind::Dot => {
          if self.peek(1).kind == TokenKind::KwAwait {
            expr = self.parse_await(expr)?;
          } else {
            expr = self.parse_field_or_method(context, expr)?;
          }
        },
        TokenKind::LBracket => {
          expr = self.parse_index(context, expr)?;
        },
        _ => break,
      }
    }

    Ok(expr)
  }

  fn parse_try_suffix(&mut self, mut expr: Expr) -> Result<Expr, ()> {
    while matches!(self.current_token().kind, TokenKind::Question) {
      expr = self.parse_try(expr)?;
    }
    Ok(expr)
  }

  pub(crate) fn parse_await(&mut self, expr: Expr) -> Result<Expr, ()> {
    self.expect(TokenKind::Dot)?;
    let mut token = self.current_token();
    self.advance(); // consume `await`

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Await {
        expr: Box::new(expr),
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_try(&mut self, expr: Expr) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.expect(TokenKind::Question)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Try {
        expr: Box::new(expr),
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_index(&mut self, context: ExprContext, object: Expr) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.expect(TokenKind::LBracket)?;
    let index = self.parse_expression(vec![], context)?;
    self.expect(TokenKind::RBracket)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Index {
        object: Box::new(object),
        index: Box::new(index),
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_field_or_method(&mut self, context: ExprContext, object: Expr) -> Result<Expr, ()> {
    self.expect(TokenKind::Dot)?;
    let mut token = self.current_token();

    match &token.kind {
      // Named field or method access
      TokenKind::Ident => {
        let name = self.get_token_lexeme(&token);
        self.advance(); // consume identifier

        // `.method(args)`
        if self.current_token().kind == TokenKind::LParen {
          self.expect(TokenKind::LParen)?;
          let args = self.parse_call_params(context)?;
          self.expect(TokenKind::RParen)?;

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
        self.advance();

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
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("invalid token `{lexeme}` after `.`"),
          )
          .with_label(
            token.span,
            Some("expected an identifier, tuple index, or method call target".to_string()),
            LabelStyle::Primary,
          )
          .with_help("Examples: `.foo`, `.0`, `.await`, or `.method(args)`.".to_string());
        self.emit(diagnostic);
        Err(())
      },
    }
  }

  fn parse_call(&mut self, context: ExprContext, callee: Expr) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.expect(TokenKind::LParen)?;
    let args = self.parse_call_params(context)?;
    self.expect(TokenKind::RParen)?;

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Call {
        callee: Box::new(callee),
        args,
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  fn parse_call_params(&mut self, context: ExprContext) -> Result<Vec<Expr>, ()> {
    let mut args = vec![];

    while !self.is_eof() && self.current_token().kind != TokenKind::RParen {
      let expr = self.parse_expression(vec![], context)?;
      args.push(expr);

      match self.check_comma_with_trailing(true)? {
        true => continue,
        false => break,
      }
    }

    Ok(args)
  }
}
