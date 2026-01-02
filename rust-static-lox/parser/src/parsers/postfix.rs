use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::{LiteralKind, TokenKind};

use crate::{
  ast::{Expr, ExprKind, FieldAccess, Path},
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};

impl Parser {
  pub(crate) fn parse_postfix(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let expr = self.parse_primary(context)?;

    let mut seen_await = false;
    let expr = self.parse_postfix_chain(expr, context, &mut seen_await)?;
    self.parse_postfix_suffix(expr, &mut seen_await)
  }

  /// Continues parsing postfix operators for an already parsed expression.
  pub(crate) fn parse_postfix_chain(
    &mut self,
    mut expr: Expr,
    context: ParserContext,
    seen_await: &mut bool,
  ) -> Result<Expr, ()> {
    loop {
      match self.current_token().kind {
        TokenKind::LParen => {
          expr = self.parse_call(context, expr)?;
        },
        TokenKind::Dot => {
          if self.peek(1).kind == TokenKind::KwAwait {
            expr = self.parse_await(expr, seen_await)?;
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

  fn parse_postfix_suffix(&mut self, mut expr: Expr, seen_await: &mut bool) -> Result<Expr, ()> {
    loop {
      match self.current_token().kind {
        TokenKind::Question => {
          expr = self.parse_try(expr)?;
        },
        TokenKind::Dot if self.peek(1).kind == TokenKind::KwAwait => {
          expr = self.parse_await(expr, seen_await)?;
        },
        _ => break,
      }
    }
    Ok(expr)
  }

  pub(crate) fn parse_await(&mut self, expr: Expr, seen_await: &mut bool) -> Result<Expr, ()> {
    self.expect(TokenKind::Dot)?;
    let mut token = self.current_token();
    self.advance(); // consume `await`

    if *seen_await {
      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          "await expression cannot be followed by another await expression".to_string(),
        )
        .with_label(
          token.span,
          Some("await expressions cannot be chained".to_string()),
          LabelStyle::Primary,
        )
        .with_help("Examples: `await foo.await`, `await await foo`".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    *seen_await = true;

    // Handle the call on await like : await()
    if matches!(self.current_token().kind, TokenKind::LParen) {
      let token = self.current_token();
      let lexeme = self.get_token_lexeme(&token);
      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          format!("unexpected token `{}`", lexeme),
        )
        .with_label(
          token.span,
          Some("Expected an identifier, tuple index, or method call target".to_string()),
          LabelStyle::Primary,
        )
        .with_help("Examples: `.foo`, `.0`, `.await`, or `.method(args)`.".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Await {
        expr: Box::new(expr),
      },
      span: *token.span.merge(self.last_token_span()),
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
      span: *token.span.merge(self.last_token_span()),
    })
  }

  fn parse_index(&mut self, context: ParserContext, object: Expr) -> Result<Expr, ()> {
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
      span: *token.span.merge(self.last_token_span()),
    })
  }

  fn parse_field_or_method(&mut self, context: ParserContext, object: Expr) -> Result<Expr, ()> {
    self.expect(TokenKind::Dot)?;
    let mut token = self.current_token();

    match &token.kind {
      // Named field or method access
      TokenKind::Ident => {
        let name = self.parse_name(false)?;

        let turbofish = if match_and_consume!(self, TokenKind::ColonColon)? {
          self.parse_generic_args(context)?
        } else {
          None
        };

        if turbofish.is_some() && self.current_token().kind != TokenKind::LParen {
          let found = self.get_token_lexeme(&self.current_token());
          let diagnostic = self
            .diagnostic(
              DiagnosticError::UnexpectedToken,
              "turbofish is only allowed on method calls",
            )
            .with_label(
              self.current_token().span,
              Some(format!("expected `(` after turbofish, found `{found}`")),
              LabelStyle::Primary,
            )
            .with_help("use `.method::<T>(...)` to apply turbofish to a method call".to_string());
          self.emit(diagnostic);
          return Err(());
        }

        // `.foo!`
        if match_and_consume!(self, TokenKind::Bang)? {
          let mac = self.parse_macro_invocation(Path::from_ident(name.as_str().to_string()))?;
          return Ok(Expr {
            attributes: vec![],
            kind: ExprKind::Macro { mac },
            span: *token.span.merge(self.last_token_span()),
          });
        }

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
              turbofish,
              args,
            },
            span: *token.span.merge(self.last_token_span()),
          });
        }

        // `.field`
        Ok(Expr {
          attributes: vec![],
          kind: ExprKind::Field {
            object: Box::new(object),
            field: FieldAccess::Named(name),
          },
          span: *token.span.merge(self.last_token_span()),
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
          span: *token.span.merge(self.last_token_span()),
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

  fn parse_call(&mut self, context: ParserContext, mut callee: Expr) -> Result<Expr, ()> {
    fn has_terminal_turbofish(expr: &Expr) -> bool {
      match &expr.kind {
        ExprKind::Group { expr } => has_terminal_turbofish(expr),
        ExprKind::Path { path, .. } => {
          if path.segments.len() != 1 {
            return false;
          }
          match path.segments.last() {
            Some(segment) => segment.args.is_some(),
            None => false,
          }
        },
        _ => false,
      }
    }

    if has_terminal_turbofish(&callee) {
      let span = *callee.span.merge(self.last_token_span());

      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          "turbofish is not allowed on a call target",
        )
        .with_label(
          span,
          Some("remove the turbofish or use it on a qualified path or method".to_string()),
          LabelStyle::Primary,
        )
        .with_help("examples: `Vec::<T>::new()` or `value.method::<T>()`".to_string());
      self.emit(diagnostic);
      return Err(());
    }

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
      span: *token.span.merge(self.last_token_span()),
    })
  }

  fn parse_call_params(&mut self, context: ParserContext) -> Result<Vec<Expr>, ()> {
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
