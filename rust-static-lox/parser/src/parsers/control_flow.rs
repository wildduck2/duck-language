use crate::{ast::Expr, match_and_consume, parser_utils::ExprContext, Parser};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_if_expression(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.advance(engine); // consume the "if"
    let condition = self.parse_expression(vec![], context, engine)?;

    if !self.is_valid_condition(&condition) {
      token.span.merge(self.current_token().span);
      let diag = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::InvalidCondition),
        "invalid condition expression".to_string(),
        self.source_file.path.clone(),
      )
      .with_label(
        token.span,
        Some("this expression cannot be used as a condition".to_string()),
        LabelStyle::Primary,
      )
      .with_help("a condition must be a normal expression that evaluates to a value".to_string());

      engine.add(diag);
      return Err(());
    }

    let then_branch = self.parse_expression(vec![], context, engine)?;

    let mut else_branch = None;
    if match_and_consume!(self, engine, TokenKind::KwElse)? {
      else_branch = Some(self.parse_expression(vec![], context, engine)?);
    }

    token.span.merge(self.current_token().span);
    Ok(Expr::If {
      condition: Box::new(condition),
      then_branch: Box::new(then_branch),
      else_branch: else_branch.map(Box::new),
      span: token.span,
    })
  }

  pub(crate) fn parse_continue_expression(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();

    let allowed = matches!(
      context,
      ExprContext::LoopCondition | ExprContext::WhileCondition
    );

    if !allowed {
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::ContinueOutsideLoop),
        "continue used outside a loop".to_string(),
        self.source_file.path.clone(),
      )
      .with_label(
        token.span,
        Some("continue is not valid here".to_string()),
        LabelStyle::Primary,
      )
      .with_help(
        "a continue expression can only appear inside a for or while loop body. \
       it skips directly to the next iteration of that loop."
          .to_string(),
      )
      .with_note(
        "if you intended to restart a loop, you may need to wrap this code in a loop \
       or restructure the control flow."
          .to_string(),
      );

      engine.add(diagnostic);
      return Err(());
    }

    self.advance(engine);

    let label = self.parse_label(engine)?;

    token.span.merge(self.current_token().span);

    Ok(Expr::Continue {
      label,
      span: token.span,
    })
  }

  pub(crate) fn parse_break_expression(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();

    let allowed = matches!(
      context,
      ExprContext::LoopCondition | ExprContext::WhileCondition
    );

    if !allowed {
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::BreakOutsideLoop),
        "break used outside a loop".to_string(),
        self.source_file.path.clone(),
      )
      .with_label(
        token.span,
        Some("cannot break here".to_string()),
        LabelStyle::Primary,
      )
      .with_help(
        "a break expression can only appear inside a for or while loop body. \
       it exits the nearest enclosing loop."
          .to_string(),
      )
      .with_note(
        "break inside an if, match, or block expression is valid as long as \
       the block is inside a loop. here there is no loop to break from."
          .to_string(),
      );

      engine.add(diagnostic);
      return Err(());
    }

    self.advance(engine);

    let label = self.parse_label(engine)?;

    let value = if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::CloseBrace
    ) {
      Some(self.parse_expression(vec![], ExprContext::Default, engine)?)
    } else {
      None
    };

    token.span.merge(self.current_token().span);

    Ok(Expr::Break {
      value: value.map(Box::new),
      label,
      span: token.span,
    })
  }

  pub(crate) fn parse_return_expression(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let mut token = self.current_token();

    let allowed = matches!(
      context,
      ExprContext::Function | ExprContext::IfCondition | ExprContext::Match | ExprContext::LetElse
    );

    if !allowed {
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::ReturnOutsideFunction),
        "return used outside a function".to_string(),
        self.source_file.path.clone(),
      )
      .with_label(
        token.span,
        Some("return is not valid here".to_string()),
        LabelStyle::Primary,
      )
      .with_help(
        "a return expression is only valid inside a function or closure body. \
       consider removing it or wrapping this code inside a function."
          .to_string(),
      )
      .with_note(
        "return inside if, match, or block expressions is valid only when \
       those constructs appear inside a function or closure."
          .to_string(),
      );

      engine.add(diagnostic);
      return Err(());
    }

    self.advance(engine);

    let value = if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::CloseBrace
    ) {
      Some(self.parse_expression(vec![], ExprContext::Default, engine)?)
    } else {
      None
    };

    token.span.merge(self.current_token().span);

    Ok(Expr::Return {
      value: value.map(Box::new),
      span: token.span,
    })
  }

  fn is_valid_condition(&self, expr: &Expr) -> bool {
    use Expr::*;

    match expr {
      Integer { .. }
      | Float { .. }
      | String { .. }
      | Char { .. }
      | ByteString { .. }
      | Byte { .. }
      | Bool { .. }
      | Ident { .. }
      | Path(_)
      | Binary { .. }
      | Unary { .. }
      | Group { .. }
      | Tuple { .. }
      | Field { .. }
      | MethodCall { .. }
      | Call { .. }
      | Index { .. }
      | Let { .. } => true,
      // TODO: check these later for correctness and more cases
      _ => false,
    }
  }

  pub(crate) fn parse_label(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Option<String>, ()> {
    let token = self.current_token();
    if !match_and_consume!(self, engine, TokenKind::Lifetime { .. })? {
      return Ok(None);
    }

    self.expect(TokenKind::Colon, engine)?;
    Ok(Some(self.get_token_lexeme(&token)))
  }
}
