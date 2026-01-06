use crate::{
  ast::expr::{Expr, ExprKind},
  match_and_consume,
  parser_utils::ParserContext,
  Parser,
};
use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_if_expression(&mut self, context: ParserContext) -> Result<Expr, ()> {
    if self.peek(1).kind == TokenKind::KwLet {
      return self.parse_if_let_expression(context);
    }

    let mut token = self.current_token();
    self.advance(); // consume the "if"
    let condition = self.parse_expression(vec![], ParserContext::IfCondition)?;

    if !matches!(self.current_token().kind, TokenKind::LBrace) {
      let found = self.get_token_lexeme(&self.current_token());
      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          format!("expected block, found `{found}`"),
        )
        .with_label(
          self.current_token().span,
          Some("expected a block `{ ... }` here".to_string()),
          LabelStyle::Primary,
        )
        .with_note(format!("unexpected token: `{found}`"))
        .with_help("add a block after the `if` condition".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    if !self.is_valid_condition(&condition) {
      token.span.merge(self.last_token_span());
      let found = self.get_token_lexeme(&token);
      let diagnostic = self
        .diagnostic(
          DiagnosticError::InvalidCondition,
          format!("expected boolean expression, found `{found}`"),
        )
        .with_label(
          token.span,
          Some("expected a condition that evaluates to a boolean".to_string()),
          LabelStyle::Primary,
        )
        .with_help(
          "conditions in `if`, `while`, and `match` expressions must be boolean".to_string(),
        );
      self.emit(diagnostic);
      return Err(());
    }

    let then_branch = self.parse_expression(vec![], context)?;

    let mut else_branch = None;
    if match_and_consume!(self, TokenKind::KwElse)? {
      else_branch = Some(self.parse_expression(vec![], context)?);
    }

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::If {
        condition: Box::new(condition),
        then_branch: Box::new(then_branch),
        else_branch: else_branch.map(Box::new),
      },
      span: *token.span.merge(self.last_token_span()),
    })
  }

  pub(crate) fn parse_if_let_expression(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.advance(); // consume "if"
    self.expect(TokenKind::KwLet)?;

    let pattern = self.parse_pattern(context)?;
    self.expect(TokenKind::Eq)?;
    let scrutinee = self.parse_expression(vec![], ParserContext::IfCondition)?;

    let then_branch = self.parse_expression(vec![], context)?;

    let mut else_branch = None;
    if matches!(self.current_token().kind, TokenKind::KwElse) {
      self.advance(); // consume "else"
      else_branch = Some(self.parse_expression(vec![], context)?);
    }

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::IfLet {
        scrutinee: Box::new(scrutinee),
        pattern,
        then_branch: Box::new(then_branch),
        else_branch: else_branch.map(Box::new),
      },
      span: *token.span.merge(self.last_token_span()),
    })
  }

  pub(crate) fn parse_continue_expression(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let mut token = self.current_token();

    let allowed = matches!(
      context,
      ParserContext::LoopCondition | ParserContext::WhileCondition
    );

    if !allowed {
      let diagnostic = self
        .diagnostic(
          DiagnosticError::ContinueOutsideLoop,
          "`continue` statement outside of loop",
        )
        .with_label(
          token.span,
          Some("`continue` can only be used inside a loop body".to_string()),
          LabelStyle::Primary,
        )
        .with_help("remove this `continue` statement or move it inside a loop".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    self.advance();

    let label = self.parse_label(false)?;

    if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::RBrace
    ) {
      let lexeme = self.get_token_lexeme(&self.current_token());
      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          format!("expected `;` or `}}`, found `{lexeme}`"),
        )
        .with_label(
          self.current_token().span,
          Some("expected `;` or `}` here".to_string()),
          LabelStyle::Primary,
        )
        .with_note(format!("unexpected token: `{lexeme}`"))
        .with_help("end the `continue` statement or close the block".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Continue { label },
      span: *token.span.merge(self.last_token_span()),
    })
  }

  pub(crate) fn parse_break_expression(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let mut token = self.current_token();

    self.advance();

    let label = self.parse_label(false)?;
    let allowed = matches!(
      context,
      ParserContext::LoopCondition | ParserContext::WhileCondition
    ) || (matches!(context, ParserContext::Block) && label.is_some());

    if !allowed {
      let diagnostic = self
        .diagnostic(
          DiagnosticError::BreakOutsideLoop,
          "`break` statement outside of loop or labeled block",
        )
        .with_label(
          token.span,
          Some("`break` can only be used inside a loop body or labeled block".to_string()),
          LabelStyle::Primary,
        )
        .with_help(
          "remove this `break` statement or move it inside a loop or labeled block".to_string(),
        );
      self.emit(diagnostic);
      return Err(());
    }

    let value = if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::RBrace
    ) {
      Some(self.parse_expression(vec![], ParserContext::Default)?)
    } else {
      None
    };

    if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::RBrace
    ) {
      let lexeme = self.get_token_lexeme(&self.current_token());
      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          format!("expected `;` or `}}`, found `{lexeme}`"),
        )
        .with_label(
          self.current_token().span,
          Some("expected `;` or `}` here".to_string()),
          LabelStyle::Primary,
        )
        .with_note(format!("unexpected token: `{lexeme}`"))
        .with_help("end the `break` statement or close the block".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Break {
        value: value.map(Box::new),
        label,
      },
      span: *token.span.merge(self.last_token_span()),
    })
  }

  pub(crate) fn parse_return_expression(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let mut token = self.current_token();

    let allowed = matches!(
      context,
      ParserContext::Function
        | ParserContext::IfCondition
        | ParserContext::Match
        | ParserContext::LetElse
        | ParserContext::Block
    );

    if !allowed {
      let diagnostic = self
        .diagnostic(
          DiagnosticError::ReturnOutsideFunction,
          "`return` statement outside of function",
        )
        .with_label(
          token.span,
          Some("`return` can only be used inside a function body".to_string()),
          LabelStyle::Primary,
        )
        .with_help("remove this `return` statement or move it inside a function".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    self.advance();

    let value = if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::RBrace
    ) {
      Some(self.parse_expression(vec![], ParserContext::Default)?)
    } else {
      None
    };

    if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::RBrace
    ) {
      let lexeme = self.get_token_lexeme(&self.current_token());
      let diagnostic = self
        .diagnostic(
          DiagnosticError::UnexpectedToken,
          format!("expected `;` or `}}`, found `{lexeme}`"),
        )
        .with_label(
          self.current_token().span,
          Some("expected `;` or `}` here".to_string()),
          LabelStyle::Primary,
        )
        .with_note(format!("unexpected token: `{lexeme}`"))
        .with_help("end the `return` statement or close the block".to_string());
      self.emit(diagnostic);
      return Err(());
    }

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Return {
        value: value.map(Box::new),
      },
      span: *token.span.merge(self.last_token_span()),
    })
  }

  fn is_valid_condition(&self, expr: &Expr) -> bool {
    match &expr.kind {
      ExprKind::Literal(_)
      | ExprKind::Path { .. }
      | ExprKind::Group { .. }
      | ExprKind::Tuple { .. }
      | ExprKind::Array { .. }
      | ExprKind::Call { .. }
      | ExprKind::MethodCall { .. }
      | ExprKind::Field { .. }
      | ExprKind::Index { .. }
      | ExprKind::Unary { .. }
      | ExprKind::Binary { .. }
      | ExprKind::Cast { .. } => true,

      // let expressions are only valid in if let / while let,
      // but keeping them allowed here matches rustc parsing stage
      ExprKind::IfLet { .. } => true,

      // everything else is not a valid boolean condition
      ExprKind::Struct { .. }
      | ExprKind::Assign { .. }
      | ExprKind::AssignOp { .. }
      | ExprKind::Range { .. }
      | ExprKind::Block { .. }
      | ExprKind::If { .. }
      | ExprKind::Match { .. }
      | ExprKind::Loop { .. }
      | ExprKind::While { .. }
      | ExprKind::WhileLet { .. }
      | ExprKind::For { .. }
      | ExprKind::Break { .. }
      | ExprKind::Continue { .. }
      | ExprKind::Return { .. }
      | ExprKind::Closure { .. }
      | ExprKind::Macro { .. }
      | ExprKind::Await { .. }
      | ExprKind::Try { .. } => false,
    }
  }

  pub(crate) fn parse_label(&mut self, start: bool) -> Result<Option<String>, ()> {
    let token = self.current_token();
    if !match_and_consume!(self, TokenKind::Lifetime { .. })? {
      return Ok(None);
    }

    if start {
      self.expect(TokenKind::Colon)?;
      if !matches!(
        self.current_token().kind,
        TokenKind::KwLoop
          | TokenKind::KwWhile
          | TokenKind::KwFor
          | TokenKind::LBrace
          | TokenKind::Semi
          | TokenKind::RBrace
      ) {
        let lexeme = self.get_token_lexeme(&self.current_token());

        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            "label must be followed by a loop or block",
          )
          .with_label(
            self.current_token().span,
            Some(format!("expected loop while for or block found {}", lexeme)),
            LabelStyle::Primary,
          )
          .with_note("labels may only be applied to loop while for or a block".to_string())
          .with_help("remove the label or wrap the expression in a block".to_string());

        self.emit(diagnostic);

        return Err(());
      }
    }

    Ok(Some(self.get_token_lexeme(&token)))
  }
}
