use crate::{
  ast::expr::{Expr, ExprKind},
  match_and_consume,
  parser_utils::ExprContext,
  Parser,
};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
};
use lexer::token::TokenKind;

impl Parser {
  pub(crate) fn parse_if_expression(&mut self, context: ExprContext) -> Result<Expr, ()> {
    if self.peek(1).kind == TokenKind::KwLet {
      return self.parse_if_let_expression(context);
    }

    let mut token = self.current_token();
    self.advance(); // consume the "if"
    let condition = self.parse_expression(vec![], context)?;

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

      self.engine.borrow_mut().add(diag);
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
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parse_if_let_expression(&mut self, context: ExprContext) -> Result<Expr, ()> {
    let mut token = self.current_token();
    self.advance(); // consume "if"
    self.expect(TokenKind::KwLet)?;

    let pattern = self.parse_pattern(context)?;
    self.expect(TokenKind::Eq)?;
    let scrutinee = self.parse_expression(vec![], context)?;

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
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parse_continue_expression(&mut self, context: ExprContext) -> Result<Expr, ()> {
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

      self.engine.borrow_mut().add(diagnostic);
      return Err(());
    }

    self.advance();

    let label = self.parse_label(false)?;

    if !matches!(
      self.current_token().kind,
      TokenKind::Colon | TokenKind::CloseBrace
    ) {
      let lexeme = self.get_token_lexeme(&self.current_token());
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
        "expected a label to be followed by a colon or closing brace".to_string(),
        self.source_file.path.clone(),
      )
      .with_label(
        self.current_token().span,
        Some(format!(
          "expected a label to be followed by a colon or closing brace, found `{lexeme}`"
        )),
        LabelStyle::Primary,
      )
      .with_help("labels must be followed by a colon or closing brace".to_string());

      self.engine.borrow_mut().add(diagnostic);
      return Err(());
    }

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Continue { label },
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parse_break_expression(&mut self, context: ExprContext) -> Result<Expr, ()> {
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

      self.engine.borrow_mut().add(diagnostic);
      return Err(());
    }

    self.advance();

    let label = self.parse_label(false)?;

    let value = if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::CloseBrace
    ) {
      Some(self.parse_expression(vec![], ExprContext::Default)?)
    } else {
      None
    };

    if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::CloseBrace
    ) {
      let lexeme = self.get_token_lexeme(&self.current_token());
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
        "expected a label to be followed by a colon or closing brace".to_string(),
        self.source_file.path.clone(),
      )
      .with_label(
        self.current_token().span,
        Some(format!(
          "expected a label to be followed by a colon or closing brace, found `{lexeme}`"
        )),
        LabelStyle::Primary,
      )
      .with_help("labels must be followed by a colon or closing brace".to_string());

      self.engine.borrow_mut().add(diagnostic);
      return Err(());
    }

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Break {
        value: value.map(Box::new),
        label,
      },
      span: *token.span.merge(self.current_token().span),
    })
  }

  pub(crate) fn parse_return_expression(&mut self, context: ExprContext) -> Result<Expr, ()> {
    let mut token = self.current_token();

    let allowed = matches!(
      context,
      ExprContext::Function
        | ExprContext::IfCondition
        | ExprContext::Match
        | ExprContext::LetElse
        | ExprContext::Block
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

      self.engine.borrow_mut().add(diagnostic);
      return Err(());
    }

    self.advance();

    let value = if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::CloseBrace
    ) {
      Some(self.parse_expression(vec![], ExprContext::Default)?)
    } else {
      None
    };

    if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::CloseBrace
    ) {
      let lexeme = self.get_token_lexeme(&self.current_token());
      let diagnostic = Diagnostic::new(
        DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
        "expected a label to be followed by a colon or closing brace".to_string(),
        self.source_file.path.clone(),
      )
      .with_label(
        self.current_token().span,
        Some(format!(
          "expected a label to be followed by a colon or closing brace, found `{lexeme}`"
        )),
        LabelStyle::Primary,
      )
      .with_help("labels must be followed by a colon or closing brace".to_string());
      self.engine.borrow_mut().add(diagnostic);
      return Err(());
    }

    Ok(Expr {
      attributes: vec![],
      kind: ExprKind::Return {
        value: value.map(Box::new),
      },
      span: *token.span.merge(self.current_token().span),
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
      | ExprKind::TupleStruct { .. }
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
    }
    Ok(Some(self.get_token_lexeme(&token)))
  }
}
