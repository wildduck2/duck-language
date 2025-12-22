use crate::{
  ast::expr::{Expr, ExprKind},
  match_and_consume,
  parser_utils::ExprContext,
  Parser,
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
      let found = self.get_token_lexeme(&token);
      self.emit(self.err_invalid_condition(token.span, &found));
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
      self.emit(self.err_continue_outside_loop(token.span));
      return Err(());
    }

    self.advance();

    let label = self.parse_label(false)?;

    if !matches!(
      self.current_token().kind,
      TokenKind::Colon | TokenKind::RBrace
    ) {
      let lexeme = self.get_token_lexeme(&self.current_token());
      self.emit(self.err_unexpected_token(
        self.current_token().span,
        "colon or closing brace",
        &lexeme,
      ));
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
      self.emit(self.err_break_outside_loop(token.span));
      return Err(());
    }

    self.advance();

    let label = self.parse_label(false)?;

    let value = if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::RBrace
    ) {
      Some(self.parse_expression(vec![], ExprContext::Default)?)
    } else {
      None
    };

    if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::RBrace
    ) {
      let lexeme = self.get_token_lexeme(&self.current_token());
      self.emit(self.err_unexpected_token(
        self.current_token().span,
        "semicolon or closing brace",
        &lexeme,
      ));
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
      self.emit(self.err_return_outside_function(token.span));
      return Err(());
    }

    self.advance();

    let value = if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::RBrace
    ) {
      Some(self.parse_expression(vec![], ExprContext::Default)?)
    } else {
      None
    };

    if !matches!(
      self.current_token().kind,
      TokenKind::Semi | TokenKind::RBrace
    ) {
      let lexeme = self.get_token_lexeme(&self.current_token());
      self.emit(self.err_unexpected_token(
        self.current_token().span,
        "semicolon or closing brace",
        &lexeme,
      ));
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
