use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
};
use lexer::token::{Token, TokenKind};

use crate::{ast::*, match_and_consume, Parser};

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ExprContext {
  Default,
  IfCondition,
  Match,
  LetElse,
  LoopCondition,
  Struct,
  WhileCondition,
  Trait,
  Impl,
  Block,
  Function,
  Closure,
  Macro,
}

impl Parser {
  /// Parses the top-level production, collecting statements until EOF.
  /// Currently this routine prints each item tree for debugging and relies on
  /// `parse_item` to decide which constructs are supported.
  pub fn parse_program(&mut self, engine: &mut DiagnosticEngine) {
    while !self.is_eof() {
      // TODO: check this context
      match self.parse_stmt(ExprContext::Default, engine) {
        // Returns Item, not Stmt
        Ok(item) => {
          // println!("{:#?}", item);
          println!("{:#?}", item);
          // item.print_tree("", true);
          // self.ast.push(item); // ast should be Vec<Item>
        },
        Err(_) => self.synchronize(engine),
      }
    }
  }

  /// Dispatches to the correct item parser after consuming attributes & visibility.
  fn parse_item(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
    engine: &mut DiagnosticEngine,
  ) -> Result<Item, ()> {
    match self.current_token().kind {
      TokenKind::KwStruct => self.parse_struct_decl(attributes, visibility, engine),
      TokenKind::KwFn
      | TokenKind::KwConst
      | TokenKind::KwAsync
      | TokenKind::KwUnsafe
      | TokenKind::KwExtern => self.parse_fn_decl(attributes, visibility, engine),
      TokenKind::KwEnum => self.parse_enum_decl(attributes, visibility, engine),
      // TokenKind::KwConst => self.parse_const_decl(attributes, visibility, engine),
      // TokenKind::KwStatic => self.parse_static_decl(attributes, visibility, engine),
      // TokenKind::KwType => self.parse_type_alias_decl(attributes, visibility, engine),
      // TokenKind::KwMod => self.parse_module_decl(attributes, visibility, engine),
      // TokenKind::KwUse => self.parse_use_decl(attributes, visibility, engine),
      // TokenKind::KwExternCrate => self.parse_extern_crate_decl(attributes, visibility, engine),
      // TokenKind::KwMacro => self.parse_macro_decl(attributes, visibility, engine),
      // TokenKind::KwMacro2 => self.parse_macro2_decl(attributes, visibility, engine),
      // TokenKind::KwExternType => self.parse_extern_type_decl(attributes, visibility, engine),
      // TokenKind::KwUnion => self.parse_union_decl(attributes, visibility, engine),
      // TokenKind::KwExtern => self.parse_extern_decl(attributes, visibility, engine),
      kind => {
        let lexeme = self.get_token_lexeme(&self.current_token());
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          format!("unsupported item starting with `{lexeme}`"),
          self.source_file.path.clone(),
        )
        .with_label(
          self.current_token().span,
          Some("the parser currently only understands struct declarations".to_string()),
          LabelStyle::Primary,
        )
        .with_help(format!(
          "item kind `{:?}` is not implemented yet; add support in `parse_item`",
          kind
        ));
        engine.add(diagnostic);
        Err(())
      },
    }
  }

  /// Parses a single statement node (stubbed for future grammar branches).
  /// Currently supports empty statements and expression statements.
  pub(crate) fn parse_stmt(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Stmt, ()> {
    let outer_attributes = self.parse_outer_attributes(engine)?;
    let visibility = self.parse_visibility(engine)?;

    match self.current_token().kind {
      TokenKind::Semi => {
        // Empty statement: just a semicolon
        self.advance(engine);
        Ok(Stmt::Empty)
      },
      // let declaration
      TokenKind::KwLet => self.parse_let_statement(context, outer_attributes, engine),

      // Hnaldle macro invocation
      TokenKind::Ident if matches!(self.peek(1).kind, TokenKind::Bang) => {
        self.parse_macro_invocation_statement(engine)
      },
      TokenKind::Dollar | TokenKind::KwCrate | TokenKind::Lt
        if !matches!(self.peek(1).kind, TokenKind::ColonColon) =>
      {
        self.parse_macro_invocation_statement(engine)
      },

      // expression statement
      _ if self.current_token().kind.can_start_expression() && !self.can_start_fun() => {
        self.parse_expr_stmt(outer_attributes, context, engine)
      },

      // item statement
      _ => {
        let item = self.parse_item(outer_attributes, visibility, engine)?;
        Ok(Stmt::Item(Box::new(item)))
      },
    }
  }

  /// Parses an expression statement, optionally consuming a trailing semicolon.
  fn parse_expr_stmt(
    &mut self,
    outer_attributes: Vec<Attribute>,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Stmt, ()> {
    let expr = self.parse_expression(outer_attributes, context, engine)?;

    if self.current_token().kind == TokenKind::Semi {
      self.expect(TokenKind::Semi, engine)?; // check if followed by semicolon
      Ok(Stmt::Expr(expr))
    } else {
      Ok(Stmt::TailExpr(expr))
    }
  }

  /// Entry point for expression parsing. The supplied `context` controls
  /// future diagnostic wording once more productions are wired in.
  /// TODO: make sure that macros are supported in this context
  pub(crate) fn parse_expression(
    &mut self,
    outer_attributes: Vec<Attribute>,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let label = self.parse_label(engine)?;

    match self.current_token().kind {
      TokenKind::KwIf => self.parse_if_expression(ExprContext::IfCondition, engine),

      TokenKind::KwMatch => self.parse_match_expression(ExprContext::Match, engine),
      TokenKind::Or => self.parse_closure(context, engine),
      TokenKind::KwMove | TokenKind::KwAsync if self.can_start_closure() => {
        self.parse_closure(context, engine)
      },
      TokenKind::OpenBrace => {
        self.parse_block(label, ExprContext::Default, outer_attributes, engine)
      },
      TokenKind::KwAsync | TokenKind::KwUnsafe | TokenKind::KwTry
        if self.can_start_block_expression() =>
      {
        self.parse_block(label, ExprContext::Default, outer_attributes, engine)
      },
      TokenKind::KwContinue => self.parse_continue_expression(context, engine),
      TokenKind::KwBreak => self.parse_break_expression(context, engine),
      TokenKind::KwLet => self.parse_let_expression(context, engine),
      TokenKind::KwReturn => self.parse_return_expression(context, engine),
      TokenKind::KwLoop => self.parse_loop_expression(label, outer_attributes, engine),
      TokenKind::KwWhile => self.parse_while_expression(label, outer_attributes, engine),
      TokenKind::KwFor => self.parse_for_expression(label, outer_attributes, engine),
      _ => self.parse_assignment_expr(context, engine),
    }
  }

  /// Parses literals, identifiers, grouped constructs, arrays, and struct expressions.
  /// Emits a targeted diagnostic when the current token cannot start a primary expression.
  pub(crate) fn parse_primary(
    &mut self,
    context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    // FIX: this will use the context to determine whether to parse a struct expr or ident
    // if matches!(context, ExprContext::Default) {
    //   // return self.parse_struct_expr(&mut token, engine);
    // }
    match self.current_token().kind {
      // Literal handling expr
      TokenKind::Literal { kind } => self.parser_literal(kind, engine),
      TokenKind::KwFalse | TokenKind::KwTrue => self.parser_bool(engine),

      // Path handling expr
      TokenKind::Dollar if matches!(self.peek(1).kind, TokenKind::KwCrate) => {
        Ok(self.parse_path_expr(true, engine)?)
      },
      TokenKind::ColonColon => Ok(self.parse_path_expr(true, engine)?),
      TokenKind::Ident | TokenKind::KwSelf | TokenKind::KwSuper | TokenKind::KwCrate
        if matches!(
          self.peek(1).kind,
          TokenKind::ColonColon | TokenKind::OpenBrace | TokenKind::OpenParen
        ) =>
      {
        Ok(self.parse_path_expr(true, engine)?)
      },

      // Ident handling expr
      TokenKind::Ident if matches!(self.peek(1).kind, TokenKind::Bang) => {
        self.parser_macro_invocation_expression(engine)
      },

      TokenKind::Ident => self.parser_ident(engine),

      TokenKind::KwSelf | TokenKind::KwSuper | TokenKind::KwCrate | TokenKind::KwSelfType => {
        self.parse_keyword_ident(engine)
      },

      // Grouped handling expr
      TokenKind::OpenParen => self.parse_grouped_and_tuple_expr(engine),

      // Array handling expr
      TokenKind::OpenBracket => self.parse_array_expr(engine),

      _ => {
        let token = self.current_token();
        let lexeme = self.get_token_lexeme(&token);

        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          "Unexpected token".to_string(),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some(format!(
            "Expected a primary expression, found \"{}\"",
            lexeme
          )),
          LabelStyle::Primary,
        )
        .with_help(Parser::get_token_help(&token.kind, &token));

        engine.add(diagnostic);

        Err(())
      },
    }
  }

  /// Returns the substring that corresponds to `token`.
  pub(crate) fn get_token_lexeme(&mut self, token: &Token) -> String {
    self
      .source_file
      .src
      .get(token.span.start..token.span.end)
      .unwrap()
      .to_string()
  }

  /// Consumes tokens until `kind` is encountered or EOF is reached.
  /// Useful for resynchronizing after a diagnostic within delimited lists.
  pub(crate) fn advance_till_match(&mut self, engine: &mut DiagnosticEngine, kind: TokenKind) {
    while !self.is_eof() && self.current_token().kind != kind {
      self.advance(engine);
    }
  }
}
