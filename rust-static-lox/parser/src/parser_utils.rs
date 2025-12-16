use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
  DiagnosticEngine,
};
use lexer::token::TokenKind;

use crate::{ast::*, Parser};

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
      TokenKind::KwType => self.parse_type_alias_decl(attributes, visibility, engine),
      TokenKind::KwStatic => self.parse_static_decl(attributes, visibility, engine),
      // TokenKind::KwConst => self.parse_const_decl(attributes, visibility, engine),
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
    let mut token = self.current_token();
    let outer_attributes = self.parse_outer_attributes(engine)?;
    let visibility = self.parse_visibility(engine)?;

    match self.current_token().kind {
      TokenKind::Semi => {
        // Empty statement: just a semicolon
        self.advance(engine);
        Ok(Stmt::Empty {
          span: *token.span.merge(self.current_token().span),
        })
      },
      // let declaration
      TokenKind::KwLet => self.parse_let_statement(context, outer_attributes, engine),

      // Hnaldle macro invocation
      TokenKind::Ident if matches!(self.peek(1).kind, TokenKind::Bang) => {
        self.parse_macro_invocation_statement(engine)
      },
      TokenKind::Dollar | TokenKind::KwCrate | TokenKind::Lt
        if !matches!(
          self.peek(1).kind,
          TokenKind::ColonColon
            | TokenKind::Ident
            | TokenKind::KwSelfType
            | TokenKind::Dollar
            | TokenKind::KwCrate
            | TokenKind::KwSuper
        ) =>
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
    let mut token = self.current_token();
    let expr = self.parse_expression(outer_attributes, context, engine)?;
    let has_semi = matches!(self.current_token().kind, TokenKind::Semi);

    Ok(Stmt::Expr {
      expr,
      has_semi,
      span: *token.span.merge(self.current_token().span),
    })
  }

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
      TokenKind::KwReturn => self.parse_return_expression(context, engine),
      TokenKind::KwLoop => self.parse_loop_expression(label, outer_attributes, engine),
      TokenKind::KwWhile => self.parse_while_expression(label, outer_attributes, engine),
      TokenKind::KwFor => self.parse_for_expression(label, outer_attributes, engine),
      _ => self.parse_assignment_expr(context, engine),
    }
  }

  pub(crate) fn parse_primary(
    &mut self,
    _context: ExprContext,
    engine: &mut DiagnosticEngine,
  ) -> Result<Expr, ()> {
    let token = self.current_token();

    match token.kind {
      // --------------------------------------------------
      // literals
      // --------------------------------------------------
      TokenKind::Literal { kind } => self.parser_literal(kind, engine),

      TokenKind::KwTrue | TokenKind::KwFalse => self.parser_bool(engine),

      // --------------------------------------------------
      // macro invocation expression
      // --------------------------------------------------
      TokenKind::Ident if matches!(self.peek(1).kind, TokenKind::Bang) => {
        self.parse_macro_invocation_expression(engine)
      },

      // --------------------------------------------------
      // path expressions
      // --------------------------------------------------
      TokenKind::Lt => self.parse_qualified_path(engine),

      TokenKind::Dollar if matches!(self.peek(1).kind, TokenKind::KwCrate) => {
        self.parse_path_expr(true, engine)
      },

      TokenKind::ColonColon => self.parse_path_expr(true, engine),

      TokenKind::Ident
      | TokenKind::KwSelf
      | TokenKind::KwSuper
      | TokenKind::KwCrate
      | TokenKind::KwSelfType => self.parse_path_expr(true, engine),

      // --------------------------------------------------
      // grouped and tuple expressions
      // --------------------------------------------------
      TokenKind::OpenParen => self.parse_grouped_and_tuple_expr(engine),

      // --------------------------------------------------
      // array expression
      // --------------------------------------------------
      TokenKind::OpenBracket => self.parse_array_expr(engine),

      // --------------------------------------------------
      // error
      // --------------------------------------------------
      _ => {
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
}
