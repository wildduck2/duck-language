use crate::{ast::*, Parser};
use diagnostic::{diagnostic::LabelStyle, types::error::DiagnosticError};
use lexer::token::TokenKind;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ParserContext {
  Default,
  IfCondition,
  Match,
  LetElse,
  LoopCondition,
  ForCondition,
  WhileCondition,
  Struct,
  Block,
  Function,
  Closure,
  Trait,
  Impl,
  Extern,
  Union,
  Macro,
  Static,
  Type,
  Enum,
  Module,
}

impl Parser {
  /// Parses the top-level production, collecting statements until EOF.
  /// This handles optional shebangs, inner attributes, and a sequence of items.
  pub fn parse_program(&mut self) {
    self.ast.clear();

    if matches!(self.current_token().kind, TokenKind::Shebang) {
      self.advance();
    }

    if self.parse_inner_attributes(ParserContext::Module).is_err() {
      self.synchronize();
    }

    while !self.is_eof() {
      let attributes = match self.parse_outer_attributes(ParserContext::Module) {
        Ok(attributes) => attributes,
        Err(_) => {
          self.synchronize();
          continue;
        },
      };

      let visibility = match self.parse_visibility(ParserContext::Module) {
        Ok(visibility) => visibility,
        Err(_) => {
          self.synchronize();
          continue;
        },
      };

      match self.parse_item(attributes, visibility) {
        Ok(item) => {
          // println!("{:#?}", item);
          self.ast.push(item)
        },
        Err(_) => self.synchronize(),
      }
    }
  }

  /// Dispatches to the correct item parser after consuming attributes & visibility.
  pub fn parse_item(
    &mut self,
    attributes: Vec<Attribute>,
    visibility: Visibility,
  ) -> Result<Item, ()> {
    match self.current_token().kind {
      TokenKind::KwMacro => self.parse_macro_decl(attributes, visibility, ParserContext::Macro),
      TokenKind::KwMacroRules => {
        self.parse_macro_rules_decl(attributes, visibility, ParserContext::Macro)
      },
      TokenKind::KwUnion => self.parse_union_decl(attributes, visibility, ParserContext::Union),
      TokenKind::KwImpl | TokenKind::KwUnsafe if self.can_start_impl() => {
        self.parse_impl_decl(attributes, visibility, ParserContext::Impl)
      },
      TokenKind::KwTrait | TokenKind::KwUnsafe | TokenKind::KwAuto if self.can_start_trait() => {
        self.parse_trait_decl(attributes, visibility, ParserContext::Trait)
      },
      TokenKind::KwStruct => self.parse_struct_decl(attributes, visibility, ParserContext::Struct),
      TokenKind::KwConst if self.can_start_const_item() => {
        self.parse_const_decl(attributes, visibility, ParserContext::Default)
      },
      TokenKind::KwExtern if matches!(self.peek(1).kind, TokenKind::KwType) => {
        self.parse_extern_type_decl(attributes, visibility, ParserContext::Extern)
      },
      TokenKind::KwExtern | TokenKind::KwUnsafe if self.can_start_foreign_extern_crate() => {
        self.parse_foreign_mod_decl(attributes, visibility, ParserContext::Module)
      },
      TokenKind::KwExtern if self.can_start_extern_crate() => {
        self.parse_extern_crate_decl(attributes, visibility, ParserContext::Extern)
      },
      TokenKind::KwEnum => self.parse_enum_decl(attributes, visibility, ParserContext::Enum),
      TokenKind::KwType => self.parse_type_alias_decl(attributes, visibility, ParserContext::Type),
      TokenKind::KwStatic => self.parse_static_decl(attributes, visibility, ParserContext::Static),
      TokenKind::KwUse => self.parse_use_decl(attributes, visibility, ParserContext::Default),
      TokenKind::KwFn
      | TokenKind::KwConst
      | TokenKind::KwAsync
      | TokenKind::KwUnsafe
      | TokenKind::KwExtern
        if self.can_start_fun() =>
      {
        self.parse_fn_decl(attributes, visibility, ParserContext::Function)
      },
      TokenKind::KwMod => self.parse_module_decl(attributes, visibility, ParserContext::Module),
      kind if kind.can_start_path() => {
        self.parse_macro_invocation_item(attributes, visibility, ParserContext::Macro)
      },
      _ => {
        let lexeme = self.get_token_lexeme(&self.current_token());
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("expected `item declaration`, found `{lexeme}`"),
          )
          .with_label(
            self.current_token().span,
            Some("expected `item declaration` here".to_string()),
            LabelStyle::Primary,
          )
          .with_note(format!("unexpected token: `{lexeme}`"))
          .with_help("use a valid item declaration or remove the token".to_string());
        self.emit(diagnostic);
        Err(())
      },
    }
  }

  /// Parses a single statement node (stubbed for future grammar branches).
  /// Currently supports empty statements and expression statements.
  pub(crate) fn parse_stmt(&mut self, context: ParserContext) -> Result<Stmt, ()> {
    let mut token = self.current_token();
    let outer_attributes = self.parse_outer_attributes(context)?;
    let visibility = self.parse_visibility(context)?;

    match self.current_token().kind {
      TokenKind::Semi => {
        // Empty statement: just a semicolon
        self.advance();
        Ok(Stmt::Empty {
          span: *token.span.merge(self.last_token_span()),
        })
      },
      // let declaration
      TokenKind::KwLet => self.parse_let_statement(outer_attributes, context),

      // Hnaldle macro invocation
      TokenKind::Ident if matches!(self.peek(1).kind, TokenKind::Bang) => {
        self.parse_macro_invocation_statement(context)
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
            | TokenKind::LParen
        ) =>
      {
        self.parse_macro_invocation_statement(context)
      },

      // expression statement
      _ if self.current_token().kind.can_start_expression()
        && !self.can_start_fun()
        && !self.can_start_const_item()
        && !self.can_start_extern_crate()
        && !self.can_start_foreign_extern_crate()
        && !self.can_start_impl()
        && !self.can_start_trait() =>
      {
        self.parse_expr_stmt(outer_attributes, context)
      },

      // item statement
      _ => {
        let item = self.parse_item(outer_attributes, visibility)?;
        Ok(Stmt::Item(Box::new(item)))
      },
    }
  }

  /// Parses an expression statement, optionally consuming a trailing semicolon.
  fn parse_expr_stmt(
    &mut self,
    outer_attributes: Vec<Attribute>,
    context: ParserContext,
  ) -> Result<Stmt, ()> {
    let mut token = self.current_token();
    let expr = self.parse_expression(outer_attributes, context)?;
    let has_semi = matches!(self.current_token().kind, TokenKind::Semi);

    Ok(Stmt::Expr {
      expr,
      has_semi,
      span: *token.span.merge(self.last_token_span()),
    })
  }

  pub(crate) fn parse_expression(
    &mut self,
    outer_attributes: Vec<Attribute>,
    context: ParserContext,
  ) -> Result<Expr, ()> {
    let label = self.parse_label(true)?;
    let attrs = outer_attributes;

    let mut expr = match self.current_token().kind {
      TokenKind::KwIf => self.parse_if_expression(context),

      TokenKind::KwMatch => self.parse_match_expression(context),
      TokenKind::Or => self.parse_closure(ParserContext::Closure),
      TokenKind::KwMove | TokenKind::KwAsync if self.can_start_closure() => {
        self.parse_closure(ParserContext::Closure)
      },
      TokenKind::LBrace => {
        let block_context = if matches!(
          context,
          ParserContext::LoopCondition
            | ParserContext::WhileCondition
            | ParserContext::ForCondition
        ) {
          context
        } else {
          ParserContext::Block
        };
        self.parse_block(label, block_context, attrs.clone())
      },
      TokenKind::KwConst | TokenKind::KwAsync | TokenKind::KwUnsafe | TokenKind::KwTry
        if self.can_start_block_expression() =>
      {
        let block_context = if matches!(
          context,
          ParserContext::LoopCondition
            | ParserContext::WhileCondition
            | ParserContext::ForCondition
        ) {
          context
        } else {
          ParserContext::Block
        };
        self.parse_block(label, block_context, attrs.clone())
      },
      TokenKind::KwContinue => self.parse_continue_expression(context),
      TokenKind::KwBreak => self.parse_break_expression(context),
      TokenKind::KwReturn => self.parse_return_expression(context),
      TokenKind::KwLoop => {
        let loop_context = if matches!(context, ParserContext::Default) {
          ParserContext::LoopCondition
        } else {
          context
        };
        self.parse_loop_expression(label, attrs.clone(), loop_context)
      },
      TokenKind::KwWhile => {
        let while_context = if matches!(context, ParserContext::Default) {
          ParserContext::WhileCondition
        } else {
          context
        };
        self.parse_while_expression(label, attrs.clone(), while_context)
      },
      TokenKind::KwFor => {
        let for_context = if matches!(context, ParserContext::Default) {
          ParserContext::ForCondition
        } else {
          context
        };
        self.parse_for_expression(label, attrs.clone(), for_context)
      },
      _ => self.parse_assignment_expr(context),
    }?;

    if !attrs.is_empty() {
      expr.attributes = attrs;
    }

    Ok(expr)
  }

  pub(crate) fn parse_primary(&mut self, context: ParserContext) -> Result<Expr, ()> {
    let token = self.current_token();

    match token.kind {
      // literals
      TokenKind::Literal { kind } => self.parser_literal(kind),

      TokenKind::KwTrue | TokenKind::KwFalse => self.parser_bool(),

      // path expressions
      TokenKind::Lt => self.parse_qualified_path(context),

      TokenKind::Dollar if matches!(self.peek(1).kind, TokenKind::KwCrate) => {
        self.parse_path_expr(true, context)
      },

      TokenKind::ColonColon => self.parse_path_expr(true, context),

      TokenKind::Ident
      | TokenKind::RawIdent
      | TokenKind::KwSelf
      | TokenKind::KwSuper
      | TokenKind::KwCrate
      | TokenKind::KwSelfType => self.parse_path_expr(true, context),

      // grouped and tuple expressions
      TokenKind::LParen => self.parse_grouped_and_tuple_expr(),

      // array expression
      TokenKind::LBracket => self.parse_array_expr(),

      _ => {
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = self
          .diagnostic(
            DiagnosticError::UnexpectedToken,
            format!("expected `primary expression`, found `{lexeme}`"),
          )
          .with_label(
            token.span,
            Some("expected `primary expression` here".to_string()),
            LabelStyle::Primary,
          )
          .with_note(format!("unexpected token: `{lexeme}`"))
          .with_help("start an expression or remove the unexpected token".to_string());
        self.emit(diagnostic);
        Err(())
      },
    }
  }
}
