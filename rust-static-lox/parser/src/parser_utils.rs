use crate::{ast::*, Parser};
use lexer::token::TokenKind;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExprContext {
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
  Macro,
}

impl Parser {
  /// Parses the top-level production, collecting statements until EOF.
  /// Currently this routine prints each item tree for debugging and relies on
  /// `parse_item` to decide which constructs are supported.
  pub fn parse_program(&mut self) {
    while !self.is_eof() {
      // TODO: check this context
      match self.parse_stmt(ExprContext::Default) {
        // Returns Item, not Stmt
        Ok(item) => {
          // println!("{:#?}", item);
          println!("{:#?}", item);
          // item.print_tree("", true);
          // self.ast.push(item); // ast should be Vec<Item>
        },
        Err(_) => self.synchronize(),
      }
    }
  }

  /// Dispatches to the correct item parser after consuming attributes & visibility.
  fn parse_item(&mut self, attributes: Vec<Attribute>, visibility: Visibility) -> Result<Item, ()> {
    match self.current_token().kind {
      TokenKind::KwStruct => self.parse_struct_decl(attributes, visibility),
      TokenKind::KwFn
      | TokenKind::KwConst
      | TokenKind::KwAsync
      | TokenKind::KwUnsafe
      | TokenKind::KwExtern => self.parse_fn_decl(attributes, visibility),
      TokenKind::KwEnum => self.parse_enum_decl(attributes, visibility),
      TokenKind::KwType => self.parse_type_alias_decl(attributes, visibility),
      TokenKind::KwStatic => self.parse_static_decl(attributes, visibility),
      // TokenKind::KwConst => self.parse_const_decl(attributes, visibility, ),
      // TokenKind::KwMod => self.parse_module_decl(attributes, visibility, ),
      // TokenKind::KwUse => self.parse_use_decl(attributes, visibility, ),
      // TokenKind::KwExternCrate => self.parse_extern_crate_decl(attributes, visibility, ),
      // TokenKind::KwMacro => self.parse_macro_decl(attributes, visibility, ),
      // TokenKind::KwMacro2 => self.parse_macro2_decl(attributes, visibility, ),
      // TokenKind::KwExternType => self.parse_extern_type_decl(attributes, visibility, ),
      // TokenKind::KwUnion => self.parse_union_decl(attributes, visibility, ),
      // TokenKind::KwExtern => self.parse_extern_decl(attributes, visibility, ),
      _ => {
        let lexeme = self.get_token_lexeme(&self.current_token());
        self.emit(self.err_unexpected_token(
          self.current_token().span,
          "item declaration",
          &lexeme,
        ));
        Err(())
      },
    }
  }

  /// Parses a single statement node (stubbed for future grammar branches).
  /// Currently supports empty statements and expression statements.
  pub(crate) fn parse_stmt(&mut self, context: ExprContext) -> Result<Stmt, ()> {
    let mut token = self.current_token();
    let outer_attributes = self.parse_outer_attributes()?;
    let visibility = self.parse_visibility()?;

    match self.current_token().kind {
      TokenKind::Semi => {
        // Empty statement: just a semicolon
        self.advance();
        Ok(Stmt::Empty {
          span: *token.span.merge(self.current_token().span),
        })
      },
      // let declaration
      TokenKind::KwLet => self.parse_let_statement(context, outer_attributes),

      // Hnaldle macro invocation
      TokenKind::Ident if matches!(self.peek(1).kind, TokenKind::Bang) => {
        self.parse_macro_invocation_statement()
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
        self.parse_macro_invocation_statement()
      },

      // expression statement
      _ if self.current_token().kind.can_start_expression() && !self.can_start_fun() => {
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
    context: ExprContext,
  ) -> Result<Stmt, ()> {
    let mut token = self.current_token();
    let expr = self.parse_expression(outer_attributes, context)?;
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
  ) -> Result<Expr, ()> {
    let label = self.parse_label(true)?;

    match self.current_token().kind {
      TokenKind::KwIf => self.parse_if_expression(ExprContext::IfCondition),

      TokenKind::KwMatch => self.parse_match_expression(ExprContext::Match),
      TokenKind::Or => self.parse_closure(ExprContext::Closure),
      TokenKind::KwMove | TokenKind::KwAsync if self.can_start_closure() => {
        self.parse_closure(ExprContext::Closure)
      },
      TokenKind::LBrace => self.parse_block(label, ExprContext::Block, outer_attributes),
      TokenKind::KwAsync | TokenKind::KwUnsafe | TokenKind::KwTry
        if self.can_start_block_expression() =>
      {
        self.parse_block(label, ExprContext::Block, outer_attributes)
      },
      TokenKind::KwContinue => self.parse_continue_expression(context),
      TokenKind::KwBreak => self.parse_break_expression(context),
      TokenKind::KwReturn => self.parse_return_expression(context),
      TokenKind::KwLoop => {
        self.parse_loop_expression(label, outer_attributes, ExprContext::LoopCondition)
      },
      TokenKind::KwWhile => {
        self.parse_while_expression(label, outer_attributes, ExprContext::WhileCondition)
      },
      TokenKind::KwFor => {
        self.parse_for_expression(label, outer_attributes, ExprContext::ForCondition)
      },
      _ => self.parse_assignment_expr(context),
    }
  }

  pub(crate) fn parse_primary(&mut self, context: ExprContext) -> Result<Expr, ()> {
    let token = self.current_token();

    match token.kind {
      // literals
      TokenKind::Literal { kind } => self.parser_literal(kind),

      TokenKind::KwTrue | TokenKind::KwFalse => self.parser_bool(),

      // macro invocation expression
      TokenKind::Ident if matches!(self.peek(1).kind, TokenKind::Bang) => {
        self.parse_macro_invocation_expression()
      },

      // path expressions
      TokenKind::Lt => self.parse_qualified_path(),

      TokenKind::Dollar if matches!(self.peek(1).kind, TokenKind::KwCrate) => {
        self.parse_path_expr(context, true)
      },

      TokenKind::ColonColon => self.parse_path_expr(context, true),

      TokenKind::Ident
      | TokenKind::KwSelf
      | TokenKind::KwSuper
      | TokenKind::KwCrate
      | TokenKind::KwSelfType => self.parse_path_expr(context, true),

      // grouped and tuple expressions
      TokenKind::LParen => self.parse_grouped_and_tuple_expr(),

      // array expression
      TokenKind::LBracket => self.parse_array_expr(),

      _ => {
        let lexeme = self.get_token_lexeme(&token);
        self.emit(self.err_unexpected_token(token.span, "primary expression", &lexeme));
        Err(())
      },
    }
  }
}
