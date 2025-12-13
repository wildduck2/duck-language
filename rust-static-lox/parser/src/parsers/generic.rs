//! TODO: Generic parameter & argument parsing is incomplete compared to Rust’s full grammar.
//!
//! Missing features & known limitations:
//!
//! -----------------------------------------------
//! GENERIC PARAMETERS (`<T, 'a, const N: usize>`)
//! -----------------------------------------------
//!
//! - **Full const-generic expression support**  
//!   Currently only `const NAME: Type = <type>` is supported.  
//!   Rust allows arbitrary expressions on the RHS (`const N: usize = 1 + 2`).
//!
//! - **Trait object auto traits (`?Trait`) and modifiers**  
//!   The modifier handling (`?`, `const`, `const ?`) is incomplete and incorrect.
//!   Rust uses:  
//!     - `?Trait` only for auto traits (e.g., `?Send`)  
//!     - `~const Trait` (but not `const Trait`).  
//!       Current grammar mismatches Rust.
//!
//! - **Lifetime bounds require better delimiter detection**  
//!   Stop conditions are incomplete.  
//!   Rust allows:  
//!   `'a: 'b + 'c` and `'a: 'static`  
//!   but must stop on: `{`, `(`, `=`, `+`, `,`, `>`, `where`.
//!
//! - **Attributes inside generic parameter lists**  
//!   You allow `#[attr] T`, but Rust also supports `#[attr] const N: usize` etc.  
//!   Alignment is not fully verified.
//!
//! - **Where-clause bounds inside generics (unstable RFC)**  
//!   Rust supports `T: Trait<Assoc = impl Trait>` in generic params.  
//!   This parser does not support associated type equality in bounds.
//!
//!
//! -----------------------------------------------
//! GENERIC ARGUMENTS (`::<T, 'a, 3, Item = U>`)
//! -----------------------------------------------
//!
//! - **Missing support for type applications in arguments**  
//!   e.g. `Iterator<Item = <T as Trait>::Assoc>`
//!
//! - **Const generics use expression grammar, not type grammar**  
//!   Currently you use `parse_expression` but bracketed expressions,
//!   unary negation, block expressions, and path expressions are still incomplete.
//!
//! - **Misclassification between TypeArg, ConstArg, ConstExpr, Binding**  
//!   The lookahead `Eq | Lt` heuristic is insufficient.  
//!   Rust uses this grammar:
//!   ```text
//!   generic_arg
//!       : lifetime
//!       | type
//!       | const_expr
//!       | type_binding   // `Assoc = Ty`
//!       | const_arg      // const NAME = expr
//!   ```
//!   Your implementation merges several cases and mis-parses edge cases like:
//!   `<T = U,>`
//!   `<Item<'a> = T,>`
//!
//! - **No support for qualified path arguments (`<T as Trait>::Assoc`)**  
//!   These must appear within generic args for many advanced Rust features.
//!
//! - **Missing support for implicit elided lifetimes in args (`<'_>`)**  
//!   Rust allows anonymous lifetimes in generics; parser must accept `'_'`.
//!
//! - **No support for `impl Trait` in generic args**  
//!   e.g. `<impl Iterator<Item = T>>` inside function return types.
//!
//!
//! -----------------------------------------------
//! PATH GENERIC ARGUMENTS (`foo::<T, U>`)
//! -----------------------------------------------
//!
//! - **Missing turbofish precedence handling**  
//!   Rust has special rules allowing:  
//!   `foo::<T>()`  
//!   `(foo::<T>)()`  
//!   `bar::<T>::baz`  
//!   The parser currently merges `<` incorrectly with comparison parsing.
//!
//! - **Angle-bracket vs. less-than operator ambiguity**  
//!   Rust uses complex disambiguation rules ("type ascription fallback") to decide whether
//!   `<` begins a generic-argument list or a `<` binary operator.  
//!   Your parser uses simple token matching and may misparse expressions like:  
//!   `x < y, z > w`  
//!   `(a<b>::c)`  
//!
//!
//! -----------------------------------------------
//! ERRORS & DIAGNOSTICS
//! -----------------------------------------------
//!
//! - **Need improved diagnostics on malformed generic params**  
//!   Cases like:  
//!   `<T U>`  
//!   `<T, , U>`  
//!   `<,T>`  
//!   `<T:>`  
//!   should produce structured, helpful errors and attempt recovery until `>`.
//!
//! - **Span merging improvements**  
//!   Begin/end spans of generic arguments should correctly include `<` and `>`
//!   and nested spans for bounds, defaults, and constraints.
//!
//! - **Better recovery strategy after syntax errors inside `<...>`**  
//!   Incorrect tokens currently cause full abort instead of resynchronizing at `>` or `,`.
//!
//!
//! -----------------------------------------------
//! PARSER ARCHITECTURE
//! -----------------------------------------------
//!
//! - **`parse_generic_params` incorrectly merges span with next token**  
//!   You `merge` the next token’s span, but generics should end at `>`,
//!   not the next unrelated token.
//!
//! - **Separation of generic params vs generic args should be stricter**  
//!   Rust differentiates:  
//!   struct Foo<T>` (params)  
//!   `Foo::<T>` (args)  
//!   They follow different grammars; current code mixes them heavily.
//!
//! - **Requires implementation of full generic argument recursion**  
//!   Nested generics:  
//!   `Foo<Bar<Baz<T, U>, X>, Y>`  
//!   should recurse correctly.
//!
//! - **Support for default generic argument values**  
//!   e.g. `Vec<T = u8>`  
//!   Not yet supported.
//!
//! This module handles **basic Rust-like generics**, but lacks advanced type theory,
//! ambiguity resolution, and the richer grammar rules needed for complete Rust compatibility.

use crate::{
  ast::{
    generic::*,
    path::{Path, PathSegment, PathSegmentKind},
    Type,
  },
  parser_utils::ExprContext,
  DiagnosticEngine, Parser,
};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
};
use lexer::token::{Token, TokenKind};

impl Parser {
  /// Function that parses `<...>` generic parameter lists and returns `None` when absent.
  ///
  /// for example:
  /// ```rust
  /// let generics = self.parse_generic_params(engine)?;
  /// ```
  /// You will use this to get the generics of a struct declaration
  /// like `struct User<T, U> { name: String, age: u8 } where T: Clone + PartialEq`
  pub(crate) fn parse_generic_params(
    &mut self,
    token: &mut Token,
    engine: &mut DiagnosticEngine,
  ) -> Result<Option<GenericParams>, ()> {
    if !matches!(self.current_token().kind, TokenKind::Lt) {
      return Ok(None);
    }

    let mut params = Vec::<GenericParam>::new();

    self.expect(TokenKind::Lt, engine)?; // consume the "<"

    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::Gt) {
      params.push(self.parse_generic_param(engine)?);

      if matches!(self.current_token().kind, TokenKind::Comma) {
        self.advance(engine); // consume the comma
      }
    }

    self.expect(TokenKind::Gt, engine)?; // consume the ">"

    token.span.merge(self.current_token().span);
    Ok(Some(GenericParams {
      params,
      span: token.span,
    }))
  }

  /// Parses a single generic parameter (type, lifetime, or const).
  pub(crate) fn parse_generic_param(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<GenericParam, ()> {
    let attributes = if matches!(self.current_token().kind, TokenKind::Pound) {
      self.parse_attributes(engine)?
    } else {
      vec![]
    };

    let token = self.current_token();

    match token.kind {
      // const generic: const N: usize = 3
      TokenKind::KwConst => {
        self.advance(engine); // consume "const"
        let name = self.parse_name(false, engine)?;

        self.expect(TokenKind::Colon, engine)?; // must have ":"
        let ty = self.parse_type(engine)?;

        let default = if matches!(self.current_token().kind, TokenKind::Eq) {
          self.advance(engine);
          Some(self.parse_type(engine)?)
        } else {
          None
        };

        Ok(GenericParam::Const {
          attributes,
          name,
          ty,
          default,
        })
      },

      // lifetime generic: 'a or 'a: 'b + 'c
      TokenKind::Lifetime { .. } => {
        let name = self.get_token_lexeme(&token);
        self.advance(engine);

        let bounds = if matches!(self.current_token().kind, TokenKind::Colon) {
          self.advance(engine);
          Some(self.parse_type_lifetime_bounds(engine)?)
        } else {
          None
        };

        Ok(GenericParam::Lifetime {
          attributes,
          name,
          bounds,
        })
      },

      // type generic: T, U: Bound, T = Default
      TokenKind::Ident => {
        let name = self.parse_name(false, engine)?;

        let bounds = self.parse_type_bounds(engine)?;

        let default = if matches!(self.current_token().kind, TokenKind::Eq) {
          self.advance(engine);
          Some(self.parse_type(engine)?)
        } else {
          None
        };

        Ok(GenericParam::Type {
          attributes,
          name,
          bounds,
          default,
        })
      },

      _ => {
        let lexeme = self.get_token_lexeme(&token);
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          format!("unexpected token `{}` in generic parameter list", lexeme),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some("expected a type, lifetime, or const parameter".to_string()),
          LabelStyle::Primary,
        );

        engine.add(diagnostic);
        Err(())
      },
    }
  }

  /// Parses either lifetime or trait bounds that follow a colon.
  pub(crate) fn parse_type_bounds(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Option<Vec<TypeBound>>, ()> {
    if !matches!(self.current_token().kind, TokenKind::Colon) {
      return Ok(None);
    };

    let x = if matches!(self.current_token().kind, TokenKind::Lifetime { .. }) {
      self.parse_type_lifetime_bounds(engine)?
    } else {
      self.parse_type_path_bounds(engine)?
    };

    self.advance(engine);
    Ok(Some(x))
  }

  /// Parses the `+ 'a + 'b` lifetime bounds chain.
  pub(crate) fn parse_type_lifetime_bounds(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<TypeBound>, ()> {
    let mut lifetime_bounds = vec![];

    while !self.is_eof()
      && !matches!(
        self.current_token().kind,
        TokenKind::OpenBrace | TokenKind::Comma | TokenKind::KwWhere | TokenKind::Gt
      )
    {
      let lifetime = self.get_token_lexeme(&self.current_token());
      self.advance(engine); // consume the lifetime
      lifetime_bounds.push(TypeBound {
        modifier: TraitBoundModifier::None,
        path: Path {
          leading_colon: false,
          segments: vec![PathSegment {
            kind: PathSegmentKind::Ident(lifetime),
            args: None,
          }],
        },
        generics: None,
        for_lifetimes: None,
      });
    }

    Ok(lifetime_bounds)
  }

  /// Parses trait bounds (`T: Trait + ?Sized`) and their modifiers.
  pub(crate) fn parse_type_path_bounds(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<TypeBound>, ()> {
    let mut bounds: Vec<TypeBound> = vec![];

    while !self.is_eof()
      && !matches!(
        self.current_token().kind,
        TokenKind::Comma | TokenKind::Gt | TokenKind::Eq | TokenKind::CloseParen | TokenKind::Semi
      )
    {
      let modifier = if matches!(self.current_token().kind, TokenKind::Question) {
        // (e.g., `?Clone`)
        self.advance(engine); // consume the "?"
        TraitBoundModifier::Maybe
      } else if matches!(self.current_token().kind, TokenKind::KwConst) {
        // (e.g., `const Clone`)
        self.advance(engine); // consume the "const"
        TraitBoundModifier::Const
      } else if matches!(self.current_token().kind, TokenKind::KwConst)
        && matches!(self.peek(1).kind, TokenKind::Question)
      {
        self.advance(engine); // consume the "const"
        self.advance(engine); // consume the "?"
                              // (e.g., `const ?Clone`)
        TraitBoundModifier::MaybeConst
      } else {
        TraitBoundModifier::None
      };

      let path = self.parse_path(false, engine)?;

      let (generics, for_lifetimes) = if matches!(self.current_token().kind, TokenKind::Lt) {
        self.advance(engine); // consume the "<"

        let value = match self.current_token().kind {
          TokenKind::Lifetime { .. } => (None, Some(self.parse_generic_lifetime_args(engine)?)),
          TokenKind::Ident => (Some(self.parse_generic_args(engine)?), None),
          _ => (None, None),
        };

        self.expect(TokenKind::Gt, engine)?; // consume the ">"
        value
      } else {
        (None, None)
      };

      bounds.push(TypeBound {
        modifier,
        path,
        generics,
        for_lifetimes,
      });

      if matches!(self.current_token().kind, TokenKind::Plus) {
        self.advance(engine); // consume the plus
      }
    }
    Ok(bounds)
  }

  fn parse_generic_lifetime_args(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Vec<String>, ()> {
    let mut lifetime = vec![];
    while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::Gt) {
      lifetime.extend(self.parse_lifetime_bounds(engine)?);
      if matches!(self.current_token().kind, TokenKind::Comma) {
        self.advance(engine); // consume the comma
      }
    }
    Ok(lifetime)
  }

  /// Function that parses generic arguments following `<...>`.
  /// It returns a vector of `GenericArg` structs that represents a list of generic arguments
  ///
  /// for example:
  /// ```rust
  /// let args = self.parse_generic_args(engine)?;
  /// ```
  ///
  /// You will use this to get the generic arguments of a path
  /// like `Path { segments, args }`
  fn parse_generic_args(&mut self, engine: &mut DiagnosticEngine) -> Result<Vec<GenericArg>, ()> {
    let mut args = Vec::<GenericArg>::new();

    while !self.is_eof() && self.current_token().kind != TokenKind::Gt {
      args.push(self.parse_generic_arg(engine)?);
      println!(
        "--debug: {:#?}",
        self.get_token_lexeme(&self.current_token())
      );

      if matches!(self.current_token().kind, TokenKind::Comma) {
        self.advance(engine); // consume the comma
      }
    }

    Ok(args)
  }

  // TODO: fix this later on we are still missing some () handling in the generic args
  pub(crate) fn parse_path_generic_args(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Option<GenericArgs>, ()> {
    self.expect(TokenKind::Lt, engine)?; // consume the "<"

    let args = self.parse_generic_args(engine)?;

    self.expect(TokenKind::Gt, engine)?; // consume the ">"

    Ok(Some(GenericArgs::AngleBracketed { args }))
  }

  /// Parses a single generic argument (type, lifetime, const, binding, …).
  pub(crate) fn parse_generic_arg(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<GenericArg, ()> {
    let mut token = self.current_token();
    let name = self.get_token_lexeme(&token);

    match token.kind {
      TokenKind::Lifetime { .. } => {
        self.advance(engine); // consume the lifetime
        Ok(GenericArg::Lifetime(name))
      },
      TokenKind::Ident if matches!(self.peek(1).kind, TokenKind::Eq | TokenKind::Lt) => {
        self.advance(engine); // consume the identifier

        let generics = self.parse_generic_params(&mut token, engine)?;

        if self.current_token().kind == TokenKind::Colon {
          self.advance(engine); // consume the ':'
          let bounds = self.parse_type_bounds(engine)?;
          return Ok(GenericArg::Constraint {
            name,
            generics,
            bounds,
          });
        }

        self.expect(TokenKind::Eq, engine)?; // consume the '='
        let ty = self.parse_type(engine)?;

        Ok(GenericArg::Binding { name, generics, ty })
      },

      TokenKind::Ident => {
        match self.peek(1).kind {
          TokenKind::Eq | TokenKind::Lt => {
            // Associated type binding or constraint
            self.advance(engine);
            let generics = self.parse_generic_params(&mut token, engine)?;
            if self.current_token().kind == TokenKind::Colon {
              self.advance(engine);
              let bounds = self.parse_type_bounds(engine)?;
              return Ok(GenericArg::Constraint {
                name,
                generics,
                bounds,
              });
            }
            self.expect(TokenKind::Eq, engine)?;
            let ty = self.parse_type(engine)?;
            Ok(GenericArg::Binding { name, generics, ty })
          },

          TokenKind::Comma
          // FIX: this doe snot work with the <Vec<T> as SliceExt>::slice_pattern
          // in the struct pattern
          // | TokenKind::Gt
          | TokenKind::CloseParen
          | TokenKind::Plus
          | TokenKind::Minus
          | TokenKind::Star
          | TokenKind::Slash
          | TokenKind::OpenBrace => {
            let expr = self.parse_expression(vec![],ExprContext::Default, engine)?;
            println!(
              "--debug: {:#?}",
              self.get_token_lexeme(&self.current_token())
            );
            Ok(GenericArg::Const(expr))
          },

          // Otherwise, assume it's a type argument
          _ => Ok(GenericArg::Type(self.parse_type(engine)?)),
        }
      },

      TokenKind::KwSelfType => {
        self.advance(engine); // consume the 'Self'
        Ok(GenericArg::Type(Type::SelfType))
      },

      TokenKind::OpenParen => {
        self.advance(engine); // consume the '('
        let mut params = vec![];
        while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseParen) {
          params.push(self.parse_type(engine)?);
          if matches!(self.current_token().kind, TokenKind::Comma) {
            self.advance(engine); // consume the comma
          }
        }
        self.expect(TokenKind::CloseParen, engine)?; // consume ')'

        Ok(GenericArg::Type(Type::Tuple(params)))
      },
      _ => {
        // TODO: enhance the diagnostic later on when we have a full clousure
        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
          format!("unexpected token `{name}` in generic argument list"),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some(format!("Expected a primary expression, found \"{}\"", name)),
          LabelStyle::Primary,
        )
        .with_help(Parser::get_token_help(&token.kind, &token));

        engine.add(diagnostic);

        Err(())
      },
    }
  }
}
