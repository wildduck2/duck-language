use crate::{
  ast::{path::Path, Mutability, QSelfHeader, Type},
  match_and_consume,
  parser_utils::ExprContext,
  DiagnosticEngine, Parser,
};
use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, LabelStyle},
  types::error::DiagnosticError,
};
use lexer::token::TokenKind;

impl Parser {
  //! TODO:
  //! extend type parsing to cover the full Rust type grammar:
  //! - tuple types (T, U) and grouped types (T)
  //! - slice types [T]
  //! - never type !
  //! - impl Trait and dyn Trait forms
  //! - function pointer types fn(..) -> ..
  //! - qualified paths in types like <T as Trait>::Item
  //! - inferred type _
  //! - parenthesized types inside more complex forms

  /// Parses a type according to a small but Rust like subset of the type grammar.
  ///
  /// Grammar subset handled here:
  ///
  /// type
  ///   -> ident_or_primitive                      // u32, i64, bool, user paths
  ///    | path_type                               // Foo, crate::foo::Bar, with optional generics
  ///    | "&" lifetime? "mut"? type               // references, for example &T or &'a mut U
  ///    | "*" ("const" | "mut") type              // raw pointers like *const T or *mut U
  ///    | "[" type ";" expression "]"             // array types like [T; N]
  ///
  /// Notes:
  /// - tuple types (T, U), slices [T], impl Trait, dyn Trait,
  ///   function pointer types and others are not yet implemented.
  pub(crate) fn parse_type(&mut self, engine: &mut DiagnosticEngine) -> Result<Type, ()> {
    let mut token = self.current_token();
    let lexeme = self.get_token_lexeme(&token);
    self.advance(engine); // consume the first token of the type

    match token.kind {
      TokenKind::Lt => {
        // we unwrap here because we know we have a `<` token
        let QSelfHeader { self_ty, trait_ref } = self.parse_qself_header(engine)?.unwrap();
        let name = self.parse_name_identifier(engine)?;
        let generics = if matches!(self.current_token().kind, TokenKind::Lt) {
          self.parse_path_generic_args(engine)?
        } else {
          None
        };

        Ok(Type::QPath {
          self_ty,
          trait_ref,
          name,
          generics: generics.map(Box::new),
        })
      },

      // Primitive names and user defined paths
      TokenKind::Ident | TokenKind::KwCrate => match lexeme.as_str() {
        "u8" => Ok(Type::U8),
        "u16" => Ok(Type::U16),
        "u32" => Ok(Type::U32),
        "u64" => Ok(Type::U64),
        "u128" => Ok(Type::U128),
        "usize" => Ok(Type::Usize),

        "i8" => Ok(Type::I8),
        "i16" => Ok(Type::I16),
        "i32" => Ok(Type::I32),
        "i64" => Ok(Type::I64),
        "i128" => Ok(Type::I128),
        "isize" => Ok(Type::Isize),

        "f32" => Ok(Type::F32),
        "f64" => Ok(Type::F64),
        "f128" => Ok(Type::F128),

        "char" => Ok(Type::Char),
        "str" => Ok(Type::Str),
        "String" => Ok(Type::String),

        "bool" => Ok(Type::Bool),

        // Fallback: treat as a path type, possibly with generic arguments
        _ => {
          // Reset position so parse_path can consume the ident or crate token
          self.current -= 1;
          Ok(Type::Path(self.parse_path(true, engine)?))
        },
      },

      // Tuple and parenthesized types: (T, U, V)
      TokenKind::OpenParen => {
        let mut types = vec![];

        while !self.is_eof() && !matches!(self.current_token().kind, TokenKind::CloseParen) {
          types.push(self.parse_type(engine)?);
          match_and_consume!(self, engine, TokenKind::Comma)?;
        }

        self.expect(TokenKind::CloseParen, engine)?;
        Ok(Type::Tuple(types))
      },

      // Array type: [ T ; expr ]
      TokenKind::OpenBracket => {
        let element = self.parse_type(engine)?;
        self.expect(TokenKind::Semi, engine)?; // consume ';'

        let size = self.parse_expression(vec![], ExprContext::Default, engine)?;
        self.expect(TokenKind::CloseBracket, engine)?; // consume ']'

        Ok(Type::Array {
          element: Box::new(element),
          size: Box::new(size),
        })
      },

      // Raw pointer: *const T or *mut T
      TokenKind::Star => {
        // If we see *T directly, this is missing the const or mut qualifier.
        if matches!(self.current_token().kind, TokenKind::Ident) {
          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidPointerType),
            "Missing mutability qualifier for raw pointer type.".to_string(),
            self.source_file.path.clone(),
          )
          .with_label(
            self.current_token().span,
            Some("expected const or mut after *.".to_string()),
            LabelStyle::Primary,
          )
          .with_note(
            "Raw pointers in Rust must explicitly specify mutability, either *const T or *mut T."
              .to_string(),
          )
          .with_help(
            "Use *const T for an immutable raw pointer, or *mut T for a mutable one.".to_string(),
          );

          engine.add(diagnostic);
          return Err(());
        }

        // Reuse Mutability for pointer mutability:
        // *const T  maps to Mutability::Immutable
        // *mut T    maps to Mutability::Mutable
        let mutability = self.parse_mutability(engine)?;

        Ok(Type::RawPointer {
          mutability,
          inner: Box::new(self.parse_type(engine)?),
        })
      },

      // Reference types: &T, &'a T, &mut T, &'a mut T
      TokenKind::And => {
        // Forbid patterns like &const T which are not valid reference types.
        if matches!(self.current_token().kind, TokenKind::KwConst) {
          let diagnostic = Diagnostic::new(
            DiagnosticCode::Error(DiagnosticError::InvalidMutabilityInField),
            "Invalid const specifier in reference type.".to_string(),
            self.source_file.path.clone(),
          )
          .with_label(
            self.current_token().span,
            Some("const is not allowed after & in a reference type.".to_string()),
            LabelStyle::Primary,
          )
          .with_note(
            "const does not apply to references. Only raw pointers support const qualifiers."
              .to_string(),
          )
          .with_help(
            "Use *const T for a raw const pointer, or &T for an immutable reference.".to_string(),
          );

          engine.add(diagnostic);
          return Err(());
        }

        // Special case: &*const T or &*mut T
        if matches!(self.current_token().kind, TokenKind::Star) {
          return Ok(Type::Reference {
            lifetime: None,
            mutability: Mutability::Immutable,
            inner: Box::new(self.parse_type(engine)?),
          });
        }

        // Optional lifetime right after &
        let lifetime = self.parse_type_lifetime(engine)?;
        // Optional mut after lifetime: &mut T or &'a mut T
        let mutability = self.parse_mutability(engine)?;

        Ok(Type::Reference {
          lifetime,
          mutability,
          inner: Box::new(self.parse_type(engine)?),
        })
      },

      TokenKind::KwSelfType => Ok(Type::SelfType),

      // Reject bare mut or const at the start of a type
      _ if matches!(token.kind, TokenKind::KwMut | TokenKind::KwConst) => {
        token.span.merge(self.current_token().span);
        let lexeme = self.get_token_lexeme(&token);

        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidMutabilityInField),
          format!("Invalid {} specifier in type position.", lexeme),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some(format!("{} is not allowed before a bare type.", lexeme)),
          LabelStyle::Primary,
        )
        .with_note("mut and const cannot modify field or type declarations directly.".to_string())
        .with_help(
          "Use &mut T or *mut T for references or pointers, or make the binding itself mutable."
            .to_string(),
        );

        engine.add(diagnostic);
        Err(())
      },

      // Fallback for unknown type starts
      _ => {
        token.span.merge(self.current_token().span);
        let lexeme = self.get_token_lexeme(&token);

        let diagnostic = Diagnostic::new(
          DiagnosticCode::Error(DiagnosticError::InvalidType),
          format!("Unknown type or unexpected token {}.", lexeme),
          self.source_file.path.clone(),
        )
        .with_label(
          token.span,
          Some(format!(
            "Type {} is not recognized in this position.",
            lexeme
          )),
          LabelStyle::Primary,
        )
        .with_help(format!(
          "If {} is a custom type, declare it or bring it into scope before use.",
          lexeme
        ));

        engine.add(diagnostic);
        Err(())
      },
    }
  }

  /// Parses an optional lifetime that appears immediately after an ampersand in a reference type.
  ///
  /// Grammar subset:
  ///
  /// referenceType
  ///   -> "&" LIFETIME? "mut"? type
  ///
  /// This helper only captures the LIFETIME part and returns:
  /// - Some(name) if the current token is a lifetime and the previous token was '&'
  /// - None otherwise
  pub(crate) fn parse_type_lifetime(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Option<String>, ()> {
    if matches!(self.current_token().kind, TokenKind::Lifetime { .. })
      && matches!(self.peek_prev(0).kind, TokenKind::And)
    {
      let token = self.current_token();
      self.advance(engine); // consume the lifetime token
      Ok(Some(self.get_token_lexeme(&token)))
    } else {
      Ok(None)
    }
  }

  pub(crate) fn parse_qself_header(
    &mut self,
    engine: &mut DiagnosticEngine,
  ) -> Result<Option<QSelfHeader>, ()> {
    if !matches!(self.current_token().kind, TokenKind::Lt) {
      return Ok(None);
    }

    // assumes current token is `<`
    self.expect(TokenKind::Lt, engine)?;
    let self_ty = Box::new(self.parse_type(engine)?);

    let trait_ref = if match_and_consume!(self, engine, TokenKind::KwAs)? {
      Some(self.parse_path(true, engine)?)
    } else {
      None
    };

    self.expect(TokenKind::Gt, engine)?;
    self.expect(TokenKind::ColonColon, engine)?;

    Ok(Some(QSelfHeader { self_ty, trait_ref }))
  }
}
