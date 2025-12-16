use diagnostic::Span;
use TokenKind::*;

/// A token produced by the lexer, containing both its kind and source location
#[derive(Debug, Clone)]
pub struct Token {
  pub kind: TokenKind,
  pub span: Span,
}

// ============================================================================
// SUPPORTING ENUMS
// ============================================================================

/// Style of a documentation comment
///
/// Documentation comments in Rust come in two flavors:
/// - **Outer**: Document the item that follows (`///` or `/** */`)
/// - **Inner**: Document the enclosing item (`//!` or `/*! */`)
///
/// # Examples
/// ```rust
/// /// This is an outer doc comment for the function below
/// fn foo() {
///   //! This is an inner doc comment for the function itself
/// }
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DocStyle {
  /// Outer doc comment: `///` or `/** */`
  Outer,
  /// Inner doc comment: `//!` or `/*! */`
  Inner,
}

/// Numeric base for integer and float literals
///
/// Rust supports four numeric bases, indicated by prefixes:
///
/// | Base        | Prefix | Example    | Digits         |
/// |-------------|--------|------------|----------------|
/// | Binary      | `0b`   | `0b1010`   | 0-1            |
/// | Octal       | `0o`   | `0o755`    | 0-7            |
/// | Decimal     | _(none)_ | `42`    | 0-9            |
/// | Hexadecimal | `0x`   | `0xDEAD`   | 0-9, a-f, A-F  |
///
/// # Examples
/// ```rust
/// let binary = 0b1111_0000;      // Base::Binary
/// let octal = 0o755;             // Base::Octal
/// let decimal = 1_000_000;       // Base::Decimal
/// let hex = 0xDEAD_BEEF;         // Base::Hexadecimal
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Base {
  /// Binary literals with `0b` prefix (base 2)
  Binary = 2,
  /// Octal literals with `0o` prefix (base 8)
  Octal = 8,
  /// Decimal literals with no prefix (base 10)
  Decimal = 10,
  /// Hexadecimal literals with `0x` prefix (base 16)
  Hexadecimal = 16,
}

/// The kind of literal token
///
/// Rust supports various literal types for numbers, characters, and strings.
/// Some variants carry extra metadata about malformed literals (for example,
/// `empty_int` on integers) to enable better error reporting.
///
/// # Examples
/// ```rust
/// 42              // Integer { base: Decimal, empty_int: false, .. }
/// 0xFF            // Integer { base: Hexadecimal, empty_int: false, .. }
/// 3.14            // Float { base: Decimal, .. }
/// 'x'             // Char
/// "hello"         // Str
/// r#"raw"#        // RawStr { n_hashes: 1 }
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
  /// Integer literal with optional suffix
  ///
  /// # Examples
  /// ```rust
  /// 42              // decimal
  /// 0b1010          // binary
  /// 0o755           // octal
  /// 0xDEAD_BEEF     // hexadecimal
  /// 100_u32         // with type suffix
  /// 0x_             // empty_int = true (malformed)
  /// ```
  Integer {
    /// The numeric base of the literal
    base: Base,
    /// True if no digits follow the base prefix (e.g., `0x` with nothing after)
    empty_int: bool,
    /// The start of the suffix (e.g., `u8` | `u32`)
    suffix_start: usize,
  },

  /// Floating-point literal with optional suffix
  ///
  /// # Examples
  /// ```rust
  /// 3.14            // basic float
  /// 1e10            // exponential notation
  /// 2.5E-3          // exponential with sign
  /// 1.0f32          // with type suffix
  /// 1e_             // malformed (empty exponent)
  /// ```
  ///
  /// **NOTE**: In Rust source code, floating-point literals are written in
  /// decimal form. Non-decimal bases are not accepted and should be rejected
  /// (or only used for diagnostics).
  Float {
    /// The numeric base of the literal.
    ///
    /// For valid Rust code this will effectively be `Base::Decimal`; other
    /// bases may be produced internally only for diagnostic purposes.
    base: Base,
    /// The start of the suffix (e.g., `f32`, `f64`)
    suffix_start: usize,
  },

  /// Character literal (single Unicode scalar)
  ///
  /// # Examples
  /// ```rust
  /// 'a'             // ASCII character
  /// '🦀'            // Unicode emoji
  /// '\n'            // escape sequence
  /// '\u{1F980}'     // Unicode escape
  /// ```
  Char,

  /// Byte literal (single ASCII byte)
  ///
  /// # Examples
  /// ```rust
  /// b'a'            // ASCII byte
  /// b'\n'           // escape sequence
  /// b'\x7F'         // hex escape
  /// ```
  ///
  /// **NOTE**: Byte literals must contain only ASCII characters (0–127).
  Byte,

  /// String literal with escape sequences
  ///
  /// # Examples
  /// ```rust
  /// "hello"         // basic string
  /// "foo\nbar"      // with escape
  /// "multi
  /// line"           // multiline with an embedded newline (valid)
  /// "unterminated   // (malformed)
  /// ```
  Str,

  /// Byte string literal (ASCII-only string as `&[u8]`)
  ///
  /// # Examples
  /// ```rust
  /// b"hello"        // ASCII bytes
  /// b"\x48\x69"     // hex escapes for "Hi"
  /// b"unterminated  // (malformed)
  /// ```
  ByteStr,

  /// C string literal (null-terminated, type `&CStr`)
  ///
  /// Added in Rust 1.77 for FFI interop.
  ///
  /// # Examples
  /// ```rust
  /// c"hello"        // becomes "hello\0"
  /// c"with\0null"   // explicit null allowed
  /// c"unterminated  // (malformed)
  /// ```
  CStr,

  /// Raw string literal (no escape processing)
  ///
  /// # Examples
  /// ```rust
  /// r"no\escapes"           // n_hashes = 0
  /// r#"with "quotes""#      // n_hashes = 1
  /// r##"more # freedom"##   // n_hashes = 2
  /// ```
  RawStr {
    /// Number of `#` delimiters used
    n_hashes: usize,
  },

  /// Raw byte string literal (raw + byte string combined)
  ///
  /// # Examples
  /// ```rust
  /// br"raw bytes"
  /// br#"with "quotes""#
  /// ```
  RawByteStr {
    /// Number of `#` delimiters used
    n_hashes: usize,
  },

  /// Raw C string literal (raw + C string combined)
  ///
  /// Added in Rust 1.77.
  ///
  /// # Examples
  /// ```rust
  /// cr"raw c string"
  /// cr#"with "quotes""#
  /// ```
  RawCStr {
    /// Number of `#` delimiters used
    n_hashes: usize,
  },
}

// ============================================================================
// MAIN TOKEN KIND ENUM
// ============================================================================

/// The kind of token produced by the lexer
///
/// This enum represents every possible token in Rust's syntax, including:
/// - **Comments and whitespace** (trivia tokens)
/// - **Identifiers and lifetimes**
/// - **Literals** (numbers, strings, characters)
/// - **Keywords** (control flow, declarations, modifiers)
/// - **Operators and punctuation**
/// - **Special tokens** (EOF, errors)
///
/// The lexer produces a stream of these tokens, which the parser then
/// consumes to build an AST.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
  // =========================================================================
  // COMMENTS & WHITESPACE (Trivia)
  // =========================================================================
  /// Line comment: `//` or `///` or `//!`
  ///
  /// # Examples
  /// ```rust
  /// // regular comment
  /// /// outer doc comment
  /// //! inner doc comment
  /// ```
  LineComment {
    /// Some(DocStyle) if this is a doc comment, None for regular comments
    doc_style: Option<DocStyle>,
  },

  /// Block comment: `/* */` or `/** */` or `/*! */`
  ///
  /// # Examples
  /// ```rust
  /// /* regular comment */
  /// /** outer doc comment */
  /// /*! inner doc comment */
  /// /* unterminated comment...
  /// ```
  BlockComment {
    /// Some(DocStyle) if this is a doc comment, None for regular comments
    doc_style: Option<DocStyle>,
    /// False if the closing `*/` is missing
    terminated: bool,
  },

  /// Whitespace: spaces, tabs, newlines, carriage returns
  ///
  /// Matches the pattern `[ \t\n\r]+`
  Whitespace,

  // =========================================================================
  // SHEBANG
  // =========================================================================
  /// Shebang line (only valid at the very start of a file)
  ///
  /// # Example
  /// ```rust
  /// #!/usr/bin/env rustrc
  /// #![allow(dead_code)]
  /// ```
  Shebang,

  // =========================================================================
  // IDENTIFIERS
  // =========================================================================
  /// Valid identifier
  ///
  /// Matches `[a-zA-Z_][a-zA-Z0-9_]*` or Unicode XID identifiers.
  ///
  /// # Examples
  /// ```rust
  /// foo
  /// _private
  /// CamelCase
  /// snake_case
  /// ```
  Ident,

  /// Raw identifier (allows using keywords as identifiers)
  ///
  /// # Examples
  /// ```rust
  /// r#type      // can use 'type' keyword as identifier
  /// r#match
  /// r#fn
  /// ```
  RawIdent,

  // =========================================================================
  // LIFETIMES
  // =========================================================================
  /// Lifetime parameter: `'a`, `'static`, `'_`
  ///
  /// # Examples
  /// ```rust
  /// 'a          // regular lifetime
  /// 'static     // built-in lifetime
  /// '_          // anonymous lifetime
  /// '0invalid   // starts_with_number = true (error)
  /// ```
  Lifetime {
    /// True if the lifetime name starts with a digit (invalid)
    starts_with_number: bool,
  },

  /// NOTE: this is a placeholder for future compatibility
  /// Raw lifetime (hypothetical - for future compatibility)
  RawLifetime,

  // =========================================================================
  // LITERALS
  // =========================================================================
  /// Any literal value: numbers, strings, chars, bytes
  ///
  /// # Examples
  /// ```rust
  /// 42          // Integer literal
  /// 3.14        // Float literal
  /// 'x'         // Char literal
  /// "hello"     // Str literal
  /// 100_u32     // Int with suffix
  /// ```
  Literal {
    /// The specific kind of literal
    kind: LiteralKind,
  },

  // =========================================================================
  // KEYWORDS
  // =========================================================================
  KwGen, // gen

  // Control Flow Keywords
  KwIf,       // if
  KwElse,     // else
  KwMatch,    // match
  KwLoop,     // loop
  KwWhile,    // while
  KwFor,      // for
  KwBreak,    // break
  KwContinue, // continue
  KwReturn,   // return

  // Declaration Keywords
  KwLet,     // let
  KwFn,      // fn
  KwStruct,  // struct
  KwEnum,    // enum
  KwUnion,   // union
  KwTrait,   // trait
  KwImpl,    // impl
  KwType,    // type
  KwMod,     // mod
  KwUse,     // use
  KwConst,   // const
  KwStatic,  // static
  KwExtern,  // extern
  KwMacro,   // macro (2.0 / reserved)
  KwAuto,    // auto (contextual in real Rust; treated as keyword here)
  KwDefault, // default (contextual in real Rust; treated as keyword here)

  // Modifier Keywords
  KwPub,    // pub
  KwMut,    // mut
  KwRef,    // ref
  KwMove,   // move
  KwUnsafe, // unsafe
  KwAsync,  // async
  KwAwait,  // await
  KwDyn,    // dyn

  // Special Identifiers
  KwSelf,     // self (value)
  KwSelfType, // Self (type)
  KwSuper,    // super
  KwCrate,    // crate

  // Literal Keywords
  KwTrue,  // true
  KwFalse, // false

  // Other Keywords
  KwAs,    // as
  KwIn,    // in
  KwWhere, // where

  // Reserved Keywords (not yet used, but reserved for future use)
  KwAbstract, // abstract
  KwBecome,   // become
  KwBox,      // box
  KwDo,       // do
  KwFinal,    // final
  KwOverride, // override
  //KwPriv,   // priv
  KwTry,     // try
  KwTypeof,  // typeof
  KwUnsized, // unsized
  KwVirtual, // virtual
  KwYield,   // yield

  // =========================================================================
  // PUNCTUATION & DELIMITERS
  // =========================================================================
  Semi,         // ;
  Comma,        // ,
  Dot,          // .
  OpenParen,    // (
  CloseParen,   // )
  OpenBrace,    // {
  CloseBrace,   // }
  OpenBracket,  // [
  CloseBracket, // ]
  At,           // @
  Pound,        // #
  Tilde,        // ~
  Question,     // ?
  Colon,        // :
  Dollar,       // $

  // =========================================================================
  // OPERATORS (Single-character and Compound)
  // =========================================================================

  // Assignment & Comparison
  Eq,   // =
  EqEq, // ==
  Ne,   // !=
  Lt,   // <
  Le,   // <=
  Gt,   // >
  Ge,   // >=

  // Arithmetic
  Plus,    // +
  Minus,   // -
  Star,    // *
  Slash,   // /
  Percent, // %

  // Bitwise & Logical
  And,   // &
  Or,    // |
  Caret, // ^
  Bang,  // !

  // Compound Assignment
  PlusEq,       // +=
  MinusEq,      // -=
  StarEq,       // *=
  SlashEq,      // /=
  PercentEq,    // %=
  AndEq,        // &=
  OrEq,         // |=
  CaretEq,      // ^=
  ShiftLeftEq,  // <<=
  ShiftRightEq, // >>=

  // Special Operators
  ColonColon, // :: (path separator)
  ThinArrow,  // -> (return type, match arm)
  FatArrow,   // => (match arm)
  DotDot,     // .. (range, struct update)
  DotDotEq,   // ..= (inclusive range)

  /// End of file
  Eof,
}

// ============================================================================
// IMPLEMENTATION METHODS
// ============================================================================

impl TokenKind {
  /// Returns true if this token can start an expression.
  ///
  /// This is essentially the same as [`can_start_expr`], but used where
  /// the longer name reads better.
  pub fn can_start_expression(&self) -> bool {
    matches!(
      self,
      Ident
      | RawIdent
      | Literal { .. }

      // grouping and array and block and struct literal
      | OpenParen
      | OpenBracket
      | OpenBrace

      // closure
      | Or
      | KwMove

      // unary operators
      | Minus
      | Star
      | Bang
      | And

      // control flow expressions
      | KwIf
      | KwMatch
      | KwWhile
      | KwLoop
      | KwFor
      | KwReturn
      | KwBreak
      | KwContinue

      // lifetime at expression position (rare but valid, e.g. break 'lbl)
      | Lifetime { .. }

      // path starting with ::
      | ColonColon
      | KwSelf
      | KwSelfType
      | KwSuper
      | KwCrate

      | KwTry
      | KwConst
      | KwAsync
      | KwUnsafe
      | KwExtern
      | Lt
    )
  }

  pub fn can_continue_expression(&self) -> bool {
    matches!(
      self,
      // postfix ops
      Dot            // .field, .await, method calls, tuple index
      | OpenParen  // call
      | OpenBracket// index
      | Question   // ?

    // cast
      | KwAs

    // binary ops
      | Plus
      | Minus
      | Star
      | Slash
      | Percent
      | Caret
      | Or
      | And

    // comparison
      | EqEq
      | Ne
      | Lt
      | Le
      | Gt
      | Ge

    // compound assign
      | PlusEq
      | MinusEq
      | StarEq
      | SlashEq
      | PercentEq
      | AndEq
      | OrEq
      | CaretEq
      | ShiftLeftEq
      | ShiftRightEq

    // ranges
      | DotDot
      | DotDotEq

    // call and match arm helpers
      | ThinArrow
      | FatArrow
    )
  }

  pub fn can_continue_expression_and_not(&self, and: TokenKind) -> bool {
    self.can_continue_expression() && *self != and
  }

  pub fn can_continue_expression_or(&self, or: TokenKind) -> bool {
    self.can_continue_expression() || *self == or
  }

  pub fn can_start_expression_and_not(&self, and: TokenKind) -> bool {
    self.can_start_expression() && *self != and
  }

  pub fn can_start_expression_or(&self, or: TokenKind) -> bool {
    self.can_start_expression() || *self == or
  }

  pub fn is_binary_operator(&self) -> bool {
    matches!(self, EqEq | Ne | Lt | Le | Gt | Ge)
  }

  /// Returns true if this token is trivia (whitespace or comment)
  ///
  /// Trivia tokens are typically skipped during parsing but preserved
  /// for source formatting tools, IDEs, and error diagnostics.
  ///
  /// # Examples
  /// ```rust
  /// assert!(Whitespace.is_trivia());
  /// assert!(LineComment { doc_style: None }.is_trivia());
  /// assert!(!Ident.is_trivia());
  /// ```
  pub fn is_trivia(&self) -> bool {
    matches!(self, Whitespace | LineComment { .. } | BlockComment { .. })
  }

  /// Returns true if this token is a literal
  ///
  /// # Examples
  /// ```rust
  /// assert!(
  ///   Literal {
  ///     kind: LiteralKind::Integer {
  ///       base: Base::Decimal,
  ///       empty_int: false,
  ///       suffix_start: 0,
  ///     },
  ///   }
  ///   .is_literal()
  /// );
  /// assert!(!Ident.is_literal());
  /// ```
  pub fn is_literal(&self) -> bool {
    matches!(self, Literal { .. })
  }

  /// Returns true if this token is a keyword
  ///
  /// # Examples
  /// ```rust
  /// assert!(KwFn.is_keyword());
  /// assert!(KwLet.is_keyword());
  /// assert!(!Ident.is_keyword());
  /// ```
  pub fn is_keyword(&self) -> bool {
    matches!(
      self,
      KwAs
        | KwBreak
        | KwConst
        | KwContinue
        | KwCrate
        | KwElse
        | KwEnum
        | KwExtern
        | KwFalse
        | KwFn
        | KwFor
        | KwIf
        | KwImpl
        | KwIn
        | KwLet
        | KwLoop
        | KwMatch
        | KwMod
        | KwMove
        | KwMut
        | KwPub
        | KwRef
        | KwReturn
        | KwSelf
        | KwSelfType
        | KwStatic
        | KwStruct
        | KwSuper
        | KwTrait
        | KwTrue
        | KwType
        | KwUnsafe
        | KwUse
        | KwWhere
        | KwWhile
        | KwAsync
        | KwAwait
        | KwDyn
        | KwAbstract
        | KwBecome
        | KwFinal
        | KwMacro
        | KwOverride
        | KwTry
        | KwTypeof
        | KwUnion
        | KwUnsized
        | KwYield
        | KwBox
        | KwDo
        | KwVirtual
    )
  }
}

impl LiteralKind {
  /// Returns true if this literal is a string-like type
  ///
  /// Includes all string variants: regular strings, byte strings,
  /// C strings, and their raw variants.
  ///
  /// # Examples
  /// ```rust
  /// assert!(LiteralKind::Str.is_string_like());
  /// assert!(LiteralKind::RawStr { n_hashes: 1 }.is_string_like());
  /// assert!(
  ///   !LiteralKind::Integer {
  ///     base: Base::Decimal,
  ///     empty_int: false,
  ///     suffix_start: 0,
  ///   }
  ///   .is_string_like()
  /// );
  /// ```
  pub fn is_string_like(&self) -> bool {
    matches!(
      self,
      LiteralKind::Str
        | LiteralKind::ByteStr
        | LiteralKind::CStr
        | LiteralKind::RawStr { .. }
        | LiteralKind::RawByteStr { .. }
        | LiteralKind::RawCStr { .. }
    )
  }

  /// Returns true if this literal is a numeric type
  ///
  /// # Examples
  /// ```rust
  /// assert!(
  ///   LiteralKind::Integer {
  ///     base: Base::Decimal,
  ///     empty_int: false,
  ///     suffix_start: 0,
  ///   }
  ///   .is_numeric()
  /// );
  /// assert!(
  ///   LiteralKind::Float {
  ///     base: Base::Decimal,
  ///     suffix_start: 0,
  ///   }
  ///   .is_numeric()
  /// );
  /// assert!(!LiteralKind::Char.is_numeric());
  /// ```
  pub fn is_numeric(&self) -> bool {
    matches!(
      self,
      LiteralKind::Integer { .. } | LiteralKind::Float { .. }
    )
  }

  /// Returns true if this literal is a character-like type
  ///
  /// # Examples
  /// ```rust
  /// assert!(LiteralKind::Char.is_char_like());
  /// assert!(LiteralKind::Byte.is_char_like());
  /// assert!(!LiteralKind::Str.is_char_like());
  /// ```
  pub fn is_char_like(&self) -> bool {
    matches!(self, LiteralKind::Char | LiteralKind::Byte)
  }
}
