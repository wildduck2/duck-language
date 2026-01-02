use crate::types::Severity;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticError {
  // ============================================================================
  // General Errors (E0001-E0099)
  // ============================================================================
  CodeNotFound,
  InvalidArguments,

  // ============================================================================
  // Lexer Errors (E0100-E0199)
  // ============================================================================
  // Shebang and character errors
  InvalidShebang,
  InvalidCharacter,

  // String and character literal errors
  UnterminatedString,
  TooManyRawStrHashes,
  InvalidStringStart,
  InvalidEscape,

  // Prefix errors
  UnknownPrefix,
  ReservedPrefix,

  // Lifetime errors
  InvalidLifetime,

  // Number literal errors
  InvalidInteger,

  // Identifier errors
  InvalidIdentifier,

  // ============================================================================
  // Parser Errors (E0200-E0299)
  // ============================================================================
  // Token and syntax errors
  UnexpectedToken,
  MissingClosingBracket,

  // Literal errors
  InvalidLiteral,
  EmptyChar,

  // Visibility and naming errors
  InvalidVisibilityRestriction,
  InvalidNameIdentifier,

  // Type errors
  InvalidType,
  InvalidMutabilityInField,
  InvalidPointerType,
  InvalidMutability,

  // Lifetime errors
  UnexpectedLifetime,

  // Generic and trait bound errors
  InvalidWherePredicate,
  InvalidWhereClause,
  InvalidTraitBound,
  InvalidTraitBoundModifier,
  InvalidComma,
  EmptyGenericArgs,
  InvalidGenericArg,

  // Path errors
  InvalidPathSegment,

  // Syntax and structure errors
  InvalidTrailingComma,

  // Function and parameter errors
  InvalidAbi,
  InvalidVariadic,
  InvalidSelfParam,
  InvalidSelfInFreeFunction,

  // Control flow errors
  ReturnOutsideFunction,
  ContinueOutsideLoop,
  BreakOutsideLoop,
  InvalidCondition,

  // Block and flavor errors
  ExpectedBlockAfterFlavor,
  InvalidBlockFlavorContext,
  InvalidFlavorOrder,

  // Semantic errors
  UndefinedVariable,
  MismatchedTypes,
  TraitNotSatisfied,
  BorrowCheckerViolation,
  InvalidConstKeyword,
}

impl DiagnosticError {
  pub fn code(&self) -> &str {
    match self {
      // General Errors (E0001-E0099)
      Self::CodeNotFound => "E0001",
      Self::InvalidArguments => "E0002",

      // Lexer Errors (E0100-E0199)
      // Shebang and character errors
      Self::InvalidShebang => "E0100",
      Self::InvalidCharacter => "E0101",

      // String and character literal errors
      Self::UnterminatedString => "E0102",
      Self::TooManyRawStrHashes => "E0103",
      Self::InvalidStringStart => "E0104",
      Self::InvalidEscape => "E0105",

      // Prefix errors
      Self::UnknownPrefix => "E0106",
      Self::ReservedPrefix => "E0107",

      // Lifetime errors
      Self::InvalidLifetime => "E0108",

      // Number literal errors
      Self::InvalidInteger => "E0109",

      // Identifier errors
      Self::InvalidIdentifier => "E0110",

      // Parser Errors (E0200-E0299)
      // Token and syntax errors
      Self::UnexpectedToken => "E0200",
      Self::MissingClosingBracket => "E0203",

      // Literal errors
      Self::InvalidLiteral => "E0201",
      Self::EmptyChar => "E0202",

      // Visibility and naming errors
      Self::InvalidVisibilityRestriction => "E0204",
      Self::InvalidNameIdentifier => "E0205",

      // Type errors
      Self::InvalidType => "E0207",
      Self::InvalidMutabilityInField => "E0208",
      Self::InvalidPointerType => "E0209",
      Self::InvalidMutability => "E0211",

      // Lifetime errors
      Self::UnexpectedLifetime => "E0206",

      // Generic and trait bound errors
      Self::InvalidWherePredicate => "E0210",
      Self::InvalidWhereClause => "E0232",
      Self::InvalidTraitBound => "E0233",
      Self::InvalidTraitBoundModifier => "E0226",
      Self::InvalidComma => "E0231",
      Self::EmptyGenericArgs => "E0228",
      Self::InvalidGenericArg => "E0229",

      // Path errors
      Self::InvalidPathSegment => "E0230",

      // Syntax and structure errors
      Self::InvalidTrailingComma => "E0229",

      // Function and parameter errors
      Self::InvalidAbi => "E0224",
      Self::InvalidVariadic => "E0225",
      Self::InvalidSelfParam => "E0227",
      Self::InvalidSelfInFreeFunction => "E0223",

      // Control flow errors
      Self::ReturnOutsideFunction => "E0217",
      Self::ContinueOutsideLoop => "E0218",
      Self::BreakOutsideLoop => "E0219",
      Self::InvalidCondition => "E0222",

      // Block and flavor errors
      Self::ExpectedBlockAfterFlavor => "E0216",
      Self::InvalidBlockFlavorContext => "E0220",
      Self::InvalidFlavorOrder => "E0221",

      // Semantic errors
      Self::UndefinedVariable => "E0212",
      Self::MismatchedTypes => "E0213",
      Self::TraitNotSatisfied => "E0214",
      Self::BorrowCheckerViolation => "E0215",

      Self::InvalidConstKeyword => "E0216",
    }
  }

  pub fn severity(&self) -> Severity {
    Severity::Error
  }
}
