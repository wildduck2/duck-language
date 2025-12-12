use crate::types::Severity;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticError {
  // general
  CodeNotFound,
  InvalidArguments,

  // lexer
  InvalidShebang,
  InvalidCharacter,
  UnterminatedString,
  TooManyRawStrHashes,
  InvalidStringStart,
  InvalidEscape,
  UnknownPrefix,
  ReservedPrefix,
  InvalidLifetime,
  InvalidInteger,

  // parser
  UnexpectedToken,
  InvalidLiteral,
  EmptyChar,
  MissingClosingBracket,
  InvalidVisibilityRestriction,
  InvalidNameIdentifier,
  UnexpectedLifetime,
  InvalidType,
  InvalidMutabilityInField,
  InvalidPointerType,
  InvalidWherePredicate,
  InvalidMutability,
  UndefinedVariable,
  MismatchedTypes,
  TraitNotSatisfied,
  BorrowCheckerViolation,
  ExpectedBlockAfterFlavor,
  ReturnOutsideFunction,
  ContinueOutsideLoop,
  BreakOutsideLoop,
  InvalidBlockFlavorContext,
  InvalidFlavorOrder,
  InvalidCondition,
  InvalidSelfInFreeFunction,
  InvalidAbi,
  InvalidVariadic,
}

impl DiagnosticError {
  pub fn code(&self) -> &str {
    match self {
      Self::CodeNotFound => "E0001",
      Self::InvalidArguments => "E0002",

      Self::InvalidShebang => "E0003",
      Self::InvalidCharacter => "E0004",
      Self::UnterminatedString => "E0005",
      Self::TooManyRawStrHashes => "E0006",
      Self::InvalidStringStart => "E0007",
      Self::InvalidEscape => "E0008",
      Self::UnknownPrefix => "E0009",
      Self::ReservedPrefix => "E0010",
      Self::InvalidLifetime => "E0011",
      Self::InvalidInteger => "E0012",

      Self::UnexpectedToken => "E0013",
      Self::InvalidLiteral => "E0014",
      Self::EmptyChar => "E0015",
      Self::MissingClosingBracket => "E0016",
      Self::InvalidVisibilityRestriction => "E0017",
      Self::InvalidNameIdentifier => "E0018",
      Self::UnexpectedLifetime => "E0019",
      Self::InvalidType => "E0020",
      Self::InvalidMutabilityInField => "E0021",
      Self::InvalidPointerType => "E0022",
      Self::InvalidWherePredicate => "E0023",
      Self::InvalidMutability => "E0024",
      Self::ExpectedBlockAfterFlavor => "E0029",
      Self::ReturnOutsideFunction => "E0029",
      Self::ContinueOutsideLoop => "E0029",
      Self::BreakOutsideLoop => "E0029",
      Self::InvalidBlockFlavorContext => "E0029",
      Self::InvalidFlavorOrder => "E0029",
      Self::InvalidCondition => "E0029",
      Self::InvalidSelfInFreeFunction => "E0029",
      Self::InvalidAbi => "E0029",
      Self::InvalidVariadic => "E0029",

      Self::UndefinedVariable => "E0025",
      Self::MismatchedTypes => "E0026",
      Self::TraitNotSatisfied => "E0027",
      Self::BorrowCheckerViolation => "E0028",
    }
  }

  pub fn severity(&self) -> Severity {
    Severity::Error
  }
}
