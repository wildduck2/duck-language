// Matches: visibility -> "pub" ("(" ("crate" | "self" | "super" | "in" simplePath) ")")?

use crate::ast::Path;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Visibility {
  Public,
  PublicCrate,
  PublicSuper,
  PublicSelf,
  PublicIn(Path),
  Private,
}
