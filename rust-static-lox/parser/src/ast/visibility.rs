// Matches: visibility -> "pub" ("(" ("crate" | "self" | "super" | "in" simplePath) ")")?

use crate::ast::Path;

#[derive(Debug, Clone)]
pub(crate) enum Visibility {
  Public,
  PublicCrate,
  PublicSuper,
  PublicSelf,
  PublicIn(Path),
  Private,
}
