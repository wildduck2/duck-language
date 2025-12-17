// Matches: visibility -> "pub" ("(" ("crate" | "self" | "super" | "in" simplePath) ")")?

use crate::ast::Path;

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
  Public,
  PublicCrate,
  PublicSuper,
  PublicSelf,
  PublicIn(Path),
  Private,
}
