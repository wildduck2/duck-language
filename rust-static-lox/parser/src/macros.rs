/// Attempts to match any of the given token kinds and consumes it on success.
///
/// Usage:
///     match_and_consume!(self, engine, TokenKind::DotDot);
///     match_and_consume!(self, engine, TokenKind::DotDot, TokenKind::DotDotEq);
#[macro_export]
macro_rules! match_and_consume {
  ($self:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {{
    let cur = $self.current_token().kind;

    // Check the pattern match like `matches!`
    if matches!(cur, $pattern $(if $guard)?) {
        $self.advance();
        Ok(true)
    } else {
        Ok(false)
    }
  }};
}
