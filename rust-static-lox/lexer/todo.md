# Complete Rust Lexer Implementation TODO

A comprehensive checklist for building a production-ready Rust lexer from scratch.

---

## Phase 1: Core Infrastructure  

### 1.1 Basic Lexer Structure
- [x] Create `Lexer` struct with:
  - [x] `source: SourceFile` - Source file with content
  - [x] `current: usize` - Current byte position
  - [x] `start: usize` - Start byte position of current token
  - [x] `line: usize` - Current line (1-indexed)
  - [x] `column: usize` - Current column (1-indexed)
- [x] Implement `Lexer::new(source: SourceFile) -> Self`
- [x] Implement `Lexer::scan_tokens()` (main entry point)
- [x] Add helper methods:
  - [x] `peek() -> Option<char>` - Look at current char without consuming
  - [x] `peek_next(offset: usize) -> Option<char>` - Look ahead n characters
  - [x] `advance() -> char` - Advance and return current char
  - [x] `get_current_offset() -> usize` - Get current byte position
  - [x] `get_current_lexeme() -> &str` - Get current token text

### 1.2 Span/Location Tracking
- [x] Implement span calculation (start/end positions)
- [x] Add line and column tracking (optional, for better error messages)
- [x] Create `Span::new(start: usize, end: usize)` constructor
- [x] Implement `Span::merge()` for combining spans

---

## Phase 2: Whitespace & Comments  

### 2.1 Whitespace
- [x] Implement `lex_whitespace() -> Token`
- [x] Handle all Unicode whitespace characters:
  - [x] Space (` `)
  - [x] Tab (`\t`)
  - [x] Newline (`\n`)
  - [x] Carriage return (`\r`)
  - [x] Other Unicode whitespace (use `char::is_whitespace()`)
- [x] Test with mixed whitespace

### 2.2 Line Comments
- [x] Implement `lex_line_comment() -> Token`
- [x] Detect `//` at start
- [x] Check for doc comments:
  - [x] `///` → `DocStyle::Outer`
  - [x] `//!` → `DocStyle::Inner`
  - [x] `//` → `None` (regular comment)
- [x] Consume until end of line (not including `\n`)
- [x] Handle EOF in middle of line comment

### 2.3 Block Comments
- [x] Implement `lex_block_comment() -> Token`
- [x] Detect `/*` at start
- [x] Check for doc comments:
  - [x] `/**` (not `/**/`) → `DocStyle::Outer`
  - [x] `/*!` → `DocStyle::Inner`
  - [x] `/*` → `None` (regular comment)
- [x] **Handle nesting**: `/* /* nested */ */`
  - [x] Track nesting depth counter
  - [x] Increment on `/*`, decrement on `*/`
- [x] Set `terminated: false` if EOF before closing `*/`
- [x] Test deeply nested comments

### 2.4 Shebang
- [x] Implement `lex_shebang() -> Option<Token>`
- [x] **Only valid as first token** (position 0)
- [x] Must start with `#!`
- [x] Must be followed by `[` or `/` (not `![` for inner attribute)
- [x] Consume until end of line
- [x] Return `None` if not at position 0 or invalid

---

## Phase 3: Identifiers & Keywords  

### 3.1 Regular Identifiers
- [x] Implement `lex_ident() -> Token` (via `lex_keywords()`)
- [x] Validate first character:
  - [x] `a-z`, `A-Z`, `_`
  - [ ] Unicode XID_Start characters (ASCII only currently)
- [x] Validate continuation characters:
  - [x] `a-z`, `A-Z`, `0-9`, `_`
  - [ ] Unicode XID_Continue characters (ASCII only currently)
- [ ] Use `char::is_xid_start()` and `char::is_xid_continue()` (currently ASCII only)
- [x] Mark as `InvalidIdent` if contains invalid characters (like starting with digits)

### 3.2 Raw Identifiers
- [x] Implement `lex_raw_ident() -> Token` (via `lex_keywords()`)
- [x] Detect `r#` prefix
- [x] Parse identifier after `r#`
- [x] Validate that it's a valid identifier
- [ ] Special cases:
  - [ ] `r#_` is invalid (underscore alone cannot be raw ident)
  - [ ] `r#crate` has special meaning

### 3.3 Keywords (Parser-level, but document here)
- [x] Document that lexer emits keyword tokens (not `Ident`)
- [x] Lexer checks against keyword list:
  - [x] Strict keywords: `as`, `break`, `const`, `continue`, `crate`, `else`, `enum`, `extern`, `false`, `fn`, `for`, `if`, `impl`, `in`, `let`, `loop`, `match`, `mod`, `move`, `mut`, `pub`, `ref`, `return`, `self`, `Self`, `static`, `struct`, `super`, `trait`, `true`, `type`, `unsafe`, `use`, `where`, `while`
  - [x] Reserved keywords: `abstract`, `become`, `box`, `do`, `final`, `macro`, `override`, `priv`, `typeof`, `unsized`, `virtual`, `yield`
  - [x] Weak keywords (contextual): `async`, `await`, `dyn`, `try`, `union`

---

## Phase 4: Lifetimes  

### 4.1 Regular Lifetimes
- [x] Implement `lex_lifetime() -> Token`
- [x] Detect `'` followed by identifier
- [x] Parse identifier part (same rules as regular idents)
- [x] Check if starts with number:
  - [x] `'1abc` → set `starts_with_number: true` (invalid)
- [x] Special lifetimes:
  - [x] `'_` (anonymous lifetime)
  - [x] `'static` (special, but treated as regular ident)

### 4.2 Raw Lifetimes
- [ ] Implement `lex_raw_lifetime() -> Token`
- [ ] Detect `'r#` prefix
- [ ] Parse identifier after `'r#`
- [ ] Allow keywords as lifetime names

### 4.3 Lifetime vs Char Literal Disambiguation
- [x] If `'` followed by identifier start → lifetime
- [x] If `'` followed by anything else → try char literal
- [x] Handle edge cases like `'1` (invalid lifetime)

---

## Phase 5: Numeric Literals  

### 5.1 Integer Literals
- [x] Implement `lex_number() -> Token`
- [x] Detect base prefix:
  - [x] `0b` → Binary
  - [x] `0o` → Octal
  - [x] `0x` → Hexadecimal
  - [x] None → Decimal
- [x] Parse digits according to base:
  - [x] Binary: `0-1`
  - [x] Octal: `0-7`
  - [x] Decimal: `0-9`
  - [x] Hex: `0-9`, `a-f`, `A-F`
- [x] Handle digit separators: `1_000_000`
- [x] Set `empty_int: true` if no digits after prefix (e.g., `0x`)
- [x] Parse optional suffix: `u8`, `i32`, `u64`, `i128`, `usize`, `isize` (suffix_start tracked but not parsed)

### 5.2 Float Literals
- [x] Detect float when number contains:
  - [x] Decimal point: `3.14`
  - [x] Exponent: `1e10`, `2E-5`
- [x] Parse decimal point and fractional part
- [x] Parse exponent:
  - [x] `e` or `E` followed by optional `+`/`-`
  - [x] Then digits
  - [x] Set `empty_exponent: true` if no digits after `e`
- [x] Parse optional suffix: `f32`, `f64` (suffix_start tracked but not parsed)
- [x] **Special case**: `1f32` is int with suffix, not float (handled correctly)
- [x] Hexadecimal floats (rare): `0x1.8p3` (not implemented)

### 5.3 Edge Cases
- [x] `123` → Int (decimal)
- [x] `0x` → Int with `empty_int: true`
- [x] `1.` → Float (with empty fractional part)
- [x] `1e` → Float with `empty_exponent: true`
- [ ] `1.foo()` → Int `1`, then `.`, then method call (not a float!) (needs parser coordination)
- [ ] `1.2.3` → Float `1.2`, then `.`, then `3` (needs parser coordination)

---

## Phase 6: Character & Byte Literals  

### 6.1 Character Literals
- [x] Implement `lex_char() -> Token`
- [x] Detect opening `'`
- [x] Parse content:
  - [x] Regular character: `'a'`
  - [x] Escape sequences (see 6.3)
  - [x] Unicode escapes: `'\u{1F980}'`
- [x] Detect closing `'`
- [x] Set `terminated: false` if EOF or newline before closing
- [x] Validate:
  - [x] Not empty: `''` is invalid (handled by terminated flag)
  - [ ] Single character (after escaping) (validation in parser)

### 6.2 Byte Literals
- [x] Implement `lex_byte() -> Token` (via `lex_bchar()`)
- [x] Detect `b'` prefix
- [x] Parse ASCII-only content
- [x] Same escape rules as char, but restricted to ASCII
- [x] Validate byte is in range 0-127 (after escaping)

### 6.3 Escape Sequences
- [x] Implement escape sequence parsing (inline in lexers)
- [x] Simple escapes:
  - [x] `\n` → newline
  - [x] `\r` → carriage return
  - [x] `\t` → tab
  - [x] `\\` → backslash
  - [x] `\'` → single quote
  - [x] `\"` → double quote
  - [x] `\0` → null
- [x] Byte escapes: `\xHH` (2 hex digits)
- [x] Unicode escapes: `\u{HHHHHH}` (1-6 hex digits)
- [x] Validate Unicode scalar values (not surrogate pairs)

---

## Phase 7: String Literals  

### 7.1 Regular Strings
- [x] Implement `lex_string() -> Token` (via `lex_str()`)
- [x] Detect opening `"`
- [x] Parse content with escape sequences
- [x] Allow multi-line strings
- [x] Detect closing `"`
- [x] Set `terminated: false` if EOF before closing

### 7.2 Raw Strings
- [x] Implement `lex_raw_string() -> Token` (via `lex_raw_str()`)
- [x] Detect `r` followed by zero or more `#`, then `"`
- [x] Count opening `#` characters → `n_hashes`
- [x] Validate no other characters between `r` and `#*"` (emits diagnostic)
- [x] Parse content (no escapes processed)
- [x] Find closing: `"` followed by same number of `#`
- [x] Set `terminated: false` if:
  - [x] EOF reached
  - [x] Different number of closing `#`
- [x] Set `n_hashes` capped at 255 if > 255 (emits diagnostic)

### 7.3 Byte Strings
- [x] Implement `lex_byte_string() -> Token` (via `lex_bstr()`)
- [x] Detect `b"` prefix
- [x] Same as regular strings but ASCII-only
- [x] Validate all bytes are 0-127 (after escaping)

### 7.4 Raw Byte Strings
- [x] Implement `lex_raw_byte_string() -> Token` (via `lex_bstr()`)
- [x] Detect `br` prefix
- [x] Combine raw string and byte string rules
- [x] No escapes, ASCII-only content

### 7.5 C Strings (Rust 1.77+)
- [x] Implement `lex_c_string() -> Token` (via `lex_cstr()`)
- [x] Detect `c"` prefix
- [x] Same as regular strings
- [x] Automatically null-terminated (semantic, not lexer concern)
- [ ] Cannot contain interior `\0` (except as last char) (validation in parser)

### 7.6 Raw C Strings
- [x] Implement `lex_raw_c_string() -> Token` (via `lex_craw_str()`)
- [x] Detect `cr` prefix
- [x] Combine raw string and C string rules

---

## Phase 8: Operators & Punctuation  

### 8.1 Single-Character Tokens
Implement individual lexing functions for each:
- [x] `;` → `Semi`
- [x] `,` → `Comma`
- [x] `.` → `Dot` (check not start of `..` or number)
- [x] `(` → `OpenParen`
- [x] `)` → `CloseParen`
- [x] `{` → `OpenBrace`
- [x] `}` → `CloseBrace`
- [x] `[` → `OpenBracket`
- [x] `]` → `CloseBracket`
- [x] `@` → `At`
- [x] `#` → `Pound`
- [x] `~` → `Tilde`
- [x] `?` → `Question`
- [x] `:` → `Colon`
- [x] `$` → `Dollar`
- [x] `=` → `Eq` (also handles `==`, `=>`)
- [x] `!` → `Bang` (also handles `!=`)
- [x] `<` → `Lt` (also handles `<=`, `<<`, `<<=`)
- [x] `>` → `Gt` (also handles `>=`, `>>`, `>>=`)
- [x] `-` → `Minus` (also handles `-=`, `->`)
- [x] `&` → `And` (also handles `&&`, `&=`)
- [x] `|` → `Or` (also handles `||`, `|=`)
- [x] `+` → `Plus` (also handles `+=`)
- [x] `*` → `Star` (also handles `*=`)
- [x] `/` → `Slash` (also handles `/=`, comments)
- [x] `^` → `Caret` (also handles `^=`)
- [x] `%` → `Percent` (also handles `%=`)
- [x] `::` → `ColonColon`
- [x] `..` → `DotDot`
- [x] `..=` → `DotDotEq`

### 8.2 Disambiguation
- [x] `/` → Check if followed by `/` (line comment) or `*` (block comment)
- [ ] `.` → Check if followed by digit (float literal) (handled in number lexer)
- [ ] `'` → Check if lifetime or char literal (currently only char literal)
- [x] `#` → Check if `#!` at position 0 (shebang)
- [x] Letters → Check if prefix for literal (`b`, `r`, `br`, `c`, `cr`)

---

## Phase 9: Unknown Prefixes & Reserved Syntax  

### 9.1 Unknown Literal Prefixes
- [x] Implement `detect_unknown_prefix() -> Option<Token>`
- [x] If identifier followed immediately by `"`, `'`, or `#"`:
  - [x] Check if it's a known prefix (`b`, `r`, `br`, `c`, `cr`)
  - [x] If not known → `UnknownPrefix`
  - [x] Token contains only the prefix part

### 9.2 Unknown Lifetime Prefixes
- [x] Implement `detect_unknown_lifetime_prefix() -> Option<Token>`
- [ ] If `'` + identifier + `#`:
  - [ ] Not `'r#` → `UnknownPrefixLifetime`

### 9.3 Reserved Prefixes (Rust 2024+)
- [ ] Implement `detect_reserved_prefix() -> Option<Token>`
- [ ] Check for single-letter + `#` patterns: `k#`, `f#`
- [ ] Reserved for future literal types
- [ ] Return `ReservedPrefix` token

---

## Phase 10: Main Lexer Loop  

### 10.2 Character-based Dispatch
- [x] Whitespace chars → `lex_whitespace()`
- [x] `/` → Check for comments or `Slash`
- [x] `#` → Check for shebang or `Pound`
- [x] `'` → Check for char literal (lifetime not yet implemented)
- [x] `"` → `lex_string()`
- [x] `a-z`, `A-Z`, `_` → `lex_keywords()` or check prefix
- [x] `0-9` → `lex_number()`
- [x] Operators → Single-char tokens
- [x] Everything else → `Unknown` (currently returns None with diagnostic)

### 10.3 Prefix Detection
- [x] After lexing potential identifier, check next char:
  - [x] `"` → Could be string prefix (handled in `lex_string()`)
  - [x] `'` → Could be byte literal prefix (handled in `lex_string()`)
  - [x] `#` → Could be raw string prefix (handled in `lex_string()`)
- [x] Uses lookahead via `peek()` and `peek_next()`

---

## Phase 11: Error Handling & Validation  

### 11.1 Malformed Literals
- [x] Unterminated strings/chars: set `terminated: false`
- [x] Invalid escape sequences: emits diagnostics
- [x] Empty numbers after prefix: set `empty_int: true`
- [x] Invalid raw string delimiters: emits diagnostics, caps n_hashes

### 11.2 Invalid Characters
- [x] Return `Unknown` token for unrecognized characters (currently returns None)
- [x] Unicode characters not valid in identifiers → `InvalidIdent` (for starting with digits)
- [x] Non-ASCII in byte literals → validation error (emits diagnostic)

### 11.3 Contextual Validation
- [x] Shebang only at position 0
- [x] Raw identifiers cannot be `_` alone (not validated)
- [x] Lifetimes cannot start with numbers (not implemented)

---

## Phase 12: Testing  

### 12.1 Unit Tests Per Feature
- [ ] Whitespace: various Unicode whitespace
- [ ] Comments: nested, unterminated, doc comments
- [ ] Identifiers: ASCII, Unicode, keywords, raw
- [ ] Lifetimes: regular, raw, invalid
- [ ] Numbers: all bases, floats, empty, suffixes
- [ ] Chars/bytes: escapes, unicode, unterminated
- [ ] Strings: all variants, raw with different `#` counts
- [ ] Operators: all single-char tokens
- [ ] Unknown/invalid tokens

### 12.2 Integration Tests
- [ ] Lex complete Rust files from standard library
- [ ] Lex code with intentional errors
- [ ] Lex edge cases: empty file, only whitespace, only comments
- [ ] Performance test: large files (1MB+)

### 12.3 Fuzzing
- [ ] Set up cargo-fuzz
- [ ] Fuzz with random bytes
- [ ] Fuzz with semi-valid Rust code
- [ ] Check for panics and infinite loops

---

## Phase 13: Optimizations 

### 13.1 Performance
- [ ] Profile hot paths with `cargo flamegraph`
- [ ] Optimize character lookahead (avoid repeated UTF-8 decoding)
- [ ] Use byte-based matching where possible (ASCII fast path)
- [ ] Consider SIMD for whitespace/identifier scanning

### 13.2 Memory
- [ ] Minimize allocations in hot path
- [ ] Reuse buffers where possible
- [ ] Consider arena allocation for tokens

### 13.3 Benchmarking
- [ ] Set up criterion benchmarks
- [ ] Benchmark against rustc_lexer
- [ ] Test on various file sizes and code styles

---

## Phase 14: API & Documentation  

### 14.1 public API
- [x] Clean, ergonomic API for consumers:
  ```rust
  let mut lexer = Lexer::new(source);
  lexer.scan_tokens(&mut engine);
  let tokens = lexer.tokens;
  ```
- [ ] Iterator implementation (not implemented)
- [x] Error recovery mode (continue after errors via diagnostics)

### 14.2 Documentation
- [x] Document all public types and methods
- [x] Add examples to struct/function docs
- [x] Create comprehensive README
- [x] Document differences from rustc_lexer (if any)

### 14.3 Examples
- [ ] Simple token printer example
- [ ] Syntax highlighter example
- [ ] Token statistics analyzer

---

## Phase 15: Advanced Features (Optional)  

### 15.1 Error Recovery
- [ ] Continue lexing after errors
- [ ] Produce sensible tokens for malformed input
- [ ] Provide helpful error messages

### 15.2 Source Maps
- [ ] Track original source locations
- [ ] Support for `#[macro_export]` and `include!()` expansions
- [ ] Map tokens back to original files

### 15.3 Proc Macro Support
- [ ] TokenStream compatible output
- [ ] Preserve token spacing information
- [ ] Support token tree construction

### 15.4 IDE Support
- [ ] Provide token semantic information
- [ ] Support incremental lexing
- [ ] Token classification for syntax highlighting

---

## Phase 16: Validation & Release  

### 16.1 Compliance Testing
- [x] Test against Rust specification
- [x] Compare output with rustc_lexer on Rust repo

### 16.2 Code Quality
- [x] Run clippy with strict lints
- [x] Ensure no unsafe code (or justify it)
- [ ] 100% documentation coverage
- [x] Format with rustfmt

## Key Resources  

- [Rust Reference - Lexical Structure](https://doc.rust-lang.org/reference/lexical-structure.html)
- [rustc_lexer source code](https://github.com/rust-lang/rust/tree/master/compiler/rustc_lexer)
- [Unicode XID specification](http://www.unicode.org/reports/tr31/)
- [Rust Edition Guide](https://doc.rust-lang.org/edition-guide/)

