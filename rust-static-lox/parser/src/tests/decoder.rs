use crate::decoder::Decoder;
use diagnostic::{DiagnosticEngine, Span};

fn create_span() -> Span {
  Span::new(0, 0)
}

fn create_engine() -> DiagnosticEngine {
  DiagnosticEngine::new()
}

fn test_decode_string(input: &str) -> Result<String, ()> {
  let mut engine = create_engine();
  Decoder::decode_string(input, "test.rs", create_span(), &mut engine)
}

fn test_decode_string_err(input: &str) -> bool {
  let mut engine = create_engine();
  Decoder::decode_string(input, "test.rs", create_span(), &mut engine).is_err()
}

fn test_decode_byte_string(input: &str) -> Result<Vec<u8>, ()> {
  let mut engine = create_engine();
  Decoder::decode_byte_string(input, "test.rs", create_span(), &mut engine)
}

fn test_decode_byte_string_err(input: &str) -> bool {
  let mut engine = create_engine();
  Decoder::decode_byte_string(input, "test.rs", create_span(), &mut engine).is_err()
}

fn test_decode_char(input: &str) -> Result<char, ()> {
  let mut engine = create_engine();
  Decoder::decode_char(input, "test.rs", create_span(), &mut engine)
}

fn test_decode_char_err(input: &str) -> bool {
  let mut engine = create_engine();
  Decoder::decode_char(input, "test.rs", create_span(), &mut engine).is_err()
}

fn test_decode_byte(input: &str) -> Result<u8, ()> {
  let mut engine = create_engine();
  Decoder::decode_byte(input, "test.rs", create_span(), &mut engine)
}

fn test_decode_byte_err(input: &str) -> bool {
  let mut engine = create_engine();
  Decoder::decode_byte(input, "test.rs", create_span(), &mut engine).is_err()
}

// decode_string tests
#[test]
fn test_decode_string_empty() {
  assert_eq!(test_decode_string(""), Ok("".to_string()));
}

#[test]
fn test_decode_string_simple() {
  assert_eq!(test_decode_string("hello"), Ok("hello".to_string()));
}

#[test]
fn test_decode_string_all_escapes() {
  assert_eq!(test_decode_string("\\n"), Ok("\n".to_string()));
  assert_eq!(test_decode_string("\\r"), Ok("\r".to_string()));
  assert_eq!(test_decode_string("\\t"), Ok("\t".to_string()));
  assert_eq!(test_decode_string("\\0"), Ok("\0".to_string()));
  assert_eq!(test_decode_string("\\\\"), Ok("\\".to_string()));
  assert_eq!(test_decode_string("\\\""), Ok("\"".to_string()));
  assert_eq!(test_decode_string("\\'"), Ok("'".to_string()));
}

#[test]
fn test_decode_string_hex_escape() {
  assert_eq!(test_decode_string("\\x41"), Ok("A".to_string()));
  assert_eq!(test_decode_string("\\x00"), Ok("\0".to_string()));
  assert_eq!(test_decode_string("\\xFF"), Ok("\u{FF}".to_string()));
  assert_eq!(test_decode_string("\\xAB"), Ok("\u{AB}".to_string()));
  assert_eq!(test_decode_string("\\xab"), Ok("\u{AB}".to_string()));
}

#[test]
fn test_decode_string_unicode_escape() {
  assert_eq!(test_decode_string("\\u{41}"), Ok("A".to_string()));
  assert_eq!(test_decode_string("\\u{0}"), Ok("\0".to_string()));
  assert_eq!(test_decode_string("\\u{10FFFF}"), Ok("\u{10FFFF}".to_string()));
  assert_eq!(test_decode_string("\\u{1F600}"), Ok("😀".to_string()));
  assert_eq!(test_decode_string("\\u{ABCD}"), Ok("\u{ABCD}".to_string()));
  assert_eq!(test_decode_string("\\u{abcd}"), Ok("\u{ABCD}".to_string()));
}

#[test]
fn test_decode_string_hex_escape_invalid() {
  assert!(test_decode_string_err("\\x4"));
  assert!(test_decode_string_err("\\x"));
  assert!(test_decode_string_err("\\xG1"));
  assert!(test_decode_string_err("\\x1G"));
  assert!(test_decode_string_err("\\xGG"));
}

#[test]
fn test_decode_string_unicode_escape_invalid() {
  assert!(test_decode_string_err("\\u41"));
  assert!(test_decode_string_err("\\u{}"));
  assert!(test_decode_string_err("\\u{41"));
  assert!(test_decode_string_err("\\u{1234567}"));
  assert!(test_decode_string_err("\\u{110000}"));
  assert!(test_decode_string_err("\\u{D800}"));
  assert!(test_decode_string_err("\\u{DFFF}"));
  assert!(test_decode_string_err("\\u{DC00}"));
  assert!(test_decode_string_err("\\u{41G}"));
}

#[test]
fn test_decode_string_invalid_escape() {
  assert!(test_decode_string_err("\\"));
  assert!(test_decode_string_err("\\z"));
  assert!(test_decode_string_err("\\a"));
  assert!(test_decode_string_err("\\b"));
}

#[test]
fn test_decode_string_unicode_boundaries() {
  assert_eq!(test_decode_string("\\u{D7FF}"), Ok("\u{D7FF}".to_string()));
  assert!(test_decode_string_err("\\u{D800}"));
  assert!(test_decode_string_err("\\u{DFFF}"));
  assert_eq!(test_decode_string("\\u{E000}"), Ok("\u{E000}".to_string()));
}

#[test]
fn test_decode_string_all_hex_values() {
  for i in 0..=255u8 {
    let hex = format!("\\x{:02X}", i);
    let result = test_decode_string(&hex);
    assert!(result.is_ok(), "Failed for \\x{:02X}", i);
    assert_eq!(result.unwrap().chars().next().unwrap() as u32, i as u32);
  }
}

// decode_byte_string tests
#[test]
fn test_decode_byte_string_empty() {
  assert_eq!(test_decode_byte_string(""), Ok(vec![]));
}

#[test]
fn test_decode_byte_string_simple() {
  assert_eq!(test_decode_byte_string("hello"), Ok(b"hello".to_vec()));
}

#[test]
fn test_decode_byte_string_with_prefix() {
  assert_eq!(test_decode_byte_string("b\"test\""), Ok(b"test".to_vec()));
}

#[test]
fn test_decode_byte_string_all_escapes() {
  assert_eq!(test_decode_byte_string("b\"\\n\""), Ok(vec![b'\n']));
  assert_eq!(test_decode_byte_string("b\"\\r\""), Ok(vec![b'\r']));
  assert_eq!(test_decode_byte_string("b\"\\t\""), Ok(vec![b'\t']));
  assert_eq!(test_decode_byte_string("b\"\\0\""), Ok(vec![0]));
  assert_eq!(test_decode_byte_string("b\"\\\\\""), Ok(vec![b'\\']));
  assert_eq!(test_decode_byte_string("b\"\\\"\""), Ok(vec![b'"']));
  assert_eq!(test_decode_byte_string("b\"\\'\""), Ok(vec![b'\'']));
}

#[test]
fn test_decode_byte_string_hex_escape() {
  assert_eq!(test_decode_byte_string("b\"\\x41\""), Ok(vec![0x41]));
  assert_eq!(test_decode_byte_string("b\"\\x00\""), Ok(vec![0]));
  assert_eq!(test_decode_byte_string("b\"\\xFF\""), Ok(vec![0xFF]));
}

#[test]
fn test_decode_byte_string_all_hex_values() {
  for i in 0..=255u8 {
    let hex = format!("b\"\\x{:02X}\"", i);
    assert_eq!(test_decode_byte_string(&hex), Ok(vec![i]));
  }
}

#[test]
fn test_decode_byte_string_unicode_disallowed() {
  assert!(test_decode_byte_string_err("\\u{41}"));
  assert!(test_decode_byte_string_err("\\u"));
}

#[test]
fn test_decode_byte_string_non_byte_value() {
  assert!(test_decode_byte_string_err("\\u{100}"));
}

#[test]
fn test_decode_byte_string_empty_after_removal() {
  assert_eq!(test_decode_byte_string("b\"\""), Ok(vec![]));
}

// decode_char tests
#[test]
fn test_decode_char_simple() {
  assert_eq!(test_decode_char("a"), Ok('a'));
  assert_eq!(test_decode_char("'a'"), Ok('a'));
  assert_eq!(test_decode_char("b'a'"), Ok('a'));
}

#[test]
fn test_decode_char_all_escapes() {
  assert_eq!(test_decode_char("'\\n'"), Ok('\n'));
  assert_eq!(test_decode_char("'\\r'"), Ok('\r'));
  assert_eq!(test_decode_char("'\\t'"), Ok('\t'));
  assert_eq!(test_decode_char("'\\0'"), Ok('\0'));
  assert_eq!(test_decode_char("'\\\\'"), Ok('\\'));
  assert_eq!(test_decode_char("'\\\"'"), Ok('"'));
  assert_eq!(test_decode_char("'\\''"), Ok('\''));
}

#[test]
fn test_decode_char_hex_escape() {
  assert_eq!(test_decode_char("'\\x41'"), Ok('A'));
  assert_eq!(test_decode_char("'\\x00'"), Ok('\0'));
  assert_eq!(test_decode_char("'\\xFF'"), Ok('\u{FF}'));
}

#[test]
fn test_decode_char_unicode_escape() {
  assert_eq!(test_decode_char("'\\u{41}'"), Ok('A'));
  assert_eq!(test_decode_char("'\\u{0}'"), Ok('\0'));
  assert_eq!(test_decode_char("'\\u{10FFFF}'"), Ok('\u{10FFFF}'));
  assert_eq!(test_decode_char("'\\u{1F600}'"), Ok('😀'));
}

#[test]
fn test_decode_char_empty() {
  assert!(test_decode_char_err(""));
  assert!(test_decode_char_err("''"));
}

#[test]
fn test_decode_char_multiple() {
  assert!(test_decode_char_err("ab"));
  assert!(test_decode_char_err("'ab'"));
  assert!(test_decode_char_err("abc"));
  assert!(test_decode_char_err("abcd"));
}

#[test]
fn test_decode_char_unicode_boundaries() {
  assert_eq!(test_decode_char("'\\u{D7FF}'"), Ok('\u{D7FF}'));
  assert!(test_decode_char_err("'\\u{D800}'"));
  assert!(test_decode_char_err("'\\u{DFFF}'"));
  assert_eq!(test_decode_char("'\\u{E000}'"), Ok('\u{E000}'));
}

// decode_byte tests
#[test]
fn test_decode_byte_simple() {
  assert_eq!(test_decode_byte("a"), Ok(b'a'));
  assert_eq!(test_decode_byte("'a'"), Ok(b'a'));
  assert_eq!(test_decode_byte("b'a'"), Ok(b'a'));
}

#[test]
fn test_decode_byte_all_escapes() {
  assert_eq!(test_decode_byte("b'\\n'"), Ok(b'\n'));
  assert_eq!(test_decode_byte("b'\\t'"), Ok(b'\t'));
  assert_eq!(test_decode_byte("b'\\0'"), Ok(0));
  assert_eq!(test_decode_byte("b'\\\\'"), Ok(b'\\'));
  assert_eq!(test_decode_byte("b'\\''"), Ok(b'\''));
}

#[test]
fn test_decode_byte_hex_escape() {
  assert_eq!(test_decode_byte("b'\\x41'"), Ok(0x41));
  assert_eq!(test_decode_byte("b'\\x00'"), Ok(0));
  assert_eq!(test_decode_byte("b'\\xFF'"), Ok(0xFF));
}

#[test]
fn test_decode_byte_all_values() {
  for i in 0..=255u8 {
    let hex = format!("b'\\x{:02X}'", i);
    assert_eq!(test_decode_byte(&hex), Ok(i));
  }
}

#[test]
fn test_decode_byte_unicode_valid() {
  assert_eq!(test_decode_byte("b'\\u{41}'"), Ok(0x41));
  assert_eq!(test_decode_byte("b'\\u{0}'"), Ok(0));
  assert_eq!(test_decode_byte("b'\\u{FF}'"), Ok(0xFF));
}

#[test]
fn test_decode_byte_non_byte_value() {
  assert!(test_decode_byte_err("b'\\u{100}'"));
  assert!(test_decode_byte_err("b'\\u{10FFFF}'"));
}

#[test]
fn test_decode_byte_empty() {
  assert!(test_decode_byte_err(""));
}

#[test]
fn test_decode_byte_multiple() {
  assert!(test_decode_byte_err("ab"));
}

// Edge cases
#[test]
fn test_decode_string_unicode_digit_counts() {
  assert_eq!(test_decode_string("\\u{0}"), Ok("\0".to_string()));
  assert_eq!(test_decode_string("\\u{1}"), Ok("\u{1}".to_string()));
  assert_eq!(test_decode_string("\\u{F}"), Ok("\u{F}".to_string()));
  assert_eq!(test_decode_string("\\u{41}"), Ok("A".to_string()));
  assert_eq!(test_decode_string("\\u{100}"), Ok("\u{100}".to_string()));
  assert_eq!(test_decode_string("\\u{1000}"), Ok("\u{1000}".to_string()));
  assert_eq!(test_decode_string("\\u{10000}"), Ok("\u{10000}".to_string()));
  assert_eq!(test_decode_string("\\u{10FFFF}"), Ok("\u{10FFFF}".to_string()));
  assert!(test_decode_string_err("\\u{1000000}"));
}

#[test]
fn test_decode_string_mixed_escapes() {
  assert_eq!(
    test_decode_string("a\\x42c\\u{44}e"),
    Ok("aBcDe".to_string())
  );
  assert_eq!(
    test_decode_string("\\u{48}\\u{65}\\u{6C}\\u{6C}\\u{6F}"),
    Ok("Hello".to_string())
  );
}

#[test]
fn test_decode_string_unicode_surrogate_range() {
  for i in 0xD800..=0xDFFF {
    let hex = format!("\\u{{{:X}}}", i);
    assert!(test_decode_string_err(&hex), "Should reject surrogate {}", i);
  }
}
