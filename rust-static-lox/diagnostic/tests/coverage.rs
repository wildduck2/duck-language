use diagnostic::{
  code::DiagnosticCode,
  diagnostic::{Diagnostic, Label, LabelStyle},
  types::{error::DiagnosticError, warning::DiagnosticWarning, Severity},
  DiagnosticEngine, SourceFile, SourceMap, Span,
};
use std::{env, fs, process::Command, time::SystemTime};

#[test]
fn diagnostic_error_codes_cover_all_variants() {
  use DiagnosticError::*;

  let cases = [
    (CodeNotFound, "E0001"),
    (InvalidArguments, "E0002"),
    (InvalidShebang, "E0100"),
    (InvalidCharacter, "E0101"),
    (UnterminatedString, "E0102"),
    (TooManyRawStrHashes, "E0103"),
    (InvalidStringStart, "E0104"),
    (InvalidEscape, "E0105"),
    (UnknownPrefix, "E0106"),
    (ReservedPrefix, "E0107"),
    (InvalidLifetime, "E0108"),
    (InvalidInteger, "E0109"),
    (InvalidIdentifier, "E0110"),
    (UnexpectedToken, "E0200"),
    (MissingClosingBracket, "E0203"),
    (InvalidLiteral, "E0201"),
    (EmptyChar, "E0202"),
    (InvalidVisibilityRestriction, "E0204"),
    (InvalidNameIdentifier, "E0205"),
    (InvalidType, "E0207"),
    (InvalidMutabilityInField, "E0208"),
    (InvalidPointerType, "E0209"),
    (InvalidMutability, "E0211"),
    (UnexpectedLifetime, "E0206"),
    (InvalidWherePredicate, "E0210"),
    (InvalidWhereClause, "E0232"),
    (InvalidTraitBound, "E0233"),
    (InvalidTraitBoundModifier, "E0226"),
    (InvalidComma, "E0231"),
    (EmptyGenericArgs, "E0228"),
    (InvalidGenericArg, "E0229"),
    (InvalidPathSegment, "E0230"),
    (InvalidTrailingComma, "E0229"),
    (InvalidAbi, "E0224"),
    (InvalidVariadic, "E0225"),
    (InvalidSelfParam, "E0227"),
    (InvalidSelfInFreeFunction, "E0223"),
    (ReturnOutsideFunction, "E0217"),
    (ContinueOutsideLoop, "E0218"),
    (BreakOutsideLoop, "E0219"),
    (InvalidCondition, "E0222"),
    (ExpectedBlockAfterFlavor, "E0216"),
    (InvalidBlockFlavorContext, "E0220"),
    (InvalidFlavorOrder, "E0221"),
    (UndefinedVariable, "E0212"),
    (MismatchedTypes, "E0213"),
    (TraitNotSatisfied, "E0214"),
    (BorrowCheckerViolation, "E0215"),
  ];

  for (error, code) in cases {
    assert_eq!(error.code(), code);
    assert_eq!(error.severity(), Severity::Error);
  }
}

#[test]
fn diagnostic_warning_codes_cover_all_variants() {
  let cases = [
    (DiagnosticWarning::UnusedVariable, "W0001"),
    (DiagnosticWarning::InvalidConstDeclaration, "W0002"),
  ];

  for (warning, code) in cases {
    assert_eq!(warning.code(), code);
    assert_eq!(warning.severity(), Severity::Warning);
  }
}

#[test]
fn diagnostic_code_dispatches() {
  let err = DiagnosticCode::Error(DiagnosticError::InvalidIdentifier);
  let warn = DiagnosticCode::Warning(DiagnosticWarning::UnusedVariable);

  assert_eq!(err.code(), "E0110");
  assert_eq!(warn.code(), "W0001");
  assert_eq!(err.severity(), Severity::Error);
  assert_eq!(warn.severity(), Severity::Warning);
}

#[test]
fn diagnostic_debug_and_clone_coverage() {
  let label = Label {
    span: Span::new(0, 1),
    message: Some("msg".to_string()),
    style: LabelStyle::Primary,
  };
  let label_clone = label.clone();
  let _ = format!("{:?}", label);
  let _ = format!("{:?}", label_clone);

  let style = LabelStyle::Secondary;
  let _ = format!("{:?}", style);
  let _ = style.clone();

  let diag = Diagnostic::new(
    DiagnosticCode::Error(DiagnosticError::CodeNotFound),
    "debug".to_string(),
    "file.rs".to_string(),
  )
  .with_label(Span::new(0, 1), Some("label".to_string()), LabelStyle::Primary);
  let _ = format!("{:?}", diag);
}

#[test]
fn span_and_source_file_helpers() {
  let src = "ab\ncde\n";
  let file = SourceFile::new("test.rs".to_string(), src.to_string());

  assert_eq!(file.line_count(), 3);
  assert_eq!(file.line_content(1), Some("ab"));
  assert_eq!(file.line_content(2), Some("cde"));
  assert_eq!(file.line_content(3), Some(""));
  assert_eq!(file.line_content(0), None);
  assert_eq!(file.line_content(4), None);

  assert_eq!(file.line_col(0), (1, 1));
  assert_eq!(file.line_col(4), (2, 2));

  let span = Span::from_line_col(2, 2, 2, &file);
  assert_eq!(span, Span::new(4, 6));

  let span_far = Span::from_line_col(10, 3, 1, &file);
  assert_eq!(span_far, Span::new(2, 3));

  let mut merged = Span::new(5, 8);
  merged.merge(Span::new(2, 10));
  assert_eq!(merged, Span::new(2, 10));

  let backwards = Span::new(5, 3);
  assert_eq!(backwards.len(), 0);
  assert!(backwards.is_empty());
  assert!(!backwards.contains(4));

  let span = Span::new(3, 5);
  assert_eq!(span.len(), 2);
  assert!(!span.is_empty());
  assert!(span.contains(4));

  let default_span = Span::default();
  assert_eq!(default_span, Span::new(0, 0));

  let snippet = file.snippet(Span::new(1, 100));
  assert_eq!(snippet, "b\ncde\n");
}

#[test]
fn source_map_add_and_lookup() {
  let mut map = SourceMap::new();
  map.add_file("file.rs", "line1\n");
  assert!(map.has_file("file.rs"));
  assert_eq!(map.get("file.rs").unwrap().path, "file.rs");

  let unique = SystemTime::now()
    .duration_since(std::time::UNIX_EPOCH)
    .unwrap()
    .as_nanos();
  let root = env::temp_dir().join(format!("diagnostic_test_{}", unique));
  let nested = root.join("nested");
  fs::create_dir_all(&nested).unwrap();
  let file_a = root.join("a.txt");
  let file_b = nested.join("b.txt");
  fs::write(&file_a, "a").unwrap();
  fs::write(&file_b, "b").unwrap();

  let mut map = SourceMap::new();
  map.add_wd(root.to_str().unwrap()).unwrap();
  assert!(map.has_file(file_a.to_str().unwrap()));
  assert!(map.has_file(file_b.to_str().unwrap()));

  fs::remove_dir_all(&root).unwrap();
}

#[test]
fn add_wd_missing_path_exits() {
  if env::var("DIAGNOSTIC_ADD_WD_EXIT").is_ok() {
    let mut map = SourceMap::new();
    let _ = map.add_wd("this/path/does/not/exist");
    return;
  }

  // Spawn the test binary so we can observe the exit code without killing this test.
  let exe = env::current_exe().unwrap();
  let status = Command::new(exe)
    .env("DIAGNOSTIC_ADD_WD_EXIT", "1")
    .arg("--exact")
    .arg("add_wd_missing_path_exits")
    .status()
    .unwrap();

  assert_eq!(status.code(), Some(64));
}

#[cfg(coverage)]
#[test]
fn add_wd_forced_entry_error_exits() {
  if env::var("DIAGNOSTIC_FORCE_ENTRY_ERROR_CHILD").is_ok() {
    let root = env::var("DIAGNOSTIC_FORCE_ENTRY_ERROR_ROOT").unwrap();
    let mut map = SourceMap::new();
    let _ = diagnostic::source_map::with_forced_readdir_entry_error(|| map.add_wd(&root));
    return;
  }

  let unique = SystemTime::now()
    .duration_since(std::time::UNIX_EPOCH)
    .unwrap()
    .as_nanos();
  let root = env::temp_dir().join(format!("diagnostic_force_entry_error_{unique}"));
  fs::create_dir_all(&root).unwrap();
  fs::write(root.join("file.txt"), "data").unwrap();

  let exe = env::current_exe().unwrap();
  let status = Command::new(exe)
    .env("DIAGNOSTIC_FORCE_ENTRY_ERROR_CHILD", "1")
    .env("DIAGNOSTIC_FORCE_ENTRY_ERROR_ROOT", root.to_str().unwrap())
    .arg("--exact")
    .arg("add_wd_forced_entry_error_exits")
    .status()
    .unwrap();

  assert_eq!(status.code(), Some(64));
  fs::remove_dir_all(&root).unwrap();
}

#[cfg(all(coverage, unix))]
#[test]
fn add_wd_nested_read_dir_error_exits() {
  if env::var("DIAGNOSTIC_NESTED_READDIR_CHILD").is_ok() {
    let root = env::var("DIAGNOSTIC_NESTED_READDIR_ROOT").unwrap();
    let mut map = SourceMap::new();
    let _ = map.add_wd(&root);
    return;
  }

  use std::os::unix::fs::PermissionsExt;

  let unique = SystemTime::now()
    .duration_since(std::time::UNIX_EPOCH)
    .unwrap()
    .as_nanos();
  let root = env::temp_dir().join(format!("diagnostic_nested_read_dir_error_{unique}"));
  let blocked = root.join("blocked");
  fs::create_dir_all(&blocked).unwrap();
  fs::set_permissions(&blocked, fs::Permissions::from_mode(0o000)).unwrap();

  let exe = env::current_exe().unwrap();
  let status = Command::new(exe)
    .env("DIAGNOSTIC_NESTED_READDIR_CHILD", "1")
    .env("DIAGNOSTIC_NESTED_READDIR_ROOT", root.to_str().unwrap())
    .arg("--exact")
    .arg("add_wd_nested_read_dir_error_exits")
    .status()
    .unwrap();

  assert_eq!(status.code(), Some(64));

  fs::set_permissions(&blocked, fs::Permissions::from_mode(0o700)).unwrap();
  fs::remove_dir_all(&root).unwrap();
}

#[cfg(all(coverage, unix))]
#[test]
fn add_wd_handles_non_file_or_dir_entries() {
  use std::os::unix::fs::symlink;

  let unique = SystemTime::now()
    .duration_since(std::time::UNIX_EPOCH)
    .unwrap()
    .as_nanos();
  let root = env::temp_dir().join(format!("diagnostic_non_file_entry_{unique}"));
  fs::create_dir_all(&root).unwrap();
  let link = root.join("dangling");
  symlink(root.join("missing"), &link).unwrap();

  let mut map = SourceMap::new();
  map.add_wd(root.to_str().unwrap()).unwrap();

  fs::remove_dir_all(&root).unwrap();
}

#[test]
fn diagnostic_engine_counts_and_prints() {
  let mut engine = DiagnosticEngine::new();
  engine.insert_source("source".to_string());
  engine.add_file("file.rs", "line\n");

  let error = Diagnostic::error(
    DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
    "err".to_string(),
    "file.rs".to_string(),
  );
  let warning = Diagnostic::warning(
    DiagnosticCode::Warning(DiagnosticWarning::UnusedVariable),
    "warn".to_string(),
    "file.rs".to_string(),
  );

  engine.add(error);
  engine.add(warning);

  assert_eq!(engine.error_count(), 1);
  assert_eq!(engine.warning_count(), 1);
  assert!(engine.has_errors());
  assert!(engine.has_warnings());

  engine.print_diagnostics();
}

#[test]
fn diagnostic_format_renders_context_and_markers() {
  let mut map = SourceMap::new();
  let path = "test.rs";
  let src = "alpha\nbeta gamma\ndelta\n";
  map.add_file(path, src);
  let file = map.get(path).unwrap();

  let label_primary_one = Span::from_line_col(2, 1, 1, file);
  let label_primary_two = Span::from_line_col(2, 3, 2, file);
  let label_secondary_one = Span::from_line_col(2, 6, 1, file);
  let label_secondary_long = Span::from_line_col(3, 1, 3, file);

  let diag = Diagnostic::new(
    DiagnosticCode::Error(DiagnosticError::UnexpectedToken),
    "oops".to_string(),
    path.to_string(),
  )
  .with_context_padding(0)
  .with_label(
    label_primary_one,
    Some("primary".to_string()),
    LabelStyle::Primary,
  )
  .with_label(label_primary_two, None, LabelStyle::Primary)
  .with_label(
    label_secondary_one,
    None,
    LabelStyle::Secondary,
  )
  .with_label(
    label_secondary_long,
    Some("secondary".to_string()),
    LabelStyle::Secondary,
  )
  .with_help("help message".to_string())
  .with_note("note message".to_string());

  let output = diag.format(&map);
  assert!(output.contains("oops"));
  assert!(output.contains("test.rs:2:1"));
  assert!(output.contains("beta gamma"));
  assert!(output.contains("delta"));
  assert!(output.contains("help message"));
  assert!(output.contains("note message"));
}

#[test]
fn diagnostic_format_handles_missing_file() {
  let map = SourceMap::new();
  let diag = Diagnostic::new(
    DiagnosticCode::Error(DiagnosticError::InvalidType),
    "msg".to_string(),
    "missing.rs".to_string(),
  )
  .with_label(Span::new(0, 1), None, LabelStyle::Primary);

  let output = diag.format(&map);
  assert!(output.contains("msg"));
}

#[test]
fn diagnostic_format_without_labels() {
  let map = SourceMap::new();
  let diag = Diagnostic::new(
    DiagnosticCode::Error(DiagnosticError::CodeNotFound),
    "nolabel".to_string(),
    "missing.rs".to_string(),
  );

  let output = diag.format(&map);
  assert!(output.contains("nolabel"));
}

#[test]
fn diagnostic_format_severity_variants() {
  let mut map = SourceMap::new();
  map.add_file("file.rs", "line\n");
  let file = map.get("file.rs").unwrap();
  let label = Span::from_line_col(1, 1, 1, file);

  let error = Diagnostic::error(
    DiagnosticCode::Error(DiagnosticError::InvalidIdentifier),
    "err".to_string(),
    "file.rs".to_string(),
  )
  .with_label(label, Some("err".to_string()), LabelStyle::Primary);
  assert_eq!(error.severity, Severity::Error);
  assert!(error.format(&map).contains("error"));

  let warning = Diagnostic::warning(
    DiagnosticCode::Warning(DiagnosticWarning::UnusedVariable),
    "warn".to_string(),
    "file.rs".to_string(),
  )
  .with_label(label, Some("warn".to_string()), LabelStyle::Primary);
  assert_eq!(warning.severity, Severity::Warning);
  assert!(warning.format(&map).contains("warning"));

  let note = Diagnostic::note(
    DiagnosticCode::Error(DiagnosticError::InvalidLiteral),
    "note".to_string(),
    "file.rs".to_string(),
  )
  .with_label(label, Some("note".to_string()), LabelStyle::Primary);
  assert_eq!(note.severity, Severity::Note);
  assert!(note.format(&map).contains("note"));

  let help = Diagnostic::info(
    DiagnosticCode::Warning(DiagnosticWarning::InvalidConstDeclaration),
    "help".to_string(),
    "file.rs".to_string(),
  )
  .with_label(label, Some("help".to_string()), LabelStyle::Primary);
  assert_eq!(help.severity, Severity::Help);
  assert!(help.format(&map).contains("help"));

  error.print(&map);
  error.eprint(&map);
}
