use crate::source_map::{with_forced_readdir_entry_error, SourceMap};
use std::{
  env, fs,
  path::PathBuf,
  time::{SystemTime, UNIX_EPOCH},
};

fn temp_root(prefix: &str) -> PathBuf {
  let unique = SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .unwrap()
    .as_nanos();
  env::temp_dir().join(format!("{prefix}_{unique}"))
}

#[test]
fn get_files_propagates_entry_error() {
  let root = temp_root("diagnostic_entry_error");
  fs::create_dir_all(&root).unwrap();
  fs::write(root.join("file.duck"), "hi").unwrap();

  let mut map = SourceMap::new();
  let result = with_forced_readdir_entry_error(|| map.get_files(root.to_str().unwrap()));
  assert!(result.is_err());

  fs::remove_dir_all(&root).unwrap();
}

#[cfg(unix)]
#[test]
fn get_files_propagates_nested_read_dir_error() {
  use std::os::unix::fs::PermissionsExt;

  let root = temp_root("diagnostic_nested_error");
  let blocked = root.join("blocked");
  fs::create_dir_all(&blocked).unwrap();
  fs::set_permissions(&blocked, fs::Permissions::from_mode(0o000)).unwrap();

  let mut map = SourceMap::new();
  let result = map.get_files(root.to_str().unwrap());
  assert!(result.is_err());

  fs::set_permissions(&blocked, fs::Permissions::from_mode(0o700)).unwrap();
  fs::remove_dir_all(&root).unwrap();
}

#[cfg(unix)]
#[test]
fn get_files_handles_non_file_or_dir_entries() {
  use std::os::unix::fs::symlink;

  let root = temp_root("diagnostic_non_file_entry");
  fs::create_dir_all(&root).unwrap();
  let link = root.join("dangling");
  symlink(root.join("missing"), &link).unwrap();

  let mut map = SourceMap::new();
  let result = map.get_files(root.to_str().unwrap());
  assert!(result.is_ok());

  fs::remove_dir_all(&root).unwrap();
}
