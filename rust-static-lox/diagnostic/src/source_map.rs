use std::{collections::BTreeMap, fs};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
  pub start: usize, // byte offset in source
  pub end: usize,   // byte offset in source
}

impl Span {
  pub fn new(start: usize, end: usize) -> Self {
    Self { start, end }
  }

  pub fn merge(&mut self, other: Self) -> &mut Self {
    self.start = self.start.min(other.start);
    self.end = self.end.max(other.end);
    self
  }

  pub fn len(&self) -> usize {
    self.end.saturating_sub(self.start)
  }

  pub fn is_empty(&self) -> bool {
    self.start >= self.end
  }

  pub fn contains(&self, pos: usize) -> bool {
    pos >= self.start && pos < self.end
  }

  /// Create span from line/col (requires SourceFile for conversion)
  /// line and col are 1-indexed
  pub fn from_line_col(line: usize, col: usize, len: usize, source_file: &SourceFile) -> Self {
    let line_start = source_file
      .line_offsets
      .get(line.saturating_sub(1))
      .copied()
      .unwrap_or(0);
    let start = line_start + col.saturating_sub(1);
    let end = start + len;
    Self { start, end }
  }
}

impl Default for Span {
  fn default() -> Self {
    Self { start: 0, end: 0 }
  }
}

#[derive(Debug, Clone)]
pub struct SourceFile {
  pub path: String,
  pub src: String,
  pub line_offsets: Vec<usize>, // byte indices where each line starts
}

impl SourceFile {
  pub fn new(path: String, src: String) -> Self {
    let mut line_offsets = vec![0];
    for (i, byte) in src.bytes().enumerate() {
      if byte == b'\n' {
        line_offsets.push(i + 1);
      }
    }
    Self {
      path,
      src,
      line_offsets,
    }
  }

  /// Convert byte offset to (line, col) - both 1-indexed
  pub fn line_col(&self, byte_offset: usize) -> (usize, usize) {
    let line = match self.line_offsets.binary_search(&byte_offset) {
      Ok(i) => i,
      Err(i) => i.saturating_sub(1),
    };
    let col = byte_offset.saturating_sub(self.line_offsets.get(line).copied().unwrap_or(0));
    (line + 1, col + 1)
  }

  /// Get the line content for a given line number (1-indexed)
  pub fn line_content(&self, line_num: usize) -> Option<&str> {
    if line_num == 0 || line_num > self.line_offsets.len() {
      return None;
    }

    let start = self.line_offsets[line_num - 1];
    let end = if line_num < self.line_offsets.len() {
      self.line_offsets[line_num].saturating_sub(1) // Exclude newline
    } else {
      self.src.len()
    };

    Some(&self.src[start..end])
  }

  /// Extract snippet text for a given Span
  pub fn snippet(&self, span: Span) -> &str {
    let end = span.end.min(self.src.len());
    &self.src[span.start..end]
  }

  /// Get total number of lines
  pub fn line_count(&self) -> usize {
    self.line_offsets.len()
  }
}

#[derive(Debug, Default)]
pub struct SourceMap {
  pub files: BTreeMap<String, SourceFile>,
}

impl SourceMap {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn add_wd(&mut self, path: &str) -> Result<(), std::io::Error> {
    match self.get_files(path) {
      Err(e) => {
        println!("{}", e.to_string());
        std::process::exit(64);
      },
      Ok(_) => {},
    }

    Ok(())
  }

  fn get_files(&mut self, path: &str) -> Result<(), std::io::Error> {
    for entry in fs::read_dir(path)? {
      let entry = entry?;
      let path = entry.path();

      if path.is_dir() {
        self.get_files(path.to_str().unwrap())?;
      } else if path.is_file() {
        self.add_file(
          path.clone().to_str().unwrap(),
          &fs::read_to_string(path).unwrap(),
        );
      }
    }

    Ok(())
  }

  pub fn add_file(&mut self, path: &str, src: &str) {
    self.files.insert(
      path.to_string(),
      SourceFile::new(path.to_string(), src.to_string()),
    );
  }

  pub fn get(&self, path: &str) -> Option<&SourceFile> {
    self.files.get(path)
  }

  pub fn has_file(&self, path: &str) -> bool {
    self.files.contains_key(path)
  }
}
