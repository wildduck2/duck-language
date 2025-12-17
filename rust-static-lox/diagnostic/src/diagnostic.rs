use colored::*;
use std::io;

use crate::code::DiagnosticCode;
use crate::source_map::{SourceMap, Span};
use crate::types::Severity;

#[derive(Debug, Clone)]
pub struct Label {
  pub span: Span,
  pub message: Option<String>,
  pub style: LabelStyle,
}

#[derive(Debug, Clone, Copy)]
pub enum LabelStyle {
  Primary,   // ^~~~ in red (wave underline)
  Secondary, // ---- in cyan
}

// Use Severity from crate::types

#[derive(Debug)]
pub struct Diagnostic {
  pub code: DiagnosticCode,
  pub message: String,
  pub file_path: String,
  pub labels: Vec<Label>,
  pub help: Option<String>,
  pub note: Option<String>,
  pub severity: Severity,
  context_padding: usize, // Number of lines to show above/below the error
}

impl Diagnostic {
  pub fn new(code: DiagnosticCode, message: String, file_path: String) -> Self {
    Self {
      code,
      message,
      file_path,
      labels: Vec::new(),
      help: None,
      note: None,
      severity: code.severity(),
      context_padding: 2,
    }
  }

  // Convenience constructors
  pub fn error(code: DiagnosticCode, message: String, file_path: String) -> Self {
    Self::new(code, message, file_path)
  }

  pub fn warning(code: DiagnosticCode, message: String, file_path: String) -> Self {
    let mut diag = Self::new(code, message, file_path);
    diag.severity = Severity::Warning;
    diag
  }

  pub fn note(code: DiagnosticCode, message: String, file_path: String) -> Self {
    let mut diag = Self::new(code, message, file_path);
    diag.severity = Severity::Note;
    diag
  }

  pub fn info(code: DiagnosticCode, message: String, file_path: String) -> Self {
    let mut diag = Self::new(code, message, file_path);
    diag.severity = Severity::Help;
    diag
  }

  // Builder methods
  pub fn with_label(mut self, span: Span, message: Option<String>, style: LabelStyle) -> Self {
    self.labels.push(Label {
      span,
      message,
      style,
    });
    self
  }

  pub fn with_help(mut self, help: String) -> Self {
    self.help = Some(help);
    self
  }

  pub fn with_note(mut self, note: String) -> Self {
    self.note = Some(note);
    self
  }

  pub fn with_context_padding(mut self, padding: usize) -> Self {
    self.context_padding = padding;
    self
  }

  /// Loads context lines using SourceMap
  fn load_context(&self, source_map: &SourceMap) -> Result<Vec<(usize, String)>, io::Error> {
    if self.labels.is_empty() {
      return Ok(Vec::new());
    }

    let source_file = match source_map.get(&self.file_path) {
      Some(file) => file,
      None => return Ok(Vec::new()),
    };

    // Find the range of lines we need to display based on spans
    let mut min_line = usize::MAX;
    let mut max_line = 0;

    for label in &self.labels {
      let (line, _) = source_file.line_col(label.span.start);
      min_line = min_line.min(line);
      max_line = max_line.max(line);
    }

    if min_line == usize::MAX {
      return Ok(Vec::new());
    }

    let start_line = min_line.saturating_sub(self.context_padding).max(1);
    let end_line = (max_line + self.context_padding).min(source_file.line_count());

    let mut context_lines = Vec::new();
    for line_num in start_line..=end_line {
      if let Some(content) = source_file.line_content(line_num) {
        context_lines.push((line_num, content.to_string()));
      }
    }

    Ok(context_lines)
  }

  pub fn format(&self, source_map: &SourceMap) -> Result<String, io::Error> {
    let mut output = String::new();

    // Dynamic coloring based on severity
    let severity_text = match self.severity {
      Severity::Error => "error".bright_red().bold(),
      Severity::Warning => "warning".bright_yellow().bold(),
      Severity::Note => "note".bright_cyan().bold(),
      Severity::Help => "help".bright_blue().bold(),
    };

    // Error header with icon
    let icon = match self.severity {
      Severity::Error => "✗",
      Severity::Warning => "⚠",
      Severity::Note => "ℹ",
      Severity::Help => "●",
    };

    let code_text = format!("[{}]", self.code.code());
    let code_colored = match self.severity {
      Severity::Error => code_text.bright_red().bold(),
      Severity::Warning => code_text.bright_yellow().bold(),
      Severity::Note => code_text.bright_cyan().bold(),
      Severity::Help => code_text.bright_blue().bold(),
    };

    output.push_str(&format!(
      "{} {}{} {}\n",
      icon,
      severity_text,
      code_colored,
      self.message.white()
    ));

    if let Some(primary) = self.labels.first() {
      let source_file = source_map.get(&self.file_path);

      if let Some(file) = source_file {
        let (line, col) = file.line_col(primary.span.start);

        // File location line (rustc style)
        output.push_str(&format!(
          "  {} {}\n",
          "-->".bright_blue().bold(),
          format!("{}:{}:{}", self.file_path, line, col).bright_blue()
        ));

        // Load context dynamically
        let context_lines = self.load_context(source_map)?;

        if !context_lines.is_empty() {
          let max_line = context_lines.iter().map(|(ln, _)| *ln).max().unwrap_or(1);
          let line_width = max_line.to_string().len();

          // Top separator
          output.push_str(&format!(
            " {:width$} {}\n",
            "",
            "|".bright_blue().bold(),
            width = line_width
          ));

          // Group labels by line for proper rendering
          let mut lines_with_labels: std::collections::HashMap<usize, Vec<&Label>> =
            std::collections::HashMap::new();

          for label in &self.labels {
            let (line, _) = file.line_col(label.span.start);
            lines_with_labels
              .entry(line)
              .or_insert_with(Vec::new)
              .push(label);
          }

          // Render all context lines in order
          for (line_num, content) in &context_lines {
            let line_content = content.white();

            output.push_str(&format!(
              " {:width$} {} {}\n",
              line_num.to_string().blue().bold(),
              "|".blue().bold(),
              line_content,
              width = line_width
            ));

            // Print all labels for this line
            if let Some(labels) = lines_with_labels.get(line_num) {
              for label in labels {
                // Convert byte offset to column
                let (_, col) = file.line_col(label.span.start);
                let spaces = " ".repeat(col.saturating_sub(1));

                let marker_len = label.span.len().max(1);

                // Create the marker with wave effect (Rust style)
                let markers = if marker_len == 1 {
                  match label.style {
                    LabelStyle::Primary => "^".to_string(),
                    LabelStyle::Secondary => "-".to_string(),
                  }
                } else {
                  match label.style {
                    LabelStyle::Primary => {
                      if marker_len == 2 {
                        "^^".to_string()
                      } else {
                        // Wave effect: ^~~~
                        format!("^{}", "~".repeat(marker_len - 1))
                      }
                    },
                    LabelStyle::Secondary => "-".repeat(marker_len),
                  }
                };

                let message_color = |msg: &str| match self.severity {
                  Severity::Error => msg.bright_red(),
                  Severity::Warning => msg.bright_yellow(),
                  Severity::Note => msg.bright_cyan(),
                  Severity::Help => msg.bright_blue(),
                };

                // Color the markers
                let colored_markers = match label.style {
                  LabelStyle::Primary => message_color(&markers).bold(),
                  LabelStyle::Secondary => markers.bright_blue().bold(),
                };

                // Print marker line with optional message
                if let Some(msg) = &label.message {
                  let colored_msg = match label.style {
                    LabelStyle::Primary => message_color(msg).bold(),
                    LabelStyle::Secondary => msg.bright_blue().bold(),
                  };

                  output.push_str(&format!(
                    " {:width$} {} {}{} {}\n",
                    "",
                    "|".blue().bold(),
                    spaces,
                    colored_markers,
                    colored_msg,
                    width = line_width
                  ));
                } else {
                  output.push_str(&format!(
                    " {:width$} {} {}{}\n",
                    "",
                    "|".blue().bold(),
                    spaces,
                    colored_markers,
                    width = line_width
                  ));
                }
              }
            }
          }

          // Bottom separator before help/note
          if self.help.is_some() || self.note.is_some() {
            output.push_str(&format!(
              " {:width$} {}\n",
              "",
              "|".blue().bold(),
              width = line_width
            ));
          }
        }
      }
    }

    // Help and note - Rust style
    if let Some(help) = &self.help {
      output.push_str(&format!(
        " {} {} {}\n",
        " ",
        "= help:".bright_blue().bold(),
        help
      ));
    }

    if let Some(note) = &self.note {
      output.push_str(&format!(
        " {} {} {}\n",
        " ",
        "= note:".bright_cyan().bold(),
        note
      ));
    }

    Ok(output)
  }

  pub fn print(&self, source_map: &SourceMap) -> Result<(), io::Error> {
    print!("{}", self.format(source_map)?);
    Ok(())
  }

  pub fn eprint(&self, source_map: &SourceMap) -> Result<(), io::Error> {
    eprint!("{}", self.format(source_map)?);
    Ok(())
  }
}
