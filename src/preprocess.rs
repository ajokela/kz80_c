//! Simple C Preprocessor
//! Supports #define (simple constants and macros) and #include

use std::collections::HashMap;
use std::path::Path;

pub struct Preprocessor {
    defines: HashMap<String, String>,
    include_paths: Vec<String>,
}

impl Preprocessor {
    pub fn new() -> Self {
        Self {
            defines: HashMap::new(),
            include_paths: vec![".".to_string()],
        }
    }

    pub fn add_include_path(&mut self, path: &str) {
        self.include_paths.push(path.to_string());
    }

    pub fn define(&mut self, name: &str, value: &str) {
        self.defines.insert(name.to_string(), value.to_string());
    }

    /// Process source code, expanding #define and #include directives
    pub fn process(&mut self, source: &str, filename: &str) -> Result<String, String> {
        let mut output = String::new();
        let mut lines = source.lines().peekable();

        while let Some(line) = lines.next() {
            let trimmed = line.trim();

            if trimmed.starts_with('#') {
                self.process_directive(trimmed, filename, &mut output)?;
            } else {
                // Expand macros in regular lines
                let expanded = self.expand_macros(line);
                output.push_str(&expanded);
                output.push('\n');
            }
        }

        Ok(output)
    }

    fn process_directive(&mut self, line: &str, filename: &str, output: &mut String) -> Result<(), String> {
        let line = &line[1..].trim(); // Skip '#'

        if line.starts_with("define") {
            self.process_define(&line[6..].trim())?;
        } else if line.starts_with("include") {
            self.process_include(&line[7..].trim(), filename, output)?;
        } else if line.starts_with("ifdef") || line.starts_with("ifndef")
                || line.starts_with("else") || line.starts_with("endif") {
            // TODO: Conditional compilation - for now just skip
        } else {
            // Unknown directive - ignore for now
        }

        Ok(())
    }

    fn process_define(&mut self, rest: &str) -> Result<(), String> {
        // Parse: NAME value  or  NAME(args) value
        let mut chars = rest.chars().peekable();
        let mut name = String::new();

        // Get macro name
        while let Some(&c) = chars.peek() {
            if c.is_alphanumeric() || c == '_' {
                name.push(c);
                chars.next();
            } else {
                break;
            }
        }

        if name.is_empty() {
            return Err("Expected macro name after #define".to_string());
        }

        // Skip whitespace
        while let Some(&c) = chars.peek() {
            if c.is_whitespace() {
                chars.next();
            } else {
                break;
            }
        }

        // Rest is the value (or empty for flag-style defines)
        let value: String = chars.collect();
        self.defines.insert(name, value.trim().to_string());

        Ok(())
    }

    fn process_include(&mut self, rest: &str, current_file: &str, output: &mut String) -> Result<(), String> {
        // Parse: "filename" or <filename>
        let (filename, is_system) = if rest.starts_with('"') {
            let end = rest[1..].find('"').ok_or("Unterminated include string")?;
            (&rest[1..1+end], false)
        } else if rest.starts_with('<') {
            let end = rest[1..].find('>').ok_or("Unterminated include angle brackets")?;
            (&rest[1..1+end], true)
        } else {
            return Err("Expected \"filename\" or <filename> after #include".to_string());
        };

        // Find the file
        let content = self.find_and_read_include(filename, current_file, is_system)?;

        // Recursively process the included file
        let processed = self.process(&content, filename)?;
        output.push_str(&processed);

        Ok(())
    }

    fn find_and_read_include(&self, filename: &str, current_file: &str, is_system: bool) -> Result<String, String> {
        // For non-system includes, first try relative to current file
        if !is_system {
            if let Some(parent) = Path::new(current_file).parent() {
                let path = parent.join(filename);
                if path.exists() {
                    return std::fs::read_to_string(&path)
                        .map_err(|e| format!("Cannot read {}: {}", path.display(), e));
                }
            }
        }

        // Search include paths
        for dir in &self.include_paths {
            let path = Path::new(dir).join(filename);
            if path.exists() {
                return std::fs::read_to_string(&path)
                    .map_err(|e| format!("Cannot read {}: {}", path.display(), e));
            }
        }

        Err(format!("Cannot find include file: {}", filename))
    }

    fn expand_macros(&self, line: &str) -> String {
        let mut result = line.to_string();

        // Simple token replacement - iterate until no more replacements
        let mut changed = true;
        let mut iterations = 0;
        while changed && iterations < 100 {
            changed = false;
            iterations += 1;

            for (name, value) in &self.defines {
                if !value.is_empty() {
                    // Only replace whole tokens (not parts of identifiers)
                    let new_result = replace_token(&result, name, value);
                    if new_result != result {
                        result = new_result;
                        changed = true;
                    }
                }
            }
        }

        result
    }
}

/// Replace a token (whole word) with its value
fn replace_token(text: &str, token: &str, replacement: &str) -> String {
    let mut result = String::new();
    let mut chars = text.chars().peekable();
    let mut i = 0;
    let bytes = text.as_bytes();

    while i < text.len() {
        // Check if we're at the start of the token
        if text[i..].starts_with(token) {
            // Check character before
            let before_ok = i == 0 || !is_ident_char(bytes[i - 1] as char);
            // Check character after
            let after_pos = i + token.len();
            let after_ok = after_pos >= text.len() || !is_ident_char(bytes[after_pos] as char);

            if before_ok && after_ok {
                result.push_str(replacement);
                i += token.len();
                continue;
            }
        }

        result.push(bytes[i] as char);
        i += 1;
    }

    result
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_define() {
        let mut pp = Preprocessor::new();
        let source = "#define MAX 100\nint x = MAX;";
        let result = pp.process(source, "test.c").unwrap();
        assert!(result.contains("int x = 100;"));
    }

    #[test]
    fn test_multiple_defines() {
        let mut pp = Preprocessor::new();
        let source = "#define WIDTH 80\n#define HEIGHT 25\nint area = WIDTH * HEIGHT;";
        let result = pp.process(source, "test.c").unwrap();
        assert!(result.contains("int area = 80 * 25;"));
    }

    #[test]
    fn test_no_partial_replace() {
        let mut pp = Preprocessor::new();
        let source = "#define MAX 100\nint MAXIMUM = 50;";
        let result = pp.process(source, "test.c").unwrap();
        assert!(result.contains("int MAXIMUM = 50;"));
    }
}
