// Integration tests for kz80_c compiler
// These tests compile C programs and verify the generated binary output

use std::process::Command;
use std::path::Path;
use std::fs;
use std::sync::atomic::{AtomicU64, Ordering};

static TEST_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Generate a unique ID for temp files
fn unique_id() -> u64 {
    TEST_COUNTER.fetch_add(1, Ordering::SeqCst)
}

/// Helper to compile a C program and return the binary
fn compile_c(source: &str) -> Result<Vec<u8>, String> {
    let id = unique_id();
    let temp_dir = std::env::temp_dir();
    let source_path = temp_dir.join(format!("test_input_{}.c", id));
    let output_path = temp_dir.join(format!("test_output_{}.bin", id));

    fs::write(&source_path, source).map_err(|e| e.to_string())?;

    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let compiler_path = manifest_dir.join("target/release/kz80_c");

    // Build the compiler if it doesn't exist
    if !compiler_path.exists() {
        let build_output = Command::new("cargo")
            .args(["build", "--release"])
            .current_dir(manifest_dir)
            .output()
            .map_err(|e| e.to_string())?;

        if !build_output.status.success() {
            return Err(format!(
                "Failed to build compiler: {}",
                String::from_utf8_lossy(&build_output.stderr)
            ));
        }
    }

    let output = Command::new(&compiler_path)
        .arg(&source_path)
        .arg("-o")
        .arg(&output_path)
        .output()
        .map_err(|e| e.to_string())?;

    // Clean up source file
    let _ = fs::remove_file(&source_path);

    if !output.status.success() {
        return Err(format!(
            "Compilation failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    let binary = fs::read(&output_path).map_err(|e| e.to_string())?;

    // Clean up output file
    let _ = fs::remove_file(&output_path);

    Ok(binary)
}

/// Helper to run a binary in the emulator and capture output
fn run_binary(binary: &[u8]) -> Result<String, String> {
    let id = unique_id();
    let temp_dir = std::env::temp_dir();
    let bin_path = temp_dir.join(format!("test_run_{}.bin", id));

    fs::write(&bin_path, binary).map_err(|e| e.to_string())?;

    // Find the emulator relative to the project
    let emulator_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("emulator")
        .join("retroshield");

    if !emulator_path.exists() {
        let _ = fs::remove_file(&bin_path);
        return Err(format!("Emulator not found at {:?}", emulator_path));
    }

    let output = Command::new(&emulator_path)
        .arg(&bin_path)
        .output()
        .map_err(|e| e.to_string())?;

    // Clean up binary file
    let _ = fs::remove_file(&bin_path);

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

/// Compile and run a C program, returning its output
fn compile_and_run(source: &str) -> Result<String, String> {
    let binary = compile_c(source)?;
    run_binary(&binary)
}

// ============================================================
// Basic Output Tests
// ============================================================

#[test]
fn test_hello_world() {
    let source = r#"
        int main() {
            puts("Hello, World!");
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("Hello, World!"), "Expected 'Hello, World!' in output: {}", output);
}

#[test]
fn test_putchar() {
    let source = r#"
        int main() {
            putchar(65);
            putchar(66);
            putchar(67);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("ABC"), "Expected 'ABC' in output: {}", output);
}

// ============================================================
// Arithmetic Tests
// ============================================================

#[test]
fn test_addition() {
    let source = r#"
        int main() {
            int x;
            x = 10 + 32;
            print_num(x);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("42"), "Expected '42' in output: {}", output);
}

#[test]
fn test_subtraction() {
    let source = r#"
        int main() {
            int x;
            x = 100 - 58;
            print_num(x);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("42"), "Expected '42' in output: {}", output);
}

#[test]
fn test_multiplication() {
    let source = r#"
        int main() {
            int x;
            x = 6 * 7;
            print_num(x);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("42"), "Expected '42' in output: {}", output);
}

#[test]
fn test_division() {
    let source = r#"
        int main() {
            int x;
            x = 84 / 2;
            print_num(x);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("42"), "Expected '42' in output: {}", output);
}

#[test]
fn test_modulo() {
    let source = r#"
        int main() {
            int x;
            x = 47 % 5;
            print_num(x);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("2"), "Expected '2' in output: {}", output);
}

// ============================================================
// Bitwise Operator Tests
// ============================================================

#[test]
fn test_shift_left() {
    let source = r#"
        int main() {
            int x;
            x = 1 << 4;
            print_num(x);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("16"), "Expected '16' in output: {}", output);
}

#[test]
fn test_shift_right() {
    let source = r#"
        int main() {
            int x;
            x = 64 >> 2;
            print_num(x);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("16"), "Expected '16' in output: {}", output);
}

#[test]
fn test_bitwise_and() {
    let source = r#"
        int main() {
            int x;
            x = 0xFF & 0x0F;
            print_num(x);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("15"), "Expected '15' in output: {}", output);
}

#[test]
fn test_bitwise_or() {
    let source = r#"
        int main() {
            int x;
            x = 0xF0 | 0x0F;
            print_num(x);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("255"), "Expected '255' in output: {}", output);
}

#[test]
fn test_bitwise_xor() {
    let source = r#"
        int main() {
            int x;
            x = 0xFF ^ 0xF0;
            print_num(x);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("15"), "Expected '15' in output: {}", output);
}

// ============================================================
// Control Flow Tests
// ============================================================

#[test]
fn test_if_true() {
    let source = r#"
        int main() {
            if (1) {
                puts("yes");
            } else {
                puts("no");
            }
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("yes"), "Expected 'yes' in output: {}", output);
}

#[test]
fn test_if_false() {
    let source = r#"
        int main() {
            if (0) {
                puts("yes");
            } else {
                puts("no");
            }
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("no"), "Expected 'no' in output: {}", output);
}

#[test]
fn test_while_loop() {
    let source = r#"
        int main() {
            int i;
            i = 0;
            while (i < 5) {
                putchar(65 + i);
                i = i + 1;
            }
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("ABCDE"), "Expected 'ABCDE' in output: {}", output);
}

#[test]
fn test_for_loop() {
    let source = r#"
        int main() {
            int i;
            for (i = 0; i < 5; i = i + 1) {
                putchar(65 + i);
            }
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("ABCDE"), "Expected 'ABCDE' in output: {}", output);
}

// ============================================================
// Function Tests
// ============================================================

#[test]
fn test_function_call() {
    let source = r#"
        int add(int a, int b) {
            return a + b;
        }

        int main() {
            int result;
            result = add(30, 12);
            print_num(result);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("42"), "Expected '42' in output: {}", output);
}

#[test]
fn test_recursion_factorial() {
    let source = r#"
        int factorial(int n) {
            if (n <= 1) {
                return 1;
            }
            return n * factorial(n - 1);
        }

        int main() {
            print_num(factorial(5));
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("120"), "Expected '120' in output: {}", output);
}

#[test]
fn test_recursion_fibonacci() {
    let source = r#"
        int fib(int n) {
            if (n <= 1) {
                return n;
            }
            return fib(n - 1) + fib(n - 2);
        }

        int main() {
            print_num(fib(10));
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("55"), "Expected '55' in output: {}", output);
}

// ============================================================
// Pointer Tests
// ============================================================

#[test]
fn test_int_pointer_deref() {
    let source = r#"
        int main() {
            int x;
            int *p;
            x = 12345;
            p = &x;
            print_num(*p);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("12345"), "Expected '12345' in output: {}", output);
}

#[test]
fn test_char_pointer_deref() {
    let source = r#"
        int main() {
            char c;
            char *p;
            c = 65;
            p = &c;
            putchar(*p);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("A"), "Expected 'A' in output: {}", output);
}

#[test]
fn test_pointer_write() {
    let source = r#"
        int main() {
            int x;
            int *p;
            x = 0;
            p = &x;
            *p = 999;
            print_num(x);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("999"), "Expected '999' in output: {}", output);
}

// ============================================================
// Array Tests
// ============================================================

#[test]
fn test_local_array() {
    let source = r#"
        int main() {
            int arr[5];
            arr[0] = 10;
            arr[1] = 20;
            arr[2] = 30;
            print_num(arr[0] + arr[1] + arr[2]);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("60"), "Expected '60' in output: {}", output);
}

#[test]
fn test_char_array() {
    let source = r#"
        int main() {
            char buf[5];
            buf[0] = 72;
            buf[1] = 105;
            buf[2] = 10;
            buf[3] = 0;
            putchar(buf[0]);
            putchar(buf[1]);
            putchar(buf[2]);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("Hi"), "Expected 'Hi' in output: {}", output);
}

// ============================================================
// Global Variable Tests
// ============================================================

#[test]
fn test_global_int() {
    let source = r#"
        int g;

        int main() {
            g = 42;
            print_num(g);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("42"), "Expected '42' in output: {}", output);
}

#[test]
fn test_global_modified_by_function() {
    let source = r#"
        int counter;

        void increment() {
            counter = counter + 1;
        }

        int main() {
            counter = 0;
            increment();
            increment();
            increment();
            print_num(counter);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("3"), "Expected '3' in output: {}", output);
}

// ============================================================
// Comparison Tests
// ============================================================

#[test]
fn test_less_than() {
    let source = r#"
        int main() {
            if (5 < 10) {
                puts("yes");
            } else {
                puts("no");
            }
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("yes"), "Expected 'yes' in output: {}", output);
}

#[test]
fn test_greater_than() {
    let source = r#"
        int main() {
            if (10 > 5) {
                puts("yes");
            } else {
                puts("no");
            }
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("yes"), "Expected 'yes' in output: {}", output);
}

#[test]
fn test_equal() {
    let source = r#"
        int main() {
            if (42 == 42) {
                puts("yes");
            } else {
                puts("no");
            }
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("yes"), "Expected 'yes' in output: {}", output);
}

#[test]
fn test_not_equal() {
    let source = r#"
        int main() {
            if (42 != 43) {
                puts("yes");
            } else {
                puts("no");
            }
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("yes"), "Expected 'yes' in output: {}", output);
}

// ============================================================
// Logical Operator Tests
// ============================================================

#[test]
fn test_logical_and() {
    let source = r#"
        int main() {
            if (1 && 1) {
                puts("yes");
            } else {
                puts("no");
            }
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("yes"), "Expected 'yes' in output: {}", output);
}

#[test]
fn test_logical_or() {
    let source = r#"
        int main() {
            if (0 || 1) {
                puts("yes");
            } else {
                puts("no");
            }
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("yes"), "Expected 'yes' in output: {}", output);
}

// ============================================================
// Complex Expression Tests
// ============================================================

#[test]
fn test_complex_expression() {
    let source = r#"
        int main() {
            int x;
            x = (10 + 5) * 2 - 8 / 4;
            print_num(x);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("28"), "Expected '28' in output: {}", output);
}

#[test]
fn test_nested_function_calls() {
    let source = r#"
        int double_it(int x) {
            return x * 2;
        }

        int add_one(int x) {
            return x + 1;
        }

        int main() {
            int result;
            result = double_it(add_one(20));
            print_num(result);
            putchar(10);
            return 0;
        }
    "#;

    let output = compile_and_run(source).expect("Failed to compile and run");
    assert!(output.contains("42"), "Expected '42' in output: {}", output);
}
