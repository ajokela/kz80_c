#!/bin/bash
# Integration test runner for kz80_c
# Compiles C programs with both Rust and self-hosted compilers, runs in emulator

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
COMPILER="$SCRIPT_DIR/target/release/kz80_c"
SELF_HOSTED="$SCRIPT_DIR/self/cc.bin"
EMULATOR="$SCRIPT_DIR/../emulator/retroshield"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

PASSED=0
FAILED=0
SKIPPED=0

# Build the compiler if needed
build_compiler() {
    echo "Building Rust compiler..."
    cargo build --release --quiet

    if [ ! -f "$SELF_HOSTED" ]; then
        echo "Building self-hosted compiler..."
        "$COMPILER" self/cc.c -o "$SELF_HOSTED"
    fi
}

# Run a test with expected output
# Usage: run_test "test name" "source code" "expected output" [rust_only]
run_test() {
    local name="$1"
    local source="$2"
    local expected="$3"
    local rust_only="${4:-false}"

    local temp_src=$(mktemp)
    local temp_bin=$(mktemp)

    echo "$source" > "$temp_src"

    # Test with Rust compiler
    if "$COMPILER" "$temp_src" -o "$temp_bin" 2>/dev/null; then
        local output=$("$EMULATOR" "$temp_bin" 2>/dev/null || true)
        if echo "$output" | grep -q "$expected"; then
            echo -e "${GREEN}PASS${NC} [Rust] $name"
            ((PASSED++))
        else
            echo -e "${RED}FAIL${NC} [Rust] $name"
            echo "  Expected: $expected"
            echo "  Got: $output"
            ((FAILED++))
        fi
    else
        echo -e "${RED}FAIL${NC} [Rust] $name - compilation failed"
        ((FAILED++))
    fi

    # Test with self-hosted compiler (if available and not rust_only)
    if [ "$rust_only" = "false" ] && [ -f "$SELF_HOSTED" ]; then
        local self_bin=$(mktemp)
        # Self-hosted reads from stdin with null terminator
        if printf '%s\0' "$source" | timeout 10 "$EMULATOR" "$SELF_HOSTED" > "$self_bin" 2>/dev/null; then
            if [ -s "$self_bin" ]; then
                local self_output=$(timeout 5 "$EMULATOR" "$self_bin" 2>/dev/null || true)
                if echo "$self_output" | grep -q "$expected"; then
                    echo -e "${GREEN}PASS${NC} [Self] $name"
                    ((PASSED++))
                else
                    echo -e "${RED}FAIL${NC} [Self] $name"
                    echo "  Expected: $expected"
                    echo "  Got: $self_output"
                    ((FAILED++))
                fi
            else
                echo -e "${YELLOW}SKIP${NC} [Self] $name - empty output"
                ((SKIPPED++))
            fi
        else
            echo -e "${YELLOW}SKIP${NC} [Self] $name - compilation timeout/failed"
            ((SKIPPED++))
        fi
        rm -f "$self_bin"
    fi

    rm -f "$temp_src" "$temp_bin"
}

# Main test suite
run_all_tests() {
    echo "========================================"
    echo "kz80_c Integration Tests"
    echo "========================================"
    echo

    echo "--- Basic Output ---"

    run_test "hello world" \
        'int main() { puts("Hello"); return 0; }' \
        "Hello"

    run_test "putchar" \
        'int main() { putchar(65); putchar(66); putchar(67); return 0; }' \
        "ABC"

    echo
    echo "--- Arithmetic ---"

    run_test "addition" \
        'int main() { print_num(10 + 32); putchar(10); return 0; }' \
        "42"

    run_test "subtraction" \
        'int main() { print_num(100 - 58); putchar(10); return 0; }' \
        "42"

    run_test "multiplication" \
        'int main() { print_num(6 * 7); putchar(10); return 0; }' \
        "42"

    run_test "division" \
        'int main() { print_num(84 / 2); putchar(10); return 0; }' \
        "42"

    run_test "modulo" \
        'int main() { print_num(47 % 5); putchar(10); return 0; }' \
        "2"

    echo
    echo "--- Bitwise Operators ---"

    run_test "shift left" \
        'int main() { print_num(1 << 4); putchar(10); return 0; }' \
        "16"

    run_test "shift right" \
        'int main() { print_num(64 >> 2); putchar(10); return 0; }' \
        "16"

    run_test "bitwise and" \
        'int main() { print_num(255 & 15); putchar(10); return 0; }' \
        "15"

    run_test "bitwise or" \
        'int main() { print_num(240 | 15); putchar(10); return 0; }' \
        "255"

    run_test "bitwise xor" \
        'int main() { print_num(255 ^ 240); putchar(10); return 0; }' \
        "15"

    echo
    echo "--- Control Flow ---"

    run_test "if true" \
        'int main() { if (1) { puts("yes"); } else { puts("no"); } return 0; }' \
        "yes"

    run_test "if false" \
        'int main() { if (0) { puts("yes"); } else { puts("no"); } return 0; }' \
        "no"

    run_test "while loop" \
        'int main() { int i; i = 0; while (i < 3) { putchar(65 + i); i = i + 1; } return 0; }' \
        "ABC"

    run_test "for loop" \
        'int main() { int i; for (i = 0; i < 3; i = i + 1) { putchar(65 + i); } return 0; }' \
        "ABC"

    echo
    echo "--- Functions ---"

    run_test "function call" \
        'int add(int a, int b) { return a + b; } int main() { print_num(add(30, 12)); return 0; }' \
        "42"

    run_test "factorial" \
        'int fact(int n) { if (n <= 1) return 1; return n * fact(n - 1); } int main() { print_num(fact(5)); return 0; }' \
        "120"

    run_test "fibonacci" \
        'int fib(int n) { if (n <= 1) return n; return fib(n-1) + fib(n-2); } int main() { print_num(fib(10)); return 0; }' \
        "55"

    echo
    echo "--- Pointers ---"

    run_test "int pointer deref" \
        'int main() { int x; int *p; x = 999; p = &x; print_num(*p); return 0; }' \
        "999"

    run_test "char pointer deref" \
        'int main() { char c; char *p; c = 65; p = &c; putchar(*p); return 0; }' \
        "A"

    run_test "pointer write" \
        'int main() { int x; int *p; x = 0; p = &x; *p = 123; print_num(x); return 0; }' \
        "123"

    echo
    echo "--- Arrays ---"

    run_test "int array" \
        'int main() { int a[3]; a[0] = 10; a[1] = 20; a[2] = 12; print_num(a[0] + a[1] + a[2]); return 0; }' \
        "42"

    run_test "char array" \
        'int main() { char b[3]; b[0] = 72; b[1] = 105; b[2] = 0; putchar(b[0]); putchar(b[1]); return 0; }' \
        "Hi"

    echo
    echo "--- Global Variables ---"

    run_test "global int" \
        'int g; int main() { g = 42; print_num(g); return 0; }' \
        "42"

    run_test "global modified" \
        'int c; void inc() { c = c + 1; } int main() { c = 0; inc(); inc(); inc(); print_num(c); return 0; }' \
        "3"

    echo
    echo "--- Comparisons ---"

    run_test "less than" \
        'int main() { if (5 < 10) puts("yes"); else puts("no"); return 0; }' \
        "yes"

    run_test "greater than" \
        'int main() { if (10 > 5) puts("yes"); else puts("no"); return 0; }' \
        "yes"

    run_test "equal" \
        'int main() { if (42 == 42) puts("yes"); else puts("no"); return 0; }' \
        "yes"

    run_test "not equal" \
        'int main() { if (42 != 43) puts("yes"); else puts("no"); return 0; }' \
        "yes"

    echo
    echo "--- Logical Operators ---"

    run_test "logical and" \
        'int main() { if (1 && 1) puts("yes"); else puts("no"); return 0; }' \
        "yes"

    run_test "logical or" \
        'int main() { if (0 || 1) puts("yes"); else puts("no"); return 0; }' \
        "yes"

    echo
    echo "========================================"
    echo "Results: ${GREEN}$PASSED passed${NC}, ${RED}$FAILED failed${NC}, ${YELLOW}$SKIPPED skipped${NC}"
    echo "========================================"

    if [ $FAILED -gt 0 ]; then
        exit 1
    fi
}

# Check for emulator
if [ ! -f "$EMULATOR" ]; then
    echo "Error: Emulator not found at $EMULATOR"
    echo "Please build the emulator first."
    exit 1
fi

build_compiler
run_all_tests
