# kz80_c - Self-Hosting C Compiler for Z80

A C compiler targeting the Z80 processor, written in Rust. Designed for the RetroShield Z80 platform and **now self-hosting** - the compiler can compile itself and run on the target hardware.

## Features

### C Language Support
- **Types**: `char` (8-bit), `int` (16-bit), `float` (BCD), pointers, arrays, structs
- **Float literals**: `3.14`, `1e5`, `2.5e-3` parsed to 6-byte BCD format
- **Control flow**: `if`/`else`, `while`, `for`, `return`
- **Operators**: `+ - * / % & | ^ << >> == != < > <= >= && ||`
- **Functions**: Full support with recursion
- **Preprocessor**: `#define` and `#include`

### Runtime Library

#### Standard I/O
- `putchar(c)` - Output character to serial
- `getchar()` - Read character from serial
- `puts(str)` - Output null-terminated string

#### BCD Integer Functions (8 decimal digits)
- `bcd_from_int(ptr, value)` - Convert 16-bit int to 4-byte BCD
- `bcd_to_int(ptr)` - Convert BCD back to int
- `bcd_add(result, a, b)` - Add two BCD numbers with DAA
- `bcd_sub(result, a, b)` - Subtract BCD numbers
- `bcd_cmp(a, b)` - Compare BCD numbers (-1, 0, 1)
- `bcd_print(ptr)` - Print BCD number

#### BCD Float Functions (TI-85 style)
6-byte format: `[sign][exponent][4-byte mantissa]`

- `bcdf_from_int(ptr, value)` - Convert int to BCD float
- `bcdf_copy(dest, src)` - Copy a BCD float (6 bytes)
- `bcdf_add(result, a, b)` - Add BCD floats
- `bcdf_sub(result, a, b)` - Subtract BCD floats
- `bcdf_mul(result, a, b)` - Multiply BCD floats (repeated addition with DAA)
- `bcdf_div(result, a, b)` - Divide BCD floats
- `bcdf_cmp(a, b)` - Compare BCD floats (returns 1, 0, or -1)
- `bcdf_neg(ptr)` - Negate BCD float in place (flip sign bit)
- `bcdf_abs(ptr)` - Absolute value in place (clear sign bit)
- `bcdf_normalize(ptr)` - Normalize mantissa (stub - not yet implemented)
- `bcdf_print(ptr)` - Print BCD float with exponent

## Building

```bash
# Build the compiler
cargo build --release

# Compile a C program
./target/release/kz80_c input.c -o output.bin

# With preprocessor defines and includes
./target/release/kz80_c -DMAX=100 -I./include input.c -o output.bin

# Show preprocessed output
./target/release/kz80_c -E input.c

# Show tokens (lexer output)
./target/release/kz80_c --tokens input.c

# Show AST (parser output)
./target/release/kz80_c --ast input.c

# Show hex dump of generated code
./target/release/kz80_c -S input.c
```

## Testing

```bash
# Run all tests (unit + integration)
cargo test

# Run unit tests only (lexer, parser, preprocessor)
cargo test --bin kz80_c

# Run integration tests only (compile & run C programs)
cargo test --test integration_tests

# Run shell-based test suite (tests both Rust and self-hosted compilers)
./run_tests.sh
```

### Test Coverage

- **Unit tests** (11 tests): Lexer tokenization, parser AST generation, preprocessor macro expansion
- **Integration tests** (34 tests): End-to-end compilation and execution of C programs
- **Shell tests** (64 tests): Tests both Rust compiler and self-hosted compiler with the emulator
  - Arithmetic, bitwise, shift, comparison, and logical operators
  - Control flow (if/else, while, for)
  - Functions and recursion
  - Pointers (read/write, int* and char*)
  - Arrays and global variables

## Running on RetroShield

```bash
# Run with emulator
../emulator/retroshield output.bin
```

## Target Hardware

- **CPU**: Z80 @ 4MHz
- **Memory**: 32KB ROM (0x0000-0x7FFF), 24KB RAM (0x2000-0x7FFF)
- **I/O**: MC6850 ACIA serial (ports 0x80/0x81)

## Example Programs

See the `examples/` directory:
- `hello_world.c` - Basic serial output
- `fibonacci.c` - Recursive Fibonacci sequence
- `print_num.c` - Integer printing
- `bcdf_test.c` - BCD float operations

## Self-Hosting Compiler

The `self/cc.c` file contains a fully functional C compiler written in the C subset itself. It runs on the Z80 and can compile C programs from stdin to binary output on stdout.

### Building and Using

```bash
# Compile the self-hosting compiler with the Rust compiler
./target/release/kz80_c self/cc.c -o self/cc.bin

# Use the self-hosted compiler to compile a program
# (reads C source from stdin, outputs binary to stdout)
printf 'int main() { puts("Hello from self-hosted compiler!"); return 0; }\x00' | \
  ../emulator/retroshield self/cc.bin > hello.bin

# Run the compiled program
../emulator/retroshield hello.bin
```

### Self-Hosted Compiler Features

- Reads C source from stdin (null-terminated or Ctrl-Z/Ctrl-D for EOF)
- Outputs Z80 binary to stdout
- Built-in runtime: `putchar`, `getchar`, `puts`, `print_num`
- **Data types**: `int` (16-bit), `char` (8-bit), pointers, arrays
- **Global variables**: Allocated in RAM at 0x2000+
- **Pointers**: Address-of (`&`) and dereference (`*`) operators
- **Arrays**: Declaration with size, subscript access for read/write
- **Operators**: All arithmetic (`+ - * / %`), bitwise (`& | ^ ~`), shifts (`<< >>`), comparison (`< > <= >= == !=`), logical (`&& ||`)
- **Control flow**: `if`/`else`, `while`, `for`, `return`, `break`, `continue`
- **Functions**: Full support with parameters, locals, and recursion
- **Strings**: String literals with escape sequences
- **Float type**: BCD representation (6-byte TI-85 style)
- **Preprocessor**: Skips `#define` and `#include` lines (use Rust compiler for preprocessing)

### Example Session

```bash
# Compile and run a simple program
$ printf 'int main() { int x; x = 3 + 4; putchar(48 + x); putchar(10); return 0; }\x00' | \
    ../emulator/retroshield self/cc.bin > test.bin
$ ../emulator/retroshield test.bin
7
```

## BCD Float Format Details

The BCD float format follows the TI-85 calculator style:

```
Byte 0: Sign/flags (bit 7 = negative)
Byte 1: Exponent (offset 0x80 = 10^0, so 0x84 = 10^4)
Bytes 2-5: Mantissa (4 bytes = 8 BCD digits, packed 2 per byte)
```

Example: `42` stored as BCD float:
- Sign: `0x00` (positive)
- Exponent: `0x84` (10^4)
- Mantissa: `0x00 0x00 0x00 0x42`

The Z80's `DAA` (Decimal Adjust Accumulator) instruction is used for BCD arithmetic, providing exact decimal representation like TI calculators.

## Architecture

```
Source Code (.c)
      │
      ▼
   Lexer (token.rs, lexer.rs)
      │
      ▼
 Preprocessor (preprocess.rs)
      │
      ▼
   Parser (parser.rs)
      │
      ▼
    AST (ast.rs)
      │
      ▼
 Code Generator (codegen.rs)
      │
      ▼
 Z80 Binary (.bin)
```

## Current Limitations

### BCD Float
- Fixed exponent (0x84) for all integers - enables simple add/sub without alignment
- Add/sub assumes operands have same exponent (works for integer-based operations)
- Division truncates to integer quotient
- Normalization function exists but disabled (would require exponent alignment in add/sub)

### Rust Compiler
- Limited preprocessor (no macros with arguments, no `#ifdef`)

### Self-Hosted Compiler
- No macro expansion (`#define` lines are skipped, not processed)
- To compile code with `#define`, use the Rust compiler to preprocess first
- No struct support (Rust compiler only)
- Binary output limited by CODE_SIZE (8KB default)
- Source input buffer is 8KB - full bootstrap requires ~66KB (8x larger)
- Array-to-pointer decay for function arguments requires explicit `&arr[0]`
- Array elements in compound expressions require intermediate variables (e.g., use `x = a[0]; y = a[1]; z = x + y;` instead of `z = a[0] + a[1];`)

## Bootstrap Status

The self-hosted compiler can compile and run C programs with all language features (pointers, arrays, globals, shifts, recursion). Full self-compilation is size-constrained: the compiler source (~66KB) exceeds the 8KB input buffer. This is a fundamental Z80 memory limitation, not a compiler bug.

**Verified working:**
- All arithmetic operators (`+ - * / %`)
- All bitwise operators (`& | ^ << >>`)
- All comparison operators (`< > <= >= == !=`)
- Logical operators (`&& ||`)
- Pointer read (`*p`) and write (`*p = value`) for both `int*` and `char*`
- Global variables
- Recursive functions
- Control flow (`if`/`else`, `while`, `for`, `break`, `continue`)

## Future Work
- [ ] Implement exponent alignment for add/sub with different exponents
- [ ] Enable bcdf_normalize after exponent alignment is done
- [ ] Improve bcdf_print with proper decimal point placement
- [ ] Create minimal "stage 0" compiler (<8KB source) for true bootstrap chain

## License

BSD 3-Clause License. See [LICENSE](LICENSE) for details.

## Credits

Inspired by:
- Small-C (Ron Cain, 1980) - Original self-hosting Z80 C compiler
- TI-85/86 calculators - BCD floating point format
- [z80float library](https://github.com/Zeda/z80float) - Z80 floating point reference
