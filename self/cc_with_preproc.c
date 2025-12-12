// kz80_c - A self-hosting C compiler for Z80
// This file can be compiled by the Rust kz80_c compiler,
// and the resulting binary can then compile itself.

// ============================================================
// Configuration
// ============================================================

#define CODE_SIZE 24576
#define DATA_SIZE 4096
#define SYM_SIZE 256
#define STR_SIZE 2048

// ============================================================
// Global State
// ============================================================

// Source input
char *src;           // Current position in source
int line;            // Current line number

// Token state
int tok;             // Current token
int tok_val;         // Token value (for numbers)
char tok_str[64];    // Token string (for identifiers)

// Code generation
char code[CODE_SIZE];
int code_pos;
int data_pos;        // For string literals

// Symbol table: parallel arrays (SYM_SIZE=256, 16 chars per name)
char sym_names[4096];  // 256 * 16
int sym_type[256];     // Type: 0=global, 1=local, 2=param, 3=func
int sym_value[256];    // Address or offset
int sym_dtype[256];    // Data type: TYPE_INT, TYPE_CHAR, TYPE_FLOAT, TYPE_PTR
int sym_count;
int local_offset;

// Float literal storage (for constants)
char float_lits[512];  // Storage for float literal strings
int float_lit_pos;

// Runtime function addresses (to be patched)
int rt_bcdf_add;
int rt_bcdf_sub;
int rt_bcdf_mul;
int rt_bcdf_div;
int rt_bcdf_cmp;
int rt_bcdf_from_int;

// Integer arithmetic runtime function addresses
int rt_mul16;
int rt_div16;
int rt_mod16;
int rt_shl16;
int rt_shr16;

// String literals
char strings[STR_SIZE];
int str_pos;

// Labels for control flow
int label_count;

// Global variables position (starts in RAM at 0x2000)
int global_pos;

// Preprocessor defines (simple macro storage)
// Each define uses 16 chars for name + 16 chars for value = 32 bytes
// Support up to 64 defines = 2048 bytes
char def_names[1024];     // 64 * 16
char def_values[1024];    // 64 * 16
int def_count;

// ============================================================
// Token Types
// ============================================================

#define TK_EOF 0
#define TK_NUM 1
#define TK_IDENT 2
#define TK_STR 3
#define TK_CHAR 4

// Keywords (100+)
#define TK_INT 100
#define TK_CHAR_KW 101
#define TK_VOID 102
#define TK_FLOAT 103
#define TK_IF 104
#define TK_ELSE 105
#define TK_WHILE 106
#define TK_FOR 107
#define TK_RETURN 108
#define TK_BREAK 109
#define TK_CONTINUE 110

// Float literal
#define TK_FLOAT_LIT 5

// Data types for symbol table
#define TYPE_INT 0
#define TYPE_CHAR 1
#define TYPE_FLOAT 2
#define TYPE_PTR 3

// Operators (200+)
#define TK_PLUS 200
#define TK_MINUS 201
#define TK_STAR 202
#define TK_SLASH 203
#define TK_PERCENT 204
#define TK_AMP 205
#define TK_PIPE 206
#define TK_CARET 207
#define TK_LT 208
#define TK_GT 209
#define TK_EQ 210
#define TK_BANG 211
#define TK_TILDE 212

// Compound operators (220+)
#define TK_EQEQ 220
#define TK_BANGEQ 221
#define TK_LTEQ 222
#define TK_GTEQ 223
#define TK_AMPAMP 224
#define TK_PIPEPIPE 225
#define TK_LTLT 226
#define TK_GTGT 227
#define TK_PLUSPLUS 228
#define TK_MINUSMINUS 229

// Delimiters (240+)
#define TK_LPAREN 240
#define TK_RPAREN 241
#define TK_LBRACE 242
#define TK_RBRACE 243
#define TK_LBRACKET 244
#define TK_RBRACKET 245
#define TK_SEMICOLON 246
#define TK_COMMA 247

// ============================================================
// I/O Functions (built-in from runtime)
// ============================================================

// These are provided by the kz80_c runtime:
// int putchar(int c);
// int getchar();
// int puts(char *s);

void print_num(int n) {
    if (n < 0) {
        putchar('-');
        n = 0 - n;
    }
    if (n >= 10) {
        print_num(n / 10);
    }
    putchar('0' + n % 10);
}

void print_hex(int n) {
    int i;
    int d;
    i = 12;
    while (i >= 0) {
        d = (n >> i) & 15;
        if (d < 10) {
            putchar('0' + d);
        } else {
            putchar('A' + d - 10);
        }
        i = i - 4;
    }
}

void error(char *msg) {
    puts("Error line ");
    print_num(line);
    puts(": ");
    puts(msg);
    putchar('\n');
    while (1) {}  // Halt
}

// ============================================================
// String Functions
// ============================================================

int strlen(char *s) {
    int len;
    len = 0;
    while (s[len]) {
        len = len + 1;
    }
    return len;
}

void strcpy(char *dst, char *src) {
    while (*src) {
        *dst = *src;
        dst = dst + 1;
        src = src + 1;
    }
    *dst = 0;
}

int strcmp(char *a, char *b) {
    while (*a && *b && *a == *b) {
        a = a + 1;
        b = b + 1;
    }
    return *a - *b;
}

// ============================================================
// Lexer
// ============================================================

int is_alpha(int c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

int is_digit(int c) {
    return c >= '0' && c <= '9';
}

int is_alnum(int c) {
    return is_alpha(c) || is_digit(c);
}

// ============================================================
// Preprocessor Functions
// ============================================================

// Add a #define: store name and value
void add_define(char *name, char *value) {
    int base_n;
    int base_v;
    int i;

    if (def_count >= 64) {
        error("Too many defines");
    }

    base_n = def_count * 16;
    base_v = def_count * 16;

    // Copy name
    i = 0;
    while (name[i] != 0 && i < 15) {
        def_names[base_n + i] = name[i];
        i = i + 1;
    }
    def_names[base_n + i] = 0;

    // Copy value
    i = 0;
    while (value[i] != 0 && i < 15) {
        def_values[base_v + i] = value[i];
        i = i + 1;
    }
    def_values[base_v + i] = 0;

    def_count = def_count + 1;
}

// Find a define by name, return index or -1 if not found
int find_define(char *name) {
    int i;
    int j;
    int base;
    int match;

    i = 0;
    while (i < def_count) {
        base = i * 16;
        j = 0;
        match = 1;
        while (def_names[base + j] != 0 || name[j] != 0) {
            if (def_names[base + j] != name[j]) {
                match = 0;
                break;
            }
            j = j + 1;
        }
        if (match) {
            return i;
        }
        i = i + 1;
    }
    return -1;
}

// Get define value by index
char *get_define_value(int idx) {
    return &def_values[idx * 16];
}

// Process a #define line at current src position
// src should point to first char after "#define "
void process_define() {
    char name[64];
    char value[64];
    int i;

    // Skip leading whitespace
    while (*src == ' ' || *src == '\t') {
        src = src + 1;
    }

    // Read name
    i = 0;
    while (is_alnum(*src) || *src == '_') {
        if (i < 63) {
            name[i] = *src;
            i = i + 1;
        }
        src = src + 1;
    }
    name[i] = 0;

    // Skip whitespace between name and value
    while (*src == ' ' || *src == '\t') {
        src = src + 1;
    }

    // Read value until end of line
    i = 0;
    while (*src != 0 && *src != '\n' && *src != '\r') {
        if (i < 63) {
            value[i] = *src;
            i = i + 1;
        }
        src = src + 1;
    }
    value[i] = 0;

    // Skip newline
    if (*src == '\r') src = src + 1;
    if (*src == '\n') {
        src = src + 1;
        line = line + 1;
    }

    add_define(name, value);
}

// Skip to end of line (for unknown # directives)
void skip_to_eol() {
    while (*src != 0 && *src != '\n') {
        src = src + 1;
    }
    if (*src == '\n') {
        src = src + 1;
        line = line + 1;
    }
}

void skip_ws() {
    while (*src == ' ' || *src == '\t' || *src == '\n' || *src == '\r') {
        if (*src == '\n') {
            line = line + 1;
        }
        src = src + 1;
    }
}

void next() {
    int c;
    int i;
    int def_idx;
    char *def_val;

    skip_ws();

    // Skip comments
    while (*src == '/' && src[1] == '/') {
        while (*src && *src != '\n') {
            src = src + 1;
        }
        skip_ws();
    }

    // Handle preprocessor directives
    while (*src == '#') {
        src = src + 1;  // Skip '#'
        // Skip whitespace after #
        while (*src == ' ' || *src == '\t') {
            src = src + 1;
        }
        // Check for "define"
        if (src[0] == 'd' && src[1] == 'e' && src[2] == 'f' && src[3] == 'i' &&
            src[4] == 'n' && src[5] == 'e' && (src[6] == ' ' || src[6] == '\t')) {
            src = src + 6;  // Skip "define"
            process_define();
        } else {
            // Unknown directive - skip to end of line
            skip_to_eol();
        }
        skip_ws();
        // Check for more comments
        while (*src == '/' && src[1] == '/') {
            while (*src && *src != '\n') {
                src = src + 1;
            }
            skip_ws();
        }
    }

    c = *src;

    if (c == 0) {
        tok = TK_EOF;
        return;
    }

    // Number (integer or float)
    if (is_digit(c)) {
        tok_val = 0;
        if (c == '0' && (src[1] == 'x' || src[1] == 'X')) {
            // Hex number
            src = src + 2;
            while (1) {
                c = *src;
                if (c >= '0' && c <= '9') {
                    tok_val = tok_val * 16 + c - '0';
                } else if (c >= 'a' && c <= 'f') {
                    tok_val = tok_val * 16 + c - 'a' + 10;
                } else if (c >= 'A' && c <= 'F') {
                    tok_val = tok_val * 16 + c - 'A' + 10;
                } else {
                    break;
                }
                src = src + 1;
            }
            tok = TK_NUM;
            return;
        }
        // Decimal - check if float
        i = 0;
        while (is_digit(*src)) {
            tok_str[i] = *src;
            tok_val = tok_val * 10 + *src - '0';
            src = src + 1;
            i = i + 1;
        }
        // Check for decimal point (not followed by non-digit which could be range)
        if (*src == '.' && is_digit(src[1])) {
            // Float literal
            tok_str[i] = '.';
            i = i + 1;
            src = src + 1;
            while (is_digit(*src)) {
                tok_str[i] = *src;
                src = src + 1;
                i = i + 1;
            }
            // Check for exponent
            if (*src == 'e' || *src == 'E') {
                tok_str[i] = *src;
                i = i + 1;
                src = src + 1;
                if (*src == '+' || *src == '-') {
                    tok_str[i] = *src;
                    i = i + 1;
                    src = src + 1;
                }
                while (is_digit(*src)) {
                    tok_str[i] = *src;
                    i = i + 1;
                    src = src + 1;
                }
            }
            tok_str[i] = 0;
            tok = TK_FLOAT_LIT;
            return;
        }
        // Check for exponent without decimal point
        if (*src == 'e' || *src == 'E') {
            tok_str[i] = *src;
            i = i + 1;
            src = src + 1;
            if (*src == '+' || *src == '-') {
                tok_str[i] = *src;
                i = i + 1;
                src = src + 1;
            }
            while (is_digit(*src)) {
                tok_str[i] = *src;
                i = i + 1;
                src = src + 1;
            }
            tok_str[i] = 0;
            tok = TK_FLOAT_LIT;
            return;
        }
        tok = TK_NUM;
        return;
    }

    // Identifier or keyword
    if (is_alpha(c)) {
        i = 0;
        while (is_alnum(*src) && i < 63) {
            tok_str[i] = *src;
            i = i + 1;
            src = src + 1;
        }
        tok_str[i] = 0;

        // Check keywords
        if (strcmp(tok_str, "int") == 0) { tok = TK_INT; return; }
        if (strcmp(tok_str, "char") == 0) { tok = TK_CHAR_KW; return; }
        if (strcmp(tok_str, "void") == 0) { tok = TK_VOID; return; }
        if (strcmp(tok_str, "float") == 0) { tok = TK_FLOAT; return; }
        if (strcmp(tok_str, "if") == 0) { tok = TK_IF; return; }
        if (strcmp(tok_str, "else") == 0) { tok = TK_ELSE; return; }
        if (strcmp(tok_str, "while") == 0) { tok = TK_WHILE; return; }
        if (strcmp(tok_str, "for") == 0) { tok = TK_FOR; return; }
        if (strcmp(tok_str, "return") == 0) { tok = TK_RETURN; return; }
        if (strcmp(tok_str, "break") == 0) { tok = TK_BREAK; return; }
        if (strcmp(tok_str, "continue") == 0) { tok = TK_CONTINUE; return; }

        // Check for macro expansion
        def_idx = find_define(tok_str);
        if (def_idx >= 0) {
            def_val = get_define_value(def_idx);
            // Check if value is a number
            if (is_digit(def_val[0]) || (def_val[0] == '-' && is_digit(def_val[1]))) {
                // Parse as number
                tok_val = 0;
                i = 0;
                if (def_val[0] == '-') {
                    i = 1;
                }
                while (is_digit(def_val[i])) {
                    tok_val = tok_val * 10 + (def_val[i] - '0');
                    i = i + 1;
                }
                if (def_val[0] == '-') {
                    tok_val = -tok_val;
                }
                tok = TK_NUM;
                return;
            }
            // Otherwise copy value to tok_str and re-check keywords
            i = 0;
            while (def_val[i] != 0 && i < 63) {
                tok_str[i] = def_val[i];
                i = i + 1;
            }
            tok_str[i] = 0;
            // Re-check keywords
            if (strcmp(tok_str, "int") == 0) { tok = TK_INT; return; }
            if (strcmp(tok_str, "char") == 0) { tok = TK_CHAR_KW; return; }
            if (strcmp(tok_str, "void") == 0) { tok = TK_VOID; return; }
            if (strcmp(tok_str, "float") == 0) { tok = TK_FLOAT; return; }
            if (strcmp(tok_str, "if") == 0) { tok = TK_IF; return; }
            if (strcmp(tok_str, "else") == 0) { tok = TK_ELSE; return; }
            if (strcmp(tok_str, "while") == 0) { tok = TK_WHILE; return; }
            if (strcmp(tok_str, "for") == 0) { tok = TK_FOR; return; }
            if (strcmp(tok_str, "return") == 0) { tok = TK_RETURN; return; }
            if (strcmp(tok_str, "break") == 0) { tok = TK_BREAK; return; }
            if (strcmp(tok_str, "continue") == 0) { tok = TK_CONTINUE; return; }
        }

        tok = TK_IDENT;
        return;
    }

    // String literal
    if (c == '"') {
        src = src + 1;
        i = 0;
        while (*src != '"' && *src && i < 63) {
            if (*src == '\\') {
                src = src + 1;
                if (*src == 'n') { tok_str[i] = '\n'; }
                else if (*src == 'r') { tok_str[i] = '\r'; }
                else if (*src == 't') { tok_str[i] = '\t'; }
                else if (*src == '0') { tok_str[i] = 0; }
                else { tok_str[i] = *src; }
            } else {
                tok_str[i] = *src;
            }
            i = i + 1;
            src = src + 1;
        }
        tok_str[i] = 0;
        if (*src == '"') { src = src + 1; }
        tok = TK_STR;
        return;
    }

    // Character literal
    if (c == '\'') {
        src = src + 1;
        if (*src == '\\') {
            src = src + 1;
            if (*src == 'n') { tok_val = '\n'; }
            else if (*src == 'r') { tok_val = '\r'; }
            else if (*src == 't') { tok_val = '\t'; }
            else if (*src == '0') { tok_val = 0; }
            else { tok_val = *src; }
        } else {
            tok_val = *src;
        }
        src = src + 1;
        if (*src == '\'') { src = src + 1; }
        tok = TK_CHAR;
        return;
    }

    // Operators
    src = src + 1;

    if (c == '+') {
        if (*src == '+') { src = src + 1; tok = TK_PLUSPLUS; return; }
        tok = TK_PLUS; return;
    }
    if (c == '-') {
        if (*src == '-') { src = src + 1; tok = TK_MINUSMINUS; return; }
        tok = TK_MINUS; return;
    }
    if (c == '*') { tok = TK_STAR; return; }
    if (c == '/') { tok = TK_SLASH; return; }
    if (c == '%') { tok = TK_PERCENT; return; }
    if (c == '&') {
        if (*src == '&') { src = src + 1; tok = TK_AMPAMP; return; }
        tok = TK_AMP; return;
    }
    if (c == '|') {
        if (*src == '|') { src = src + 1; tok = TK_PIPEPIPE; return; }
        tok = TK_PIPE; return;
    }
    if (c == '^') { tok = TK_CARET; return; }
    if (c == '<') {
        if (*src == '<') { src = src + 1; tok = TK_LTLT; return; }
        if (*src == '=') { src = src + 1; tok = TK_LTEQ; return; }
        tok = TK_LT; return;
    }
    if (c == '>') {
        if (*src == '>') { src = src + 1; tok = TK_GTGT; return; }
        if (*src == '=') { src = src + 1; tok = TK_GTEQ; return; }
        tok = TK_GT; return;
    }
    if (c == '=') {
        if (*src == '=') { src = src + 1; tok = TK_EQEQ; return; }
        tok = TK_EQ; return;
    }
    if (c == 33) {  // '!'
        if (*src == '=') { src = src + 1; tok = TK_BANGEQ; return; }
        tok = TK_BANG; return;
    }
    if (c == 126) { tok = TK_TILDE; return; }  // '~'

    // Delimiters
    if (c == '(') { tok = TK_LPAREN; return; }
    if (c == ')') { tok = TK_RPAREN; return; }
    if (c == '{') { tok = TK_LBRACE; return; }
    if (c == '}') { tok = TK_RBRACE; return; }
    if (c == '[') { tok = TK_LBRACKET; return; }
    if (c == ']') { tok = TK_RBRACKET; return; }
    if (c == ';') { tok = TK_SEMICOLON; return; }
    if (c == ',') { tok = TK_COMMA; return; }

    error("Unknown character");
}

void expect(int t) {
    if (tok != t) {
        error("Unexpected token");
    }
    next();
}

// ============================================================
// Symbol Table
// ============================================================

int find_sym(char *name) {
    int i;
    i = sym_count - 1;
    while (i >= 0) {
        if (strcmp(sym_names + i * 16, name) == 0) {
            return i;
        }
        i = i - 1;
    }
    return -1;
}

int add_sym(char *name, int type, int value) {
    int i;
    if (sym_count >= SYM_SIZE) {
        error("Symbol table full");
    }
    i = sym_count;
    strcpy(sym_names + i * 16, name);
    sym_type[i] = type;
    sym_value[i] = value;
    sym_dtype[i] = TYPE_INT;  // Default to int, caller can override
    sym_count = sym_count + 1;
    return i;
}

int add_sym_typed(char *name, int type, int value, int dtype) {
    int i;
    i = add_sym(name, type, value);
    sym_dtype[i] = dtype;
    return i;
}

// ============================================================
// Code Generation Helpers
// ============================================================

void emit(int b) {
    code[code_pos] = b;
    code_pos = code_pos + 1;
}

void emit16(int w) {
    int lo;
    int hi;
    lo = w & 255;
    hi = (w >> 8) & 255;
    emit(lo);
    emit(hi);
}

void patch16(int addr, int val) {
    code[addr] = val & 255;
    code[addr + 1] = (val >> 8) & 255;
}

int new_label() {
    label_count = label_count + 1;
    return label_count;
}

// Z80 instruction emitters
void emit_ld_hl_nn(int n) { emit(0x21); emit16(n); }
void emit_ld_de_nn(int n) { emit(0x11); emit16(n); }
void emit_ld_a_n(int n) { emit(0x3E); emit(n); }
void emit_push_hl() { emit(0xE5); }
void emit_pop_hl() { emit(0xE1); }
void emit_push_de() { emit(0xD5); }
void emit_pop_de() { emit(0xD1); }
void emit_push_ix() { emit(0xDD); emit(0xE5); }
void emit_pop_ix() { emit(0xDD); emit(0xE1); }
void emit_add_hl_de() { emit(0x19); }
void emit_add_hl_sp() { emit(0x39); }
void emit_ld_sp_hl() { emit(0xF9); }
void emit_ld_sp_ix() { emit(0xDD); emit(0xF9); }
void emit_ex_de_hl() { emit(0xEB); }
void emit_call(int addr) { emit(0xCD); emit16(addr); }
void emit_jp(int addr) { emit(0xC3); emit16(addr); }
void emit_jp_z(int addr) { emit(0xCA); emit16(addr); }
void emit_jp_nz(int addr) { emit(0xC2); emit16(addr); }
void emit_ret() { emit(0xC9); }
void emit_halt() { emit(0x76); }

void emit_ld_l_ix_d(int d) { emit(0xDD); emit(0x6E); emit(d); }
void emit_ld_h_ix_d(int d) { emit(0xDD); emit(0x66); emit(d); }
void emit_ld_ix_d_l(int d) { emit(0xDD); emit(0x75); emit(d); }
void emit_ld_ix_d_h(int d) { emit(0xDD); emit(0x74); emit(d); }

void emit_ld_a_hl() { emit(0x7E); }
void emit_ld_hl_a() { emit(0x77); }
void emit_ld_a_de() { emit(0x1A); }
void emit_ld_de_a() { emit(0x12); }
void emit_ld_l_a() { emit(0x6F); }
void emit_ld_h_a() { emit(0x67); }
void emit_ld_h_n(int n) { emit(0x26); emit(n); }
void emit_ld_h_hl() { emit(0x66); }  // LD H,(HL)
void emit_ld_a_h() { emit(0x7C); }
void emit_ld_a_l() { emit(0x7D); }
void emit_ld_a_d() { emit(0x7A); }
void emit_ld_a_e() { emit(0x7B); }
void emit_ld_e_a() { emit(0x5F); }  // LD E,A
void emit_or_e() { emit(0xB3); }
void emit_or_l() { emit(0xB5); }
void emit_or_a() { emit(0xB7); }
void emit_sbc_hl_de() { emit(0xED); emit(0x52); }
void emit_inc_hl() { emit(0x23); }
void emit_dec_hl() { emit(0x2B); }
void emit_inc_de() { emit(0x13); }
void emit_add_hl_hl() { emit(0x29); }  // ADD HL,HL (double HL)
void emit_ld_bc_nn(int n) { emit(0x01); emit16(n); }
void emit_ldir() { emit(0xED); emit(0xB0); }

// ============================================================
// Runtime Library Emission
// ============================================================

// Emit the built-in runtime functions
// Returns the address after runtime code
void emit_runtime() {
    int putchar_addr;
    int getchar_addr;
    int puts_addr;
    int wait_tx;
    int wait_rx;
    int puts_loop;
    int puts_done;
    int pn_div_patch;   // Address to patch for div16 call in print_num
    int pn_mod_patch;   // Address to patch for mod16 call in print_num

    // putchar: output character in L to serial (MC6850 ACIA)
    putchar_addr = code_pos;
    add_sym("putchar", 3, putchar_addr);
    emit(0xF5);  // PUSH AF
    wait_tx = code_pos;
    emit(0xDB); emit(0x80);  // IN A,(0x80) - status
    emit(0xE6); emit(0x02);  // AND 0x02 - TX ready bit
    emit_jp_z(wait_tx);      // JP Z,wait_tx
    emit(0x7D);              // LD A,L
    emit(0xD3); emit(0x81);  // OUT (0x81),A - data
    emit(0xF1);              // POP AF
    emit_ret();

    // getchar: read character from serial, return in HL
    getchar_addr = code_pos;
    add_sym("getchar", 3, getchar_addr);
    wait_rx = code_pos;
    emit(0xDB); emit(0x80);  // IN A,(0x80) - status
    emit(0xE6); emit(0x01);  // AND 0x01 - RX ready bit
    emit_jp_z(wait_rx);      // JP Z,wait_rx
    emit(0xDB); emit(0x81);  // IN A,(0x81) - data
    emit(0x6F);              // LD L,A
    emit(0x26); emit(0x00);  // LD H,0
    emit_ret();

    // puts: output null-terminated string at HL
    // Note: HL contains string pointer (from first argument at IX+4)
    puts_addr = code_pos;
    add_sym("puts", 3, puts_addr);
    // HL already has the string pointer from caller
    puts_loop = code_pos;
    emit_ld_a_hl();          // LD A,(HL)
    emit(0xB7);              // OR A (check null)
    puts_done = code_pos + 3;  // Address for JP after we emit it
    emit_jp_z(0);            // JP Z,done (placeholder)
    emit(0xE5);              // PUSH HL (save string pointer)
    emit(0x6F);              // LD L,A (char to output)
    emit_call(putchar_addr); // CALL putchar
    emit(0xE1);              // POP HL (restore string pointer)
    emit(0x23);              // INC HL
    emit_jp(puts_loop);      // JP puts_loop
    // Patch the done address
    code[puts_done - 2] = code_pos & 0xFF;
    code[puts_done - 1] = (code_pos >> 8) & 0xFF;
    emit_ret();

    // print_num: print signed 16-bit integer in HL
    // Uses recursive digit extraction with div/mod by 10
    {
        int print_num_addr;
        int pn_positive;
        int pn_recurse;
        int pn_done;

        print_num_addr = code_pos;
        add_sym("print_num", 3, print_num_addr);

        // Check if negative (bit 15 set)
        emit(0x7C);              // LD A,H
        emit(0xB7);              // OR A (sets sign flag)
        pn_positive = code_pos + 3;
        emit(0xF2); emit16(0);   // JP P,pn_positive (if positive)

        // Negative: print '-' and negate
        emit(0xE5);              // PUSH HL
        emit(0x2E); emit(0x2D);  // LD L,'-'
        emit_call(putchar_addr);
        emit(0xE1);              // POP HL
        // Negate HL: HL = 0 - HL
        emit(0x7D);              // LD A,L
        emit(0x2F);              // CPL
        emit(0x6F);              // LD L,A
        emit(0x7C);              // LD A,H
        emit(0x2F);              // CPL
        emit(0x67);              // LD H,A
        emit(0x23);              // INC HL (two's complement)

        // pn_positive:
        code[pn_positive - 2] = code_pos & 0xFF;
        code[pn_positive - 1] = (code_pos >> 8) & 0xFF;

        // If HL >= 10, recurse with HL/10
        emit(0xE5);              // PUSH HL (save original)
        emit(0x11); emit16(10);  // LD DE,10
        emit(0xEB);              // EX DE,HL (now DE=n, HL=10)
        // Call div16: HL = DE / HL
        // We need div16 address, but it's not defined yet
        // Use a forward reference - save address for patching later
        pn_div_patch = code_pos + 1;  // Address of the 16-bit operand
        emit_call(0);            // CALL div16 (placeholder)
        // HL now has quotient
        emit(0x7C);              // LD A,H
        emit(0xB5);              // OR L
        pn_done = code_pos + 3;
        emit_jp_z(0);            // JP Z,pn_done (if quotient is 0)
        // Recurse with quotient
        emit_call(print_num_addr);

        // pn_done:
        code[pn_done - 2] = code_pos & 0xFF;
        code[pn_done - 1] = (code_pos >> 8) & 0xFF;

        // Print digit: original % 10 + '0'
        emit(0xE1);              // POP HL (restore original)
        emit(0x11); emit16(10);  // LD DE,10
        emit(0xEB);              // EX DE,HL
        // Call mod16: HL = DE % HL
        // Save address for patching after mod16 is defined
        pn_mod_patch = code_pos + 1;  // Address of the 16-bit operand
        emit_call(0);        // CALL mod16 (placeholder - will patch)
        // HL now has remainder (0-9)
        emit(0x7D);          // LD A,L
        emit(0xC6); emit(0x30); // ADD A,'0'
        emit(0x6F);          // LD L,A
        emit_call(putchar_addr);
        emit_ret();
    }

    // __mul16: HL = DE * HL (unsigned 16-bit multiply)
    // Uses shift-and-add algorithm
    rt_mul16 = code_pos;
    emit(0xC5);              // PUSH BC
    emit(0x01); emit16(0);   // LD BC,0 (result)
    // mul_loop:
    {
        int mul_loop;
        int mul_done;
        int mul_skip;
        mul_loop = code_pos;
        // Check if DE is zero
        emit(0x7A);          // LD A,D
        emit(0xB3);          // OR E
        mul_done = code_pos + 3;
        emit_jp_z(0);        // JP Z,mul_done (placeholder)
        // If DE bit 0 set, add HL to BC
        emit(0x7B);          // LD A,E
        emit(0xE6); emit(0x01);  // AND 1
        mul_skip = code_pos + 3;
        emit_jp_z(0);        // JP Z,mul_skip (placeholder)
        // BC += HL
        emit(0xE5);          // PUSH HL
        emit(0x09);          // ADD HL,BC
        emit(0x44);          // LD B,H
        emit(0x4D);          // LD C,L
        emit(0xE1);          // POP HL
        // mul_skip:
        code[mul_skip - 2] = code_pos & 0xFF;
        code[mul_skip - 1] = (code_pos >> 8) & 0xFF;
        // HL <<= 1
        emit(0x29);          // ADD HL,HL
        // DE >>= 1
        emit(0xCB); emit(0x3A);  // SRL D
        emit(0xCB); emit(0x1B);  // RR E
        emit_jp(mul_loop);   // JP mul_loop
        // mul_done:
        code[mul_done - 2] = code_pos & 0xFF;
        code[mul_done - 1] = (code_pos >> 8) & 0xFF;
        // Result in BC, move to HL
        emit(0x60);          // LD H,B
        emit(0x69);          // LD L,C
        emit(0xC1);          // POP BC
        emit_ret();
    }

    // __div16: HL = DE / HL (unsigned 16-bit divide)
    // Note: HL=divisor, DE=dividend, returns HL=quotient
    rt_div16 = code_pos;
    emit(0xC5);              // PUSH BC
    // Exchange DE and HL so HL=dividend, DE=divisor
    emit(0xEB);              // EX DE,HL
    // Check for divide by zero
    emit(0x7A);              // LD A,D
    emit(0xB3);              // OR E
    {
        int div_ok;
        int div_loop;
        int div_done;
        div_ok = code_pos + 3;
        emit(0xC2); emit16(0);  // JP NZ,div_ok (placeholder)
        // Divide by zero - return 0
        emit_ld_hl_nn(0);
        emit(0xC1);          // POP BC
        emit_ret();
        // div_ok:
        code[div_ok - 2] = code_pos & 0xFF;
        code[div_ok - 1] = (code_pos >> 8) & 0xFF;
        // Simple subtraction-based division
        emit(0x01); emit16(0);   // LD BC,0 (quotient)
        div_loop = code_pos;
        // If HL < DE, we're done
        emit(0xE5);          // PUSH HL
        emit(0xB7);          // OR A (clear carry)
        emit(0xED); emit(0x52);  // SBC HL,DE
        emit(0xE1);          // POP HL
        div_done = code_pos + 3;
        emit(0xDA); emit16(0);  // JP C,div_done (placeholder)
        // HL -= DE
        emit(0xB7);          // OR A
        emit(0xED); emit(0x52);  // SBC HL,DE
        // BC++
        emit(0x03);          // INC BC
        emit_jp(div_loop);   // JP div_loop
        // div_done:
        code[div_done - 2] = code_pos & 0xFF;
        code[div_done - 1] = (code_pos >> 8) & 0xFF;
        // Quotient in BC, move to HL
        emit(0x60);          // LD H,B
        emit(0x69);          // LD L,C
        emit(0xC1);          // POP BC
        emit_ret();
    }

    // __mod16: HL = DE % HL (unsigned 16-bit modulo)
    // Note: HL=divisor, DE=dividend, returns HL=remainder
    rt_mod16 = code_pos;
    emit(0xC5);              // PUSH BC
    emit(0xD5);              // PUSH DE
    // Exchange DE and HL so HL=dividend, DE=divisor
    emit(0xEB);              // EX DE,HL
    // Check for mod by zero
    emit(0x7A);              // LD A,D
    emit(0xB3);              // OR E
    {
        int mod_ok;
        int mod_loop;
        int mod_done;
        mod_ok = code_pos + 3;
        emit(0xC2); emit16(0);  // JP NZ,mod_ok (placeholder)
        // Mod by zero - return 0
        emit_ld_hl_nn(0);
        emit(0xD1);          // POP DE
        emit(0xC1);          // POP BC
        emit_ret();
        // mod_ok:
        code[mod_ok - 2] = code_pos & 0xFF;
        code[mod_ok - 1] = (code_pos >> 8) & 0xFF;
        // Subtract DE until HL < DE
        mod_loop = code_pos;
        // If HL < DE, we're done
        emit(0xE5);          // PUSH HL
        emit(0xB7);          // OR A
        emit(0xED); emit(0x52);  // SBC HL,DE
        emit(0xE1);          // POP HL
        mod_done = code_pos + 3;
        emit(0xDA); emit16(0);  // JP C,mod_done (placeholder)
        // HL -= DE
        emit(0xB7);          // OR A
        emit(0xED); emit(0x52);  // SBC HL,DE
        emit_jp(mod_loop);   // JP mod_loop
        // mod_done: remainder is in HL
        code[mod_done - 2] = code_pos & 0xFF;
        code[mod_done - 1] = (code_pos >> 8) & 0xFF;
        emit(0xD1);          // POP DE
        emit(0xC1);          // POP BC
        emit_ret();
    }

    // __shl16: HL = HL << E (shift count in E)
    rt_shl16 = code_pos;
    {
        int shl_loop;
        shl_loop = code_pos;
        emit_ld_a_e();           // LD A,E
        emit(0xB7);              // OR A (check if E == 0)
        emit(0xC8);              // RET Z (return if shift count is zero)
        emit_add_hl_hl();        // ADD HL,HL (HL <<= 1)
        emit(0x1D);              // DEC E
        emit_jp(shl_loop);       // JP shl_loop
    }

    // __shr16: HL = HL >> E (shift count in E)
    rt_shr16 = code_pos;
    {
        int shr_loop;
        shr_loop = code_pos;
        emit_ld_a_e();           // LD A,E
        emit(0xB7);              // OR A (check if E == 0)
        emit(0xC8);              // RET Z (return if shift count is zero)
        emit(0xCB); emit(0x3C);  // SRL H (logical shift right H)
        emit(0xCB); emit(0x1D);  // RR L (rotate right L through carry)
        emit(0x1D);              // DEC E
        emit_jp(shr_loop);       // JP shr_loop
    }

    // Now patch the print_num forward references to div16 and mod16
    patch16(pn_div_patch, rt_div16);
    patch16(pn_mod_patch, rt_mod16);
}

// ============================================================
// Float BCD Conversion
// ============================================================

// Parse float literal string to 6-byte BCD format
// Returns address of 6-byte BCD data in code
int parse_float_to_bcd(char *s) {
    int result_addr;
    int sign;
    int exp;
    int digits[8];
    int num_digits;
    int decimal_pos;
    int i;
    int exp_sign;
    int exp_val;

    // Initialize
    sign = 0;
    decimal_pos = -1;
    num_digits = 0;
    i = 0;
    while (i < 8) {
        digits[i] = 0;
        i = i + 1;
    }

    // Handle sign
    if (*s == '-') {
        sign = 0x80;
        s = s + 1;
    } else if (*s == '+') {
        s = s + 1;
    }

    // Parse mantissa digits and find decimal point
    while (*s && *s != 'e' && *s != 'E') {
        if (*s == '.') {
            decimal_pos = num_digits;
        } else if (*s >= '0' && *s <= '9') {
            if (num_digits < 8) {
                digits[num_digits] = *s - '0';
            }
            num_digits = num_digits + 1;
        }
        s = s + 1;
    }

    // If no decimal point, it's at the end
    if (decimal_pos < 0) {
        decimal_pos = num_digits;
    }

    // Parse exponent if present
    exp_val = 0;
    exp_sign = 1;
    if (*s == 'e' || *s == 'E') {
        s = s + 1;
        if (*s == '-') {
            exp_sign = -1;
            s = s + 1;
        } else if (*s == '+') {
            s = s + 1;
        }
        while (*s >= '0' && *s <= '9') {
            exp_val = exp_val * 10 + *s - '0';
            s = s + 1;
        }
        exp_val = exp_val * exp_sign;
    }

    // Remove leading zeros
    while (num_digits > 0 && digits[0] == 0) {
        i = 0;
        while (i < 7) {
            digits[i] = digits[i + 1];
            i = i + 1;
        }
        digits[7] = 0;
        num_digits = num_digits - 1;
        decimal_pos = decimal_pos - 1;
    }

    // Calculate exponent (decimal_pos - 1 gives exponent for normalized form)
    if (num_digits == 0) {
        exp = 0x80;  // Zero
    } else {
        exp = decimal_pos - 1 + exp_val + 0x80;
    }

    // Emit 6-byte BCD at current code position
    result_addr = code_pos;
    emit(sign);
    emit(exp);
    emit((digits[0] << 4) | digits[1]);
    emit((digits[2] << 4) | digits[3]);
    emit((digits[4] << 4) | digits[5]);
    emit((digits[6] << 4) | digits[7]);

    return result_addr;
}

// ============================================================
// Expression Parser & Code Generator
// ============================================================

void gen_expr();  // Forward declaration
void gen_logical();  // Forward declaration

// Track expression result type
int expr_result_type;  // TYPE_INT, TYPE_FLOAT, etc.

void gen_primary() {
    int sym;
    int addr;
    int bcd_addr;
    int skip_addr;
    int func_arg_count;

    if (tok == TK_NUM || tok == TK_CHAR) {
        emit_ld_hl_nn(tok_val);
        expr_result_type = TYPE_INT;
        next();
        return;
    }

    if (tok == TK_FLOAT_LIT) {
        // Emit BCD constant data, then load address
        // Jump over constant data first
        emit(0xC3);  // JP
        skip_addr = code_pos;
        emit16(0);
        bcd_addr = parse_float_to_bcd(tok_str);
        patch16(skip_addr, code_pos);
        emit_ld_hl_nn(bcd_addr);
        expr_result_type = TYPE_FLOAT;
        next();
        return;
    }

    if (tok == TK_STR) {
        // Store string inline with jump over it
        int skip_addr;
        int str_addr;
        int i;
        emit(0xC3);  // JP
        skip_addr = code_pos;
        emit16(0);   // Placeholder for skip address
        str_addr = code_pos;
        // Copy string to code
        i = 0;
        while (tok_str[i]) {
            emit(tok_str[i]);
            i = i + 1;
        }
        emit(0);  // Null terminator
        // Patch jump to skip over string
        patch16(skip_addr, code_pos);
        // Load string address
        emit_ld_hl_nn(str_addr);
        next();
        return;
    }

    if (tok == TK_IDENT) {
        sym = find_sym(tok_str);
        if (sym < 0) {
            error("Undefined symbol");
        }
        next_with_lookahead();

        if (tok == TK_LPAREN) {
            // Function call
            next();
            func_arg_count = 0;
            if (tok != TK_RPAREN) {
                gen_expr();
                emit_push_hl();
                func_arg_count = func_arg_count + 1;
                while (tok == TK_COMMA) {
                    next();
                    gen_expr();
                    emit_push_hl();
                    func_arg_count = func_arg_count + 1;
                }
            }
            expect(TK_RPAREN);
            emit_call(sym_value[sym]);
            // Clean up stack, preserving return value
            if (func_arg_count > 0) {
                emit_ex_de_hl();
                emit_ld_hl_nn(func_arg_count * 2);
                emit_add_hl_sp();
                emit_ld_sp_hl();
                emit_ex_de_hl();
            }
            return;
        }

        // Check for array subscript
        if (tok == TK_LBRACKET) {
            // Array access: arr[index]
            // First, get address of array (base pointer)
            if (sym_type[sym] == 1 || sym_type[sym] == 2) {
                // Local/param array: compute IX + offset to get base address
                emit_push_ix();
                emit_pop_hl();
                emit_ld_de_nn(sym_value[sym]);
                emit_add_hl_de();
            } else {
                // Global array: load address
                emit_ld_hl_nn(sym_value[sym]);
            }
            emit_push_hl();  // Save base address
            next();  // skip '['
            gen_expr();  // index in HL
            // HL = index, multiply by element size (2 for int)
            emit_add_hl_hl();  // HL = index * 2
            emit_pop_de();  // DE = base address
            emit_add_hl_de();  // HL = base + index*2
            expect(TK_RBRACKET);
            // Load value at address
            emit_ld_a_hl();
            emit_inc_hl();
            emit_ld_h_hl();
            emit_ld_l_a();
            expr_result_type = TYPE_INT;
            return;
        }

        // Variable access
        expr_result_type = sym_dtype[sym];
        if (sym_dtype[sym] == TYPE_FLOAT) {
            // Float variable - return pointer to 6-byte BCD data
            if (sym_type[sym] == 1 || sym_type[sym] == 2) {
                // Local/param: compute IX + offset
                emit_push_ix();
                emit_pop_hl();
                emit_ld_de_nn(sym_value[sym]);
                emit_add_hl_de();
            } else {
                // Global: load address directly
                emit_ld_hl_nn(sym_value[sym]);
            }
        } else if (sym_type[sym] == 1) {
            // Local int/char
            emit_ld_l_ix_d(sym_value[sym]);
            emit_ld_h_ix_d(sym_value[sym] + 1);
        } else if (sym_type[sym] == 2) {
            // Parameter int/char
            emit_ld_l_ix_d(sym_value[sym]);
            emit_ld_h_ix_d(sym_value[sym] + 1);
        } else {
            // Global int/char
            emit(0x2A);  // LD HL,(nn)
            emit16(sym_value[sym]);
        }
        return;
    }

    if (tok == TK_LPAREN) {
        next();
        gen_expr();
        expect(TK_RPAREN);
        return;
    }

    error("Expected expression");
}

void gen_unary() {
    if (tok == TK_MINUS) {
        next();
        gen_unary();
        // Negate: HL = 0 - HL
        emit_ex_de_hl();
        emit_ld_hl_nn(0);
        emit_or_a();
        emit_sbc_hl_de();
        return;
    }
    if (tok == TK_STAR) {
        // Dereference
        next();
        gen_unary();
        emit_ld_a_hl();
        emit_ld_l_a();
        emit_ld_h_n(0);
        return;
    }
    if (tok == TK_AMP) {
        // Address-of (simplified - only works for simple variables)
        next();
        if (tok != TK_IDENT) {
            error("Expected identifier after &");
        }
        int sym;
        sym = find_sym(tok_str);
        if (sym < 0) {
            error("Undefined symbol");
        }
        next();
        if (sym_type[sym] == 1 || sym_type[sym] == 2) {
            // Local/param: compute IX + offset
            emit_push_ix();
            emit_pop_hl();
            emit_ld_de_nn(sym_value[sym]);
            emit_add_hl_de();
        } else {
            // Global: load address directly
            emit_ld_hl_nn(sym_value[sym]);
        }
        return;
    }
    gen_primary();
}

void gen_mul() {
    int op;
    int left_type;
    gen_unary();
    while (tok == TK_STAR || tok == TK_SLASH || tok == TK_PERCENT) {
        left_type = expr_result_type;
        op = tok;
        next();
        emit_push_hl();
        gen_unary();
        emit_pop_de();
        // DE op HL
        if (left_type == TYPE_FLOAT || expr_result_type == TYPE_FLOAT) {
            // Float operation: DE=left ptr, HL=right ptr
            // Need temp storage for result - use stack
            emit_ld_hl_nn(-6);
            emit_add_hl_sp();
            emit_ld_sp_hl();  // Allocate 6 bytes
            emit_push_de();   // Save left ptr
            emit_push_hl();   // HL = result location
            // Stack: result addr, left ptr
            // Call bcdf op with: HL=result, DE=left, next=right (on stack)
            if (op == TK_STAR) {
                emit_call(rt_bcdf_mul);
            } else if (op == TK_SLASH) {
                emit_call(rt_bcdf_div);
            }
            // Result location already in HL from call setup
            emit_pop_hl();    // Get result addr back
            emit_pop_de();    // Clean up left
            expr_result_type = TYPE_FLOAT;
        } else {
            if (op == TK_STAR) {
                emit_call(rt_mul16);
            } else if (op == TK_SLASH) {
                emit_call(rt_div16);
            } else {
                emit_call(rt_mod16);
            }
        }
    }
}

void gen_add() {
    int op;
    int left_type;
    gen_mul();
    while (tok == TK_PLUS || tok == TK_MINUS) {
        left_type = expr_result_type;
        op = tok;
        next();
        emit_push_hl();
        gen_mul();
        if (left_type == TYPE_FLOAT || expr_result_type == TYPE_FLOAT) {
            // Float operation: stack has left ptr, HL has right ptr
            emit_pop_de();  // DE = left ptr
            // Allocate result on stack
            emit_push_hl();  // Save right
            emit_ld_hl_nn(-6);
            emit_add_hl_sp();
            emit_ld_sp_hl();  // Allocate 6 bytes
            // Now HL = result location
            // Stack layout: right_ptr above result
            // Need: HL=result, DE=left, BC=right? No, use standard calling
            // Push result location, call bcdf_add(result, left, right)
            emit_push_hl();   // result location
            emit_push_de();   // left
            // right is 2 slots above
            if (op == TK_PLUS) {
                emit_call(rt_bcdf_add);
            } else {
                emit_call(rt_bcdf_sub);
            }
            emit_pop_de();   // Clean left
            emit_pop_hl();   // Get result addr (this is our return)
            emit_pop_de();   // Clean right
            expr_result_type = TYPE_FLOAT;
        } else {
            if (op == TK_PLUS) {
                emit_pop_de();
                emit_add_hl_de();
            } else {
                emit_pop_de();
                emit_ex_de_hl();
                emit_or_a();
                emit_sbc_hl_de();
            }
        }
    }
}

void gen_shift() {
    int op;
    gen_add();
    while (tok == TK_LTLT || tok == TK_GTGT) {
        op = tok;
        next();
        emit_push_hl();
        gen_add();
        emit_pop_de();
        // DE = left value, HL = shift count
        // Runtime expects: HL = value to shift, E = shift count
        emit(0xEB);          // EX DE,HL (now HL = value to shift, DE = old shift count)
        // After exchange: E already has low byte of shift count
        if (op == TK_LTLT) {
            emit_call(rt_shl16);
        } else {
            emit_call(rt_shr16);
        }
    }
}

void gen_compare() {
    int op;
    int label;
    int left_type;
    int patch_false;
    gen_shift();
    if (tok == TK_LT || tok == TK_GT || tok == TK_LTEQ || tok == TK_GTEQ ||
        tok == TK_EQEQ || tok == TK_BANGEQ) {
        left_type = expr_result_type;
        op = tok;
        next();
        emit_push_hl();
        gen_shift();
        emit_pop_de();

        if (left_type == TYPE_FLOAT || expr_result_type == TYPE_FLOAT) {
            // Float comparison: call bcdf_cmp(DE, HL) -> returns -1, 0, 1
            emit_push_de();  // Push left
            emit_push_hl();  // Push right
            emit_call(rt_bcdf_cmp);
            emit_pop_de();   // Clean stack
            emit_pop_de();
            // HL now has cmp result (-1, 0, 1)
            // Convert to boolean based on op
            emit_ld_de_nn(0);  // DE = 0 for comparison
            emit_or_a();
            emit_sbc_hl_de();  // HL = HL - 0, flags set on sign

            emit_ld_hl_nn(0);  // Assume false
            if (op == TK_LT) {
                // cmp < 0 means a < b -> result is -1 (0xFFFF)
                emit(0x30); emit(3);  // JR NC,+3 (if not negative, skip)
            } else if (op == TK_GT) {
                // cmp > 0 means a > b -> result is 1
                emit(0x38); emit(6);  // JR C,+6 (if negative, skip)
                emit(0xCA); emit16(code_pos + 6);  // JP Z,skip (if zero, skip)
            } else if (op == TK_LTEQ) {
                // cmp <= 0
                emit(0x38); emit(3);  // JR C,+3 (if negative, set true)
                emit(0xC2); emit16(code_pos + 6);  // JP NZ,skip (if pos, skip)
            } else if (op == TK_GTEQ) {
                // cmp >= 0
                emit(0x30); emit(3);  // JR NC,+3 (if not negative, set true)
            } else if (op == TK_EQEQ) {
                // cmp == 0
                emit(0xC2); emit16(code_pos + 6);  // JP NZ,skip
            } else {  // BANGEQ
                // cmp != 0
                emit(0xCA); emit16(code_pos + 6);  // JP Z,skip
            }
            emit_inc_hl();  // HL = 1 (true)
            expr_result_type = TYPE_INT;
        } else {
            // Integer comparison
            emit_ex_de_hl();
            emit_or_a();
            emit_sbc_hl_de();
            // Now: HL = DE - HL, flags set
            label = code_pos;
            emit_ld_hl_nn(0);  // Assume false
            if (op == TK_LT) {
                emit(0x30); emit(1);  // JR NC,+1 (skip INC HL)
            } else if (op == TK_GT) {
                // a > b: true when Z=0 AND C=0
                // If Z=1 (equal), skip to after INC (false)
                // If C=1 (a < b), skip INC (false)
                emit(0xCA); emit16(code_pos + 5);  // JP Z,after INC (code_pos+1, +2 more for addr, +2 for JR)
                emit(0x38); emit(1);  // JR C,+1 (skip INC HL)
            } else if (op == TK_LTEQ) {
                // a <= b: true when Z=1 OR C=1
                // If Z=1 (equal), jump to INC (true)
                // If C=0 (a > b), skip INC (false)
                emit(0xCA); emit16(code_pos + 4);  // JP Z,to INC (code_pos+1, need +3 to reach X+4, but emit16 uses X+1 so +4)
                emit(0x30); emit(1);  // JR NC,+1 (if a > b, skip INC = false)
            } else if (op == TK_GTEQ) {
                emit(0x38); emit(1);  // JR C,+1 (skip INC HL)
            } else if (op == TK_EQEQ) {
                emit(0xC2); emit16(code_pos + 3);  // JP NZ,skip INC (code_pos already +1)
            } else {  // BANGEQ
                emit(0xCA); emit16(code_pos + 3);  // JP Z,skip INC (code_pos already +1)
            }
            emit_inc_hl();  // HL = 1 (true)
        }
    }
}

// Lookahead support for assignment detection
int have_lookahead;
int lookahead_tok;
int lookahead_val;
char lookahead_str[64];

void push_lookahead() {
    have_lookahead = 1;
    lookahead_tok = tok;
    lookahead_val = tok_val;
    strcpy(lookahead_str, tok_str);
}

void next_with_lookahead() {
    if (have_lookahead) {
        have_lookahead = 0;
        tok = lookahead_tok;
        tok_val = lookahead_val;
        strcpy(tok_str, lookahead_str);
    } else {
        next();
    }
}

void gen_expr() {
    int op;
    int sym;

    // Check for assignment (including array assignment)
    if (tok == TK_IDENT) {
        char name[64];
        strcpy(name, tok_str);
        next();

        // Check for array subscript assignment: arr[i] = value
        if (tok == TK_LBRACKET) {
            sym = find_sym(name);
            if (sym < 0) {
                error("Undefined variable");
            }
            // Get base address of array
            if (sym_type[sym] == 1 || sym_type[sym] == 2) {
                emit_push_ix();
                emit_pop_hl();
                emit_ld_de_nn(sym_value[sym]);
                emit_add_hl_de();
            } else {
                emit_ld_hl_nn(sym_value[sym]);
            }
            emit_push_hl();  // Save base address
            next();  // skip '['
            gen_expr();  // index in HL
            emit_add_hl_hl();  // HL = index * 2
            emit_pop_de();  // DE = base
            emit_add_hl_de();  // HL = address of element
            expect(TK_RBRACKET);

            if (tok == TK_EQ) {
                // Array assignment: arr[i] = value
                emit_push_hl();  // Save element address
                next();
                gen_expr();  // value in HL
                emit_pop_de();  // DE = address
                // Store HL to (DE), low byte first
                emit_ld_a_l();
                emit(0x12);  // LD (DE),A
                emit_inc_de();
                emit_ld_a_h();
                emit(0x12);  // LD (DE),A
                return;
            }
            // Array read in expression context: arr[i] op ...
            // HL has address, load value
            emit_ld_a_hl();
            emit_inc_hl();
            emit_ld_h_hl();
            emit_ld_l_a();
            // Value is now in HL. If there's a following operator, it needs to be
            // handled. But we can't call gen_logical since it would re-parse a value.
            // For now, just return - this handles simple cases like putchar(arr[0])
            return;
        }

        if (tok == TK_EQ) {
            sym = find_sym(name);
            if (sym < 0) {
                error("Undefined variable");
            }
            next();
            gen_expr();
            // Store to variable
            if (sym_type[sym] == 1 || sym_type[sym] == 2) {
                emit_ld_ix_d_l(sym_value[sym]);
                emit_ld_ix_d_h(sym_value[sym] + 1);
            } else {
                emit(0x22);  // LD (nn),HL
                emit16(sym_value[sym]);
            }
            return;
        }
        // Not assignment - save the token we read as lookahead
        push_lookahead();
        // Restore identifier
        tok = TK_IDENT;
        strcpy(tok_str, name);
    }

    gen_logical();
}

void gen_logical() {
    int op;
    int patch;

    gen_compare();

    while (tok == TK_AMPAMP || tok == TK_PIPEPIPE) {
        op = tok;
        emit_push_hl();
        next();
        gen_compare();
        emit_pop_de();

        if (op == TK_AMPAMP) {
            // AND: result is true only if both are non-zero
            // DE=first, HL=second. Test DE first.
            emit_ld_a_d();
            emit_or_e();
            emit(0xCA); emit16(code_pos + 10);  // JP Z, to set 0 (first is zero)
            // First was non-zero, now test second (HL)
            emit_ld_a_h();
            emit_or_l();
            emit(0xCA); emit16(code_pos + 6);  // JP Z, to set 0
            // Both non-zero, set result to 1
            emit_ld_hl_nn(1);
            emit(0xC3); emit16(code_pos + 6);  // JP over the 0 case
            emit_ld_hl_nn(0);
        } else {
            // OR: result is true if either is non-zero
            // DE=first, HL=second. Test DE first.
            emit_ld_a_d();
            emit_or_e();
            emit(0xC2); emit16(code_pos + 10);  // JP NZ, to set 1 (first non-zero)
            // First was zero, test second (HL)
            emit_ld_a_h();
            emit_or_l();
            emit(0xC2); emit16(code_pos + 6);  // JP NZ, to set 1
            // Both zero, set result to 0
            emit_ld_hl_nn(0);
            emit(0xC3); emit16(code_pos + 6);  // JP over the 1 case
            emit_ld_hl_nn(1);
        }
    }
}

// ============================================================
// Statement Parser
// ============================================================

void gen_stmt();  // Forward declaration

void gen_block() {
    expect(TK_LBRACE);
    while (tok != TK_RBRACE && tok != TK_EOF) {
        gen_stmt();
    }
    expect(TK_RBRACE);
}

void gen_stmt() {
    int label1;
    int label2;
    int patch1;
    int patch2;

    // Variable declaration
    if (tok == TK_INT || tok == TK_CHAR_KW || tok == TK_FLOAT) {
        int var_type;
        int var_size;
        int elem_size;
        int arr_count;
        int sym_idx;
        int var_offset;

        if (tok == TK_FLOAT) {
            var_type = TYPE_FLOAT;
            elem_size = 6;
        } else {
            var_type = TYPE_INT;
            elem_size = 2;
        }
        next();

        // Check for pointer declaration: int *name
        if (tok == TK_STAR) {
            var_type = TYPE_PTR;
            elem_size = 2;  // Pointers are 16-bit
            next();
        }

        if (tok != TK_IDENT) {
            error("Expected identifier");
        }

        // Save name and advance
        {
            char var_name[64];
            int j;
            j = 0;
            while (tok_str[j] != 0 && j < 63) {
                var_name[j] = tok_str[j];
                j = j + 1;
            }
            var_name[j] = 0;
            next();

            // Check for array declaration
            arr_count = 0;
            if (tok == TK_LBRACKET) {
                next();
                if (tok != TK_NUM) {
                    error("Expected array size");
                }
                arr_count = tok_val;
                next();
                expect(TK_RBRACKET);
                var_size = elem_size * arr_count;
                var_type = TYPE_PTR;  // Arrays are stored as pointers
            } else {
                var_size = elem_size;
            }

            // Allocate local variable
            local_offset = local_offset - var_size;
            sym_idx = add_sym_typed(var_name, 1, local_offset, var_type);
            var_offset = local_offset;

            // Adjust stack
            emit_ld_hl_nn(0 - var_size);
            emit_add_hl_sp();
            emit_ld_sp_hl();

            // For arrays, we need to store the address of the array in the symbol
            // Actually, arrays are inline on the stack, so sym_value points to start
            // No extra setup needed - array access computes: &arr + index*elem_size

            if (arr_count == 0 && tok == TK_EQ) {
                next();
                gen_expr();
                if (var_type == TYPE_FLOAT) {
                    // Float assignment - copy 6 bytes from HL to var
                    // HL = source address, need to copy to var location
                    emit_ex_de_hl();  // DE = source
                    // Calculate dest: IX + offset
                    emit_push_ix();
                    emit_pop_hl();
                    emit_ld_bc_nn(var_offset);
                    emit(0x09);  // ADD HL,BC
                    emit_ex_de_hl();  // HL = source, DE = dest
                    emit_ld_bc_nn(6);
                    emit_ldir();
                } else {
                    emit_ld_ix_d_l(var_offset);
                    emit_ld_ix_d_h(var_offset + 1);
                }
            }
        }
        expect(TK_SEMICOLON);
        return;
    }

    // If statement
    if (tok == TK_IF) {
        next();
        expect(TK_LPAREN);
        gen_expr();
        expect(TK_RPAREN);
        emit_ld_a_h();
        emit_or_l();
        patch1 = code_pos;
        emit_jp_z(0);  // Patch later
        gen_stmt();
        if (tok == TK_ELSE) {
            next();
            patch2 = code_pos;
            emit_jp(0);  // Patch later
            patch16(patch1 + 1, code_pos);
            gen_stmt();
            patch16(patch2 + 1, code_pos);
        } else {
            patch16(patch1 + 1, code_pos);
        }
        return;
    }

    // While statement
    if (tok == TK_WHILE) {
        next();
        label1 = code_pos;
        expect(TK_LPAREN);
        gen_expr();
        expect(TK_RPAREN);
        emit_ld_a_h();
        emit_or_l();
        patch1 = code_pos;
        emit_jp_z(0);
        gen_stmt();
        emit_jp(label1);
        patch16(patch1 + 1, code_pos);
        return;
    }

    // For statement: for (init; cond; update) body
    if (tok == TK_FOR) {
        next();
        expect(TK_LPAREN);
        // Init expression (may be empty)
        if (tok != TK_SEMICOLON) {
            gen_expr();
        }
        expect(TK_SEMICOLON);
        // Condition
        label1 = code_pos;  // Loop start
        if (tok != TK_SEMICOLON) {
            gen_expr();
            emit_ld_a_h();
            emit_or_l();
            patch1 = code_pos;
            emit_jp_z(0);  // Exit if false
        } else {
            patch1 = 0;  // No condition = infinite loop
        }
        expect(TK_SEMICOLON);
        // Update - save for later, need to emit after body
        // For simplicity, generate: jp over update, then body, then update, then jp back
        patch2 = code_pos;
        emit_jp(0);  // Jump over update to body
        label2 = code_pos;  // Update start
        if (tok != TK_RPAREN) {
            gen_expr();
        }
        emit_jp(label1);  // After update, go back to condition
        patch16(patch2 + 1, code_pos);  // Patch jump to here (body start)
        expect(TK_RPAREN);
        gen_stmt();  // Body
        emit_jp(label2);  // After body, go to update
        if (patch1 != 0) {
            patch16(patch1 + 1, code_pos);  // Patch exit jump
        }
        return;
    }

    // Return statement
    if (tok == TK_RETURN) {
        next();
        if (tok != TK_SEMICOLON) {
            gen_expr();
        }
        expect(TK_SEMICOLON);
        emit_ld_sp_ix();
        emit_pop_ix();
        emit_ret();
        return;
    }

    // Block
    if (tok == TK_LBRACE) {
        gen_block();
        return;
    }

    // Expression statement
    gen_expr();
    expect(TK_SEMICOLON);
}

// ============================================================
// Top-level Declaration Parser
// ============================================================

// Saved identifier name for function parsing after type/name parsed
char toplevel_name[64];

// Parse a top-level declaration (either global variable or function)
void gen_toplevel() {
    int var_type;
    int var_size;
    int arr_count;
    int elem_size;
    int i;
    int func_addr;
    int param_offset;
    int saved_count;
    int param_type;
    int param_size;

    // Parse type
    if (tok == TK_INT) {
        var_type = TYPE_INT;
        elem_size = 2;
    } else if (tok == TK_CHAR_KW) {
        var_type = TYPE_CHAR;
        elem_size = 1;
    } else if (tok == TK_VOID) {
        var_type = TYPE_INT;
        elem_size = 2;
    } else if (tok == TK_FLOAT) {
        var_type = TYPE_FLOAT;
        elem_size = 6;
    } else {
        puts("tok=");
        print_num(tok);
        error(" Expected type");
    }
    next();

    // Check for pointer type: int *name
    if (tok == TK_STAR) {
        var_type = TYPE_PTR;
        elem_size = 2;
        next();
    }

    // Parse name
    if (tok != TK_IDENT) {
        error("Expected identifier");
    }
    i = 0;
    while (tok_str[i] != 0) {
        toplevel_name[i] = tok_str[i];
        i = i + 1;
    }
    toplevel_name[i] = 0;
    next();

    // Check if this is a function (has '(') or a global variable
    if (tok == TK_LPAREN) {
        // This is a function definition
        func_addr = code_pos;
        add_sym(toplevel_name, 3, func_addr);

        // Parameters
        next();  // consume '('
        saved_count = sym_count;
        param_offset = 4;  // First param at IX+4
        if (tok != TK_RPAREN) {
            if (tok == TK_FLOAT) {
                param_type = TYPE_FLOAT;
                param_size = 6;
            } else if (tok == TK_INT || tok == TK_CHAR_KW) {
                param_type = TYPE_INT;
                param_size = 2;
            } else {
                error("Expected type");
            }
            next();
            if (tok != TK_IDENT) {
                error("Expected parameter name");
            }
            add_sym_typed(tok_str, 2, param_offset, param_type);
            next();
            param_offset = param_offset + param_size;
            while (tok == TK_COMMA) {
                next();
                if (tok == TK_FLOAT) {
                    param_type = TYPE_FLOAT;
                    param_size = 6;
                } else if (tok == TK_INT || tok == TK_CHAR_KW) {
                    param_type = TYPE_INT;
                    param_size = 2;
                } else {
                    error("Expected type");
                }
                next();
                if (tok != TK_IDENT) {
                    error("Expected parameter name");
                }
                add_sym_typed(tok_str, 2, param_offset, param_type);
                next();
                param_offset = param_offset + param_size;
            }
        }
        expect(TK_RPAREN);

        // Check for prototype (declaration without body)
        if (tok == TK_SEMICOLON) {
            next();
            sym_count = saved_count;  // Remove any param symbols
            return;
        }

        // Prologue
        emit_push_ix();
        emit(0xDD); emit(0x21); emit16(0);  // LD IX,0
        emit(0xDD); emit(0x39);  // ADD IX,SP
        local_offset = 0;

        // Body
        gen_block();

        // Epilogue (if not already returned)
        emit_ld_sp_ix();
        emit_pop_ix();
        emit_ret();

        // Remove local symbols
        sym_count = saved_count;
        return;
    }

    // This is a global variable declaration
    var_size = elem_size;

    // Check for array: int name[size]
    if (tok == TK_LBRACKET) {
        next();
        if (tok != TK_NUM) {
            error("Expected array size");
        }
        arr_count = tok_val;
        next();
        expect(TK_RBRACKET);
        var_size = elem_size * arr_count;
        var_type = TYPE_PTR;  // Arrays decay to pointers
    }

    // Add symbol with type 0 (global) and address in RAM
    add_sym_typed(toplevel_name, 0, global_pos, var_type);

    // Allocate space for the global
    global_pos = global_pos + var_size;

    // Check for initializer
    if (tok == TK_EQ) {
        // TODO: Support global initializers later
        error("Global initializers not yet supported");
    }

    expect(TK_SEMICOLON);
}

// ============================================================
// Legacy Function Parser (kept for compatibility)
// ============================================================

void gen_function() {
    int func_addr;
    int param_offset;
    int saved_count;
    int param_type;
    int param_size;

    // Return type
    if (tok != TK_INT && tok != TK_CHAR_KW && tok != TK_VOID && tok != TK_FLOAT) {
        puts("tok=");
        print_num(tok);
        error(" Expected type");
    }
    next();

    // Function name
    if (tok != TK_IDENT) {
        error("Expected function name");
    }
    func_addr = code_pos;
    add_sym(tok_str, 3, func_addr);
    next();

    // Parameters
    expect(TK_LPAREN);
    saved_count = sym_count;
    param_offset = 4;  // First param at IX+4
    if (tok != TK_RPAREN) {
        if (tok == TK_FLOAT) {
            param_type = TYPE_FLOAT;
            param_size = 6;
        } else if (tok == TK_INT || tok == TK_CHAR_KW) {
            param_type = TYPE_INT;
            param_size = 2;
        } else {
            error("Expected type");
        }
        next();
        if (tok != TK_IDENT) {
            error("Expected parameter name");
        }
        add_sym_typed(tok_str, 2, param_offset, param_type);
        next();
        param_offset = param_offset + param_size;
        while (tok == TK_COMMA) {
            next();
            if (tok == TK_FLOAT) {
                param_type = TYPE_FLOAT;
                param_size = 6;
            } else if (tok == TK_INT || tok == TK_CHAR_KW) {
                param_type = TYPE_INT;
                param_size = 2;
            } else {
                error("Expected type");
            }
            next();
            if (tok != TK_IDENT) {
                error("Expected parameter name");
            }
            add_sym_typed(tok_str, 2, param_offset, param_type);
            next();
            param_offset = param_offset + param_size;
        }
    }
    expect(TK_RPAREN);

    // Check for prototype (declaration without body)
    if (tok == TK_SEMICOLON) {
        // This is a function prototype - just register it with placeholder address
        // The symbol was already added above - update its address to 0 (undefined)
        // Actual implementation will be provided by runtime or linker
        next();
        sym_count = saved_count;  // Remove any param symbols
        return;
    }

    // Prologue
    emit_push_ix();
    emit(0xDD); emit(0x21); emit16(0);  // LD IX,0
    emit(0xDD); emit(0x39);  // ADD IX,SP
    local_offset = 0;

    // Body
    gen_block();

    // Epilogue (if not already returned)
    emit_ld_sp_ix();
    emit_pop_ix();
    emit_ret();

    // Remove local symbols
    sym_count = saved_count;
}

// ============================================================
// Main Compiler
// ============================================================

void compile(char *source) {
    src = source;
    line = 1;
    code_pos = 0;
    sym_count = 0;
    str_pos = 0;
    label_count = 0;
    have_lookahead = 0;
    global_pos = 0x2000;  // RAM starts at 0x2000 on RetroShield Z80
    def_count = 0;        // Reset preprocessor defines

    // Leave space for startup code (CALL main + HALT = 4 bytes)
    code_pos = 4;

    // Emit runtime library functions
    emit_runtime();

    next();
    while (tok != TK_EOF) {
        gen_toplevel();
    }

    // Emit startup code at beginning
    int main_addr;
    main_addr = find_sym("main");
    if (main_addr < 0) {
        error("No main function");
    }
    main_addr = sym_value[main_addr];

    int saved_pos;
    saved_pos = code_pos;
    code_pos = 0;
    emit_call(main_addr);
    emit_halt();
    code_pos = saved_pos;

    // Copy strings to end of code
    int i;
    i = 0;
    while (i < str_pos) {
        code[code_pos] = strings[i];
        code_pos = code_pos + 1;
        i = i + 1;
    }
}

// ============================================================
// Entry Point
// ============================================================

// Source buffer for reading from stdin
char source[8192];

int main() {
    int i;
    int c;

    // Read source from stdin until null byte or Ctrl-D (EOF returns 0 from ACIA)
    // Use Ctrl-Z (0x1A) as end-of-file marker for compatibility
    i = 0;
    while (i < 8191) {
        c = getchar();
        if (c == 0 || c == 0x1A || c == 0x04) {
            break;  // EOF marker (null, Ctrl-Z, or Ctrl-D)
        }
        source[i] = c;
        i = i + 1;
    }
    source[i] = 0;

    // Compile the source
    compile(source);

    // Output the binary ROM to stdout
    i = 0;
    while (i < code_pos) {
        putchar(code[i]);
        i = i + 1;
    }

    return 0;
}
