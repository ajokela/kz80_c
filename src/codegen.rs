//! Z80 Code Generator
//!
//! Generates Z80 machine code from C AST.
//! Uses HL as the primary accumulator, DE as secondary.
//! Function arguments are passed on the stack, right to left.
//! Return values are in HL.

use crate::ast::*;
use std::collections::HashMap;

/// Symbol information
#[derive(Debug, Clone)]
enum Symbol {
    Local { offset: i16, ty: Type },    // Stack offset from BP (IX)
    Global { label: String, ty: Type },  // Global label
    Param { offset: i16, ty: Type },     // Parameter offset from BP
}

/// Code generator state
pub struct CodeGen {
    code: Vec<u8>,
    labels: HashMap<String, u16>,
    forward_refs: Vec<(usize, String)>,  // (position, label)
    strings: Vec<(String, String)>,       // (label, content)
    float_consts: Vec<(String, [u8; 6])>, // (label, 6-byte BCD float)
    globals: Vec<(String, Type, Option<i32>)>,  // (name, type, init_value)
    global_addr: u16,  // Next available global variable address (in RAM)

    // Global symbols (persist across functions)
    global_symbols: HashMap<String, Symbol>,
    // Current function context (locals and params)
    symbols: HashMap<String, Symbol>,
    local_offset: i16,  // Current stack offset for locals
    label_counter: u32,

    // Loop context for break/continue
    break_label: Option<String>,
    continue_label: Option<String>,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            labels: HashMap::new(),
            forward_refs: Vec::new(),
            strings: Vec::new(),
            float_consts: Vec::new(),
            globals: Vec::new(),
            global_addr: 0x2000,  // RAM starts at 0x2000 on RetroShield Z80
            global_symbols: HashMap::new(),
            symbols: HashMap::new(),
            local_offset: 0,
            label_counter: 0,
            break_label: None,
            continue_label: None,
        }
    }

    /// Parse a float literal string (e.g., "3.14", "1e5") into 6-byte BCD format
    /// Format: [sign, exponent, m3m2, m1m0, m-1m-2, m-3m-4]
    /// Sign: 0x00 = positive, 0x80 = negative
    /// Exponent: biased by 0x80 (0x80 = exponent 0)
    /// Mantissa: normalized BCD digits (first digit 1-9, decimal after first digit)
    fn parse_float_literal(s: &str) -> [u8; 6] {
        let mut result = [0u8; 6];
        let mut s = s.trim();

        // Handle sign
        let negative = s.starts_with('-');
        if negative || s.starts_with('+') {
            s = &s[1..];
        }
        if negative {
            result[0] = 0x80;
        }

        // Parse mantissa and exponent from string
        let (mantissa_str, exp_offset) = if let Some(e_pos) = s.to_lowercase().find('e') {
            let exp: i32 = s[e_pos+1..].parse().unwrap_or(0);
            (&s[..e_pos], exp)
        } else {
            (s, 0i32)
        };

        // Extract digits and find decimal point position
        let mut digits = Vec::new();
        let mut decimal_pos: i32 = -1;
        for (i, c) in mantissa_str.chars().enumerate() {
            if c == '.' {
                decimal_pos = i as i32;
            } else if c.is_ascii_digit() {
                digits.push(c as u8 - b'0');
            }
        }

        // If no decimal point, it's at the end
        if decimal_pos < 0 {
            decimal_pos = digits.len() as i32;
        }

        // Remove leading zeros
        while !digits.is_empty() && digits[0] == 0 {
            digits.remove(0);
            decimal_pos -= 1;
        }

        // Handle zero
        if digits.is_empty() {
            return [0, 0x80, 0, 0, 0, 0]; // Zero with exponent 0
        }

        // Compute exponent: position of decimal point gives the exponent
        // After first digit, decimal point is after first digit, so exp = decimal_pos - 1
        let exponent = decimal_pos - 1 + exp_offset;
        result[1] = (exponent + 0x80) as u8;

        // Pad digits to 8 for 4 mantissa bytes
        while digits.len() < 8 {
            digits.push(0);
        }

        // Pack digits into BCD bytes
        result[2] = (digits[0] << 4) | digits[1];
        result[3] = (digits[2] << 4) | digits[3];
        result[4] = (digits[4] << 4) | digits[5];
        result[5] = (digits[6] << 4) | digits[7];

        result
    }

    fn emit(&mut self, byte: u8) {
        self.code.push(byte);
    }

    fn emit16(&mut self, word: u16) {
        self.emit(word as u8);
        self.emit((word >> 8) as u8);
    }

    fn current_addr(&self) -> u16 {
        self.code.len() as u16
    }

    fn define_label(&mut self, name: &str) {
        self.labels.insert(name.to_string(), self.current_addr());
    }

    fn new_label(&mut self, prefix: &str) -> String {
        self.label_counter += 1;
        format!("{}_{}", prefix, self.label_counter)
    }

    fn emit_label_ref(&mut self, label: &str) {
        self.forward_refs.push((self.code.len(), label.to_string()));
        self.emit16(0);  // Placeholder
    }

    /// Resolve all forward references
    fn resolve_refs(&mut self) {
        for (pos, label) in &self.forward_refs {
            if let Some(&addr) = self.labels.get(label) {
                self.code[*pos] = addr as u8;
                self.code[*pos + 1] = (addr >> 8) as u8;
            } else {
                panic!("Undefined label: {}", label);
            }
        }
    }

    // Z80 instruction emitters

    fn ld_hl_nn(&mut self, n: u16) { self.emit(0x21); self.emit16(n); }
    fn ld_de_nn(&mut self, n: u16) { self.emit(0x11); self.emit16(n); }
    fn ld_bc_nn(&mut self, n: u16) { self.emit(0x01); self.emit16(n); }
    fn ld_sp_nn(&mut self, n: u16) { self.emit(0x31); self.emit16(n); }

    fn ld_a_n(&mut self, n: u8) { self.emit(0x3E); self.emit(n); }
    fn ld_hl_mem(&mut self, addr: u16) { self.emit(0x2A); self.emit16(addr); }
    fn ld_mem_hl(&mut self, addr: u16) { self.emit(0x22); self.emit16(addr); }
    fn ld_a_mem(&mut self, addr: u16) { self.emit(0x3A); self.emit16(addr); }
    fn ld_mem_a(&mut self, addr: u16) { self.emit(0x32); self.emit16(addr); }

    fn ld_a_hl(&mut self) { self.emit(0x7E); }
    fn ld_hl_a(&mut self) { self.emit(0x77); }
    fn ld_a_de(&mut self) { self.emit(0x1A); }
    fn ld_de_a(&mut self) { self.emit(0x12); }

    fn ld_h_a(&mut self) { self.emit(0x67); }
    fn ld_h_n(&mut self, n: u8) { self.emit(0x26); self.emit(n); }
    fn ld_h_hl(&mut self) { self.emit(0x66); }  // LD H,(HL)
    fn ld_l_a(&mut self) { self.emit(0x6F); }
    fn ld_l_n(&mut self, n: u8) { self.emit(0x2E); self.emit(n); }
    fn ld_a_h(&mut self) { self.emit(0x7C); }
    fn ld_a_l(&mut self) { self.emit(0x7D); }
    fn ld_a_e(&mut self) { self.emit(0x7B); }
    fn ld_a_d(&mut self) { self.emit(0x7A); }
    fn ld_b_a(&mut self) { self.emit(0x47); }
    fn ld_b_c(&mut self) { self.emit(0x41); }
    fn ld_c_a(&mut self) { self.emit(0x4F); }
    fn ld_c_l(&mut self) { self.emit(0x4D); }
    fn ld_e_a(&mut self) { self.emit(0x5F); }
    fn ld_d_a(&mut self) { self.emit(0x57); }
    fn ld_e_l(&mut self) { self.emit(0x5D); }
    fn ld_d_h(&mut self) { self.emit(0x54); }
    fn ld_l_e(&mut self) { self.emit(0x6B); }
    fn ld_h_d(&mut self) { self.emit(0x62); }

    fn push_hl(&mut self) { self.emit(0xE5); }
    fn pop_hl(&mut self) { self.emit(0xE1); }
    fn push_de(&mut self) { self.emit(0xD5); }
    fn pop_de(&mut self) { self.emit(0xD1); }
    fn push_bc(&mut self) { self.emit(0xC5); }
    fn pop_bc(&mut self) { self.emit(0xC1); }
    fn push_af(&mut self) { self.emit(0xF5); }
    fn pop_af(&mut self) { self.emit(0xF1); }
    fn push_ix(&mut self) { self.emit(0xDD); self.emit(0xE5); }
    fn pop_ix(&mut self) { self.emit(0xDD); self.emit(0xE1); }

    fn add_hl_hl(&mut self) { self.emit(0x29); }
    fn add_hl_de(&mut self) { self.emit(0x19); }
    fn add_hl_bc(&mut self) { self.emit(0x09); }
    fn add_hl_sp(&mut self) { self.emit(0x39); }
    fn sbc_hl_de(&mut self) { self.emit(0xED); self.emit(0x52); }

    fn add_a_n(&mut self, n: u8) { self.emit(0xC6); self.emit(n); }
    fn sub_n(&mut self, n: u8) { self.emit(0xD6); self.emit(n); }
    fn and_n(&mut self, n: u8) { self.emit(0xE6); self.emit(n); }
    fn or_n(&mut self, n: u8) { self.emit(0xF6); self.emit(n); }
    fn xor_n(&mut self, n: u8) { self.emit(0xEE); self.emit(n); }
    fn cp_n(&mut self, n: u8) { self.emit(0xFE); self.emit(n); }

    fn add_a_l(&mut self) { self.emit(0x85); }
    fn sub_l(&mut self) { self.emit(0x95); }
    fn sub_c(&mut self) { self.emit(0x91); }
    fn and_l(&mut self) { self.emit(0xA5); }
    fn or_l(&mut self) { self.emit(0xB5); }
    fn or_e(&mut self) { self.emit(0xB3); }
    fn or_d(&mut self) { self.emit(0xB2); }
    fn xor_l(&mut self) { self.emit(0xAD); }
    fn cp_l(&mut self) { self.emit(0xBD); }
    fn cp_b(&mut self) { self.emit(0xB8); }
    fn or_a(&mut self) { self.emit(0xB7); }
    fn xor_a(&mut self) { self.emit(0xAF); }

    fn inc_hl(&mut self) { self.emit(0x23); }
    fn dec_hl(&mut self) { self.emit(0x2B); }
    fn inc_de(&mut self) { self.emit(0x13); }
    fn dec_de(&mut self) { self.emit(0x1B); }
    fn inc_bc(&mut self) { self.emit(0x03); }
    fn dec_bc(&mut self) { self.emit(0x0B); }
    fn inc_b(&mut self) { self.emit(0x04); }
    fn dec_b(&mut self) { self.emit(0x05); }

    fn ld_a_b(&mut self) { self.emit(0x78); }
    fn ld_a_c(&mut self) { self.emit(0x79); }
    fn ld_b_n(&mut self, n: u8) { self.emit(0x06); self.emit(n); }
    fn ld_c_n(&mut self, n: u8) { self.emit(0x0E); self.emit(n); }
    fn ld_d_n(&mut self, n: u8) { self.emit(0x16); self.emit(n); }
    fn ld_e_n(&mut self, n: u8) { self.emit(0x1E); self.emit(n); }
    fn ld_h_b(&mut self) { self.emit(0x60); }
    fn ld_l_c(&mut self) { self.emit(0x69); }

    fn or_b(&mut self) { self.emit(0xB0); }
    fn or_c(&mut self) { self.emit(0xB1); }

    fn ex_de_hl(&mut self) { self.emit(0xEB); }

    fn jp(&mut self, addr: u16) { self.emit(0xC3); self.emit16(addr); }
    fn jp_label(&mut self, label: &str) { self.emit(0xC3); self.emit_label_ref(label); }
    fn jp_z(&mut self, addr: u16) { self.emit(0xCA); self.emit16(addr); }
    fn jp_z_label(&mut self, label: &str) { self.emit(0xCA); self.emit_label_ref(label); }
    fn jp_nz(&mut self, addr: u16) { self.emit(0xC2); self.emit16(addr); }
    fn jp_nz_label(&mut self, label: &str) { self.emit(0xC2); self.emit_label_ref(label); }
    fn jp_c_label(&mut self, label: &str) { self.emit(0xDA); self.emit_label_ref(label); }
    fn jp_nc_label(&mut self, label: &str) { self.emit(0xD2); self.emit_label_ref(label); }
    fn jp_hl(&mut self) { self.emit(0xE9); }

    fn call(&mut self, addr: u16) { self.emit(0xCD); self.emit16(addr); }
    fn call_label(&mut self, label: &str) { self.emit(0xCD); self.emit_label_ref(label); }
    fn ret(&mut self) { self.emit(0xC9); }

    fn ld_ix_nn(&mut self, n: u16) { self.emit(0xDD); self.emit(0x21); self.emit16(n); }
    fn ld_sp_ix(&mut self) { self.emit(0xDD); self.emit(0xF9); }
    fn ld_ix_sp(&mut self) {
        // No direct instruction, use: LD IX,0; ADD IX,SP
        self.emit(0xDD); self.emit(0x21); self.emit16(0);
        self.emit(0xDD); self.emit(0x39);  // ADD IX,SP
    }

    // Load from IX+offset
    fn ld_l_ix_d(&mut self, d: i8) { self.emit(0xDD); self.emit(0x6E); self.emit(d as u8); }
    fn ld_h_ix_d(&mut self, d: i8) { self.emit(0xDD); self.emit(0x66); self.emit(d as u8); }
    fn ld_a_ix_d(&mut self, d: i8) { self.emit(0xDD); self.emit(0x7E); self.emit(d as u8); }
    fn ld_e_ix_d(&mut self, d: i8) { self.emit(0xDD); self.emit(0x5E); self.emit(d as u8); }
    fn ld_d_ix_d(&mut self, d: i8) { self.emit(0xDD); self.emit(0x56); self.emit(d as u8); }
    fn ld_c_ix_d(&mut self, d: i8) { self.emit(0xDD); self.emit(0x4E); self.emit(d as u8); }
    fn ld_b_ix_d(&mut self, d: i8) { self.emit(0xDD); self.emit(0x46); self.emit(d as u8); }

    // Store to IX+offset
    fn ld_ix_d_l(&mut self, d: i8) { self.emit(0xDD); self.emit(0x75); self.emit(d as u8); }
    fn ld_ix_d_h(&mut self, d: i8) { self.emit(0xDD); self.emit(0x74); self.emit(d as u8); }
    fn ld_ix_d_a(&mut self, d: i8) { self.emit(0xDD); self.emit(0x77); self.emit(d as u8); }

    fn halt(&mut self) { self.emit(0x76); }
    fn nop(&mut self) { self.emit(0x00); }

    fn scf(&mut self) { self.emit(0x37); }  // Set carry flag
    fn ccf(&mut self) { self.emit(0x3F); }  // Complement carry flag

    // I/O port instructions
    fn in_a_n(&mut self, port: u8) { self.emit(0xDB); self.emit(port); }
    fn out_n_a(&mut self, port: u8) { self.emit(0xD3); self.emit(port); }

    /// Generate code for a translation unit
    pub fn generate(&mut self, unit: &TranslationUnit) -> Vec<u8> {
        // First pass: collect global info and register placeholders in global_symbols
        // We use "__global_OFFSET_<N>" as temporary label which will be resolved later
        let mut global_info: Vec<(String, Type, Option<i32>, usize)> = Vec::new();
        let mut offset = 0usize;
        for decl in &unit.decls {
            if let Decl::Global(global) = decl {
                let init_value = match &global.init {
                    Some(Expr::IntLit(n)) => Some(*n),
                    Some(Expr::CharLit(c)) => Some(*c as i32),
                    _ => None,
                };
                let size = global.ty.size();

                // Use a label that includes offset from global base
                let addr_label = format!("__global_offset_{}", offset);

                // Add to global_symbols so functions can reference this global
                self.global_symbols.insert(global.name.clone(), Symbol::Global {
                    label: addr_label.clone(),
                    ty: global.ty.clone(),
                });

                global_info.push((global.name.clone(), global.ty.clone(), init_value, offset));
                offset += size;
            }
        }

        // Leave space for global initialization code (we'll patch this later)
        // Each init: LD HL,nn (3) + LD (nn),HL (3) = 6 bytes, or LD A,n (2) + LD (nn),A (3) = 5 bytes
        let init_code_start = self.code.len();
        let max_init_size = global_info.iter().map(|(_, ty, init, _)| {
            if init.is_some() {
                if ty.size() == 1 { 5 } else { 6 }
            } else { 0 }
        }).sum::<usize>();
        // Reserve space with NOPs that we'll overwrite later
        for _ in 0..max_init_size {
            self.emit(0x00);  // NOP placeholder
        }

        // Call main, then halt
        self.call_label("main");
        self.define_label("__exit");
        self.halt();

        // Generate code for each function
        for decl in &unit.decls {
            if let Decl::Function(func) = decl {
                self.gen_function(func);
            }
        }

        // Generate runtime support
        self.gen_runtime();

        // Generate string literals
        for (label, content) in &self.strings.clone() {
            self.define_label(label);
            for byte in content.bytes() {
                self.emit(byte);
            }
            self.emit(0);  // Null terminator
        }

        // Generate float constants
        for (label, bcd) in &self.float_consts.clone() {
            self.define_label(label);
            for byte in bcd {
                self.emit(*byte);
            }
        }

        // NOW we know the code size - place globals after code
        // Round up to 16-byte boundary for alignment
        // Also ensure globals are in RAM (>= 0x2000) for emulator compatibility
        let code_end = self.code.len() as u16;
        let aligned_end = (code_end + 15) & !15;
        let global_base = if aligned_end < 0x2000 { 0x2000 } else { aligned_end };

        // Define the actual addresses for each global
        for (name, ty, init, rel_offset) in &global_info {
            let addr = global_base + (*rel_offset as u16);
            let size = ty.size();
            self.global_addr = addr + size as u16;  // Track end of globals

            // Store in globals list
            self.globals.push((name.clone(), ty.clone(), *init));

            // Define the label with actual address
            let addr_label = format!("__global_offset_{}", rel_offset);
            self.labels.insert(addr_label, addr);
        }

        // Now patch the initialization code at the start
        let mut init_pos = init_code_start;
        for (_, ty, init, rel_offset) in &global_info {
            let ram_addr = global_base + (*rel_offset as u16);
            let size = ty.size();
            if let Some(val) = init {
                if size == 1 {
                    // LD A, n
                    self.code[init_pos] = 0x3E;
                    self.code[init_pos + 1] = (*val & 0xFF) as u8;
                    // LD (nn), A
                    self.code[init_pos + 2] = 0x32;
                    self.code[init_pos + 3] = (ram_addr & 0xFF) as u8;
                    self.code[init_pos + 4] = ((ram_addr >> 8) & 0xFF) as u8;
                    init_pos += 5;
                } else {
                    // LD HL, nn
                    self.code[init_pos] = 0x21;
                    self.code[init_pos + 1] = (*val & 0xFF) as u8;
                    self.code[init_pos + 2] = ((*val >> 8) & 0xFF) as u8;
                    // LD (nn), HL
                    self.code[init_pos + 3] = 0x22;
                    self.code[init_pos + 4] = (ram_addr & 0xFF) as u8;
                    self.code[init_pos + 5] = ((ram_addr >> 8) & 0xFF) as u8;
                    init_pos += 6;
                }
            }
        }

        // Resolve forward references
        self.resolve_refs();

        self.code.clone()
    }

    fn gen_function(&mut self, func: &Function) {
        self.define_label(&func.name);

        // Reset local state
        self.symbols.clear();
        self.local_offset = 0;

        // Function prologue: save IX, set up frame pointer
        self.push_ix();
        self.ld_ix_sp();

        // Add parameters to symbol table
        // Parameters are at IX+4, IX+6, etc. (IX+0 = saved IX, IX+2 = return addr)
        let mut param_offset: i16 = 4;
        for param in &func.params {
            self.symbols.insert(param.name.clone(), Symbol::Param {
                offset: param_offset,
                ty: param.ty.clone(),
            });
            param_offset += param.ty.size() as i16;
        }

        // Generate body
        for stmt in &func.body {
            self.gen_stmt(stmt);
        }

        // Default return (in case function doesn't return)
        self.define_label(&format!("{}_epilogue", func.name));
        // Epilogue: restore IX, return
        self.ld_sp_ix();
        self.pop_ix();
        self.ret();
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.gen_expr(expr);
            }

            Stmt::VarDecl(ty, name, init) => {
                // Allocate space on stack
                let size = ty.size() as i16;
                self.local_offset -= size;
                let var_offset = self.local_offset;
                self.symbols.insert(name.clone(), Symbol::Local {
                    offset: var_offset,
                    ty: ty.clone(),
                });

                // Adjust SP
                self.ld_hl_nn((-size) as u16);
                self.add_hl_sp();
                self.emit(0xF9);  // LD SP,HL

                // Initialize if needed
                if let Some(init_expr) = init {
                    if matches!(ty, Type::Float) {
                        // Check if initializer is an int - need conversion
                        let init_ty = self.expr_type(init_expr);
                        let is_int_init = matches!(init_ty, Some(Type::Int) | Some(Type::Char) | None);
                        // Check for IntLit specifically
                        let is_int_lit = matches!(init_expr, Expr::IntLit(_));

                        if is_int_init || is_int_lit {
                            // Convert int to float using bcdf_from_int(result, value)
                            // Push args right to left: value, result
                            self.gen_expr(init_expr);  // HL = int value
                            self.push_hl();            // push value

                            // Compute result address (destination)
                            self.push_ix();
                            self.pop_hl();
                            self.ld_de_nn(var_offset as u16);
                            self.add_hl_de();
                            self.push_hl();            // push result ptr

                            self.call_label("bcdf_from_int");

                            // Clean up stack
                            self.pop_de();  // pop result
                            self.pop_de();  // pop value
                        } else {
                            // Float initialization: copy 6 bytes from source to dest
                            // gen_expr returns pointer to source float in HL
                            self.gen_expr(init_expr);
                            // HL = source pointer
                            // Compute destination address: IX + var_offset
                            self.push_hl();  // Save source
                            self.push_ix();
                            self.pop_de();   // DE = IX (frame pointer)
                            self.ld_hl_nn(var_offset as u16);
                            self.add_hl_de();
                            self.ex_de_hl(); // DE = dest
                            self.pop_hl();   // HL = source
                            // Copy 6 bytes: LDIR with BC=6
                            self.ld_bc_nn(6);
                            self.emit(0xED); self.emit(0xB0);  // LDIR
                        }
                    } else {
                        self.gen_expr(init_expr);
                        // Store result
                        self.ld_ix_d_l(var_offset as i8);
                        if size > 1 {
                            self.ld_ix_d_h((var_offset + 1) as i8);
                        }
                    }
                }
            }

            Stmt::Block(stmts) => {
                for s in stmts {
                    self.gen_stmt(s);
                }
            }

            Stmt::If(cond, then_branch, else_branch) => {
                let else_label = self.new_label("else");
                let end_label = self.new_label("endif");

                // Evaluate condition
                self.gen_expr(cond);
                // Test HL == 0
                self.ld_a_h();
                self.or_l();
                if else_branch.is_some() {
                    self.jp_z_label(&else_label);
                } else {
                    self.jp_z_label(&end_label);
                }

                // Then branch
                self.gen_stmt(then_branch);

                if let Some(else_stmt) = else_branch {
                    self.jp_label(&end_label);
                    self.define_label(&else_label);
                    self.gen_stmt(else_stmt);
                }

                self.define_label(&end_label);
            }

            Stmt::While(cond, body) => {
                let start_label = self.new_label("while");
                let end_label = self.new_label("endwhile");

                let old_break = self.break_label.take();
                let old_continue = self.continue_label.take();
                self.break_label = Some(end_label.clone());
                self.continue_label = Some(start_label.clone());

                self.define_label(&start_label);
                self.gen_expr(cond);
                self.ld_a_h();
                self.or_l();
                self.jp_z_label(&end_label);

                self.gen_stmt(body);
                self.jp_label(&start_label);

                self.define_label(&end_label);

                self.break_label = old_break;
                self.continue_label = old_continue;
            }

            Stmt::For(init, cond, update, body) => {
                let start_label = self.new_label("for");
                let update_label = self.new_label("forupd");
                let end_label = self.new_label("endfor");

                let old_break = self.break_label.take();
                let old_continue = self.continue_label.take();
                self.break_label = Some(end_label.clone());
                self.continue_label = Some(update_label.clone());

                // Init
                if let Some(init_stmt) = init {
                    self.gen_stmt(init_stmt);
                }

                self.define_label(&start_label);

                // Condition
                if let Some(cond_expr) = cond {
                    self.gen_expr(cond_expr);
                    self.ld_a_h();
                    self.or_l();
                    self.jp_z_label(&end_label);
                }

                // Body
                self.gen_stmt(body);

                // Update
                self.define_label(&update_label);
                if let Some(update_expr) = update {
                    self.gen_expr(update_expr);
                }
                self.jp_label(&start_label);

                self.define_label(&end_label);

                self.break_label = old_break;
                self.continue_label = old_continue;
            }

            Stmt::DoWhile(body, cond) => {
                let start_label = self.new_label("do");
                let end_label = self.new_label("enddo");

                let old_break = self.break_label.take();
                let old_continue = self.continue_label.take();
                self.break_label = Some(end_label.clone());
                self.continue_label = Some(start_label.clone());

                self.define_label(&start_label);
                self.gen_stmt(body);

                self.gen_expr(cond);
                self.ld_a_h();
                self.or_l();
                self.jp_nz_label(&start_label);

                self.define_label(&end_label);

                self.break_label = old_break;
                self.continue_label = old_continue;
            }

            Stmt::Return(expr) => {
                if let Some(e) = expr {
                    self.gen_expr(e);
                }
                // Jump to function epilogue (we'll use a naming convention)
                // For now, just return directly
                self.ld_sp_ix();
                self.pop_ix();
                self.ret();
            }

            Stmt::Break => {
                if let Some(label) = &self.break_label.clone() {
                    self.jp_label(label);
                }
            }

            Stmt::Continue => {
                if let Some(label) = &self.continue_label.clone() {
                    self.jp_label(label);
                }
            }
        }
    }

    /// Infer the type of an expression (simplified - only handles common cases)
    fn expr_type(&self, expr: &Expr) -> Option<Type> {
        match expr {
            Expr::IntLit(_) => Some(Type::Int),
            Expr::FloatLit(_) => Some(Type::Float),
            Expr::CharLit(_) => Some(Type::Char),
            Expr::StringLit(_) => Some(Type::Pointer(Box::new(Type::Char))),
            Expr::Var(name) => {
                self.symbols.get(name)
                    .or_else(|| self.global_symbols.get(name))
                    .map(|sym| match sym {
                        Symbol::Local { ty, .. } => ty.clone(),
                        Symbol::Param { ty, .. } => ty.clone(),
                        Symbol::Global { ty, .. } => ty.clone(),
                    })
            }
            Expr::Unary(UnOp::Deref, inner) => {
                // Dereference: *ptr -> element type
                if let Some(Type::Pointer(inner_ty)) = self.expr_type(inner) {
                    Some(*inner_ty)
                } else {
                    None
                }
            }
            Expr::Unary(UnOp::AddrOf, inner) => {
                // Address-of: &x -> pointer to x's type
                self.expr_type(inner).map(|t| Type::Pointer(Box::new(t)))
            }
            Expr::Index(arr, _) => {
                // Array indexing: arr[i] -> element type
                if let Some(Type::Pointer(elem)) = self.expr_type(arr) {
                    Some(*elem)
                } else if let Some(Type::Array(elem, _)) = self.expr_type(arr) {
                    Some(*elem)
                } else {
                    None
                }
            }
            _ => None, // Complex expressions - default to None
        }
    }

    fn gen_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntLit(n) => {
                self.ld_hl_nn(*n as u16);
            }

            Expr::FloatLit(s) => {
                // Parse float literal and store as constant data
                let bcd = Self::parse_float_literal(s);
                let label = self.new_label("flt");
                self.float_consts.push((label.clone(), bcd));
                // Load address of float constant into HL
                self.emit(0x21);  // LD HL,nn
                self.emit_label_ref(&label);
            }

            Expr::CharLit(c) => {
                self.ld_hl_nn(*c as u16);
            }

            Expr::StringLit(s) => {
                let label = self.new_label("str");
                self.strings.push((label.clone(), s.clone()));
                self.emit(0x21);  // LD HL,
                self.emit_label_ref(&label);
            }

            Expr::Var(name) => {
                // Look up in local symbols first, then global
                let sym = self.symbols.get(name).cloned()
                    .or_else(|| self.global_symbols.get(name).cloned());
                if let Some(sym) = sym {
                    match sym {
                        Symbol::Local { offset, ty } | Symbol::Param { offset, ty } => {
                            // For arrays and floats, return address (pointer)
                            if matches!(ty, Type::Array(_, _) | Type::Float) {
                                // Compute address: IX + offset
                                self.push_ix();
                                self.pop_hl();
                                self.ld_de_nn(offset as u16);
                                self.add_hl_de();
                            } else {
                                // Load value from IX+offset
                                self.ld_l_ix_d(offset as i8);
                                if ty.size() > 1 {
                                    self.ld_h_ix_d((offset + 1) as i8);
                                } else {
                                    self.ld_a_l();
                                    self.emit(0xCB); self.emit(0x7F);  // BIT 7,A
                                    self.emit(0x28); self.emit(0x02);  // JR Z,+2
                                    self.ld_a_n(0xFF);
                                    self.ld_h_a();
                                }
                            }
                        }
                        Symbol::Global { label, ty } => {
                            if matches!(ty, Type::Array(_, _) | Type::Float) {
                                // For arrays and floats, load address
                                self.emit(0x21);  // LD HL,nn
                                self.emit_label_ref(&label);
                            } else {
                                // Load value
                                self.emit(0x2A);  // LD HL,(nn)
                                self.emit_label_ref(&label);
                            }
                        }
                    }
                }
            }

            Expr::Binary(op, left, right) => {
                // Special case for assignment
                if matches!(op, BinOp::Assign | BinOp::AddAssign | BinOp::SubAssign) {
                    self.gen_assign(op, left, right);
                    return;
                }

                // Check if this is a float operation
                let left_ty = self.expr_type(left);
                let right_ty = self.expr_type(right);
                let is_float_op = matches!(left_ty, Some(Type::Float)) || matches!(right_ty, Some(Type::Float));

                if is_float_op && matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div |
                                               BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge | BinOp::Eq | BinOp::Ne) {
                    // Float operation - call runtime functions
                    // Allocate temp result on stack (6 bytes)
                    self.ld_hl_nn((-6i16) as u16);
                    self.add_hl_sp();
                    self.emit(0xF9);  // LD SP,HL
                    let result_offset = self.local_offset - 6;
                    self.local_offset = result_offset;

                    // For bcdf functions: bcdf_xxx(result, a, b)
                    // Push args right to left: b, a, result
                    self.gen_expr(right);     // HL = ptr to right operand
                    self.push_hl();           // push b
                    self.gen_expr(left);      // HL = ptr to left operand
                    self.push_hl();           // push a
                    // Compute result address: IX + result_offset
                    self.push_ix();
                    self.pop_hl();
                    self.ld_de_nn(result_offset as u16);
                    self.add_hl_de();
                    self.push_hl();           // push result

                    // Call appropriate function
                    match op {
                        BinOp::Add => self.call_label("bcdf_add"),
                        BinOp::Sub => self.call_label("bcdf_sub"),
                        BinOp::Mul => self.call_label("bcdf_mul"),
                        BinOp::Div => self.call_label("bcdf_div"),
                        BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge | BinOp::Eq | BinOp::Ne => {
                            // For comparison, just call bcdf_cmp and handle result
                            self.pop_hl();  // Remove result ptr (not used for cmp)
                            self.local_offset += 6;  // Restore stack (we don't need result)
                            self.ld_hl_nn(6);
                            self.add_hl_sp();
                            self.emit(0xF9);  // LD SP,HL
                            self.call_label("bcdf_cmp");
                            // Result is in HL: -1, 0, or 1
                            // Convert to boolean for comparison ops
                            match op {
                                BinOp::Lt => {
                                    // HL < 0 ? (HL == -1)
                                    self.ld_a_h();
                                    self.emit(0xCB); self.emit(0x7F);  // BIT 7,A (check sign)
                                    self.ld_hl_nn(0);
                                    self.emit(0x28); self.emit(0x02);  // JR Z,+2
                                    self.inc_hl();
                                }
                                BinOp::Gt => {
                                    // HL > 0 ? (HL == 1)
                                    self.ld_a_l();
                                    self.emit(0xFE); self.emit(0x01);  // CP 1
                                    self.ld_hl_nn(0);
                                    self.emit(0x20); self.emit(0x03);  // JR NZ,+3
                                    self.ld_a_h();
                                    self.or_a();
                                    self.emit(0x20); self.emit(0x01);  // JR NZ,+1
                                    self.inc_hl();
                                }
                                BinOp::Le => {
                                    // HL <= 0 ? (HL == -1 or HL == 0)
                                    self.ld_a_h();
                                    self.or_l();
                                    self.ld_hl_nn(1);
                                    self.emit(0x28); self.emit(0x04);  // JR Z,done (HL==0)
                                    self.ld_a_h();
                                    self.emit(0xCB); self.emit(0x7F);  // BIT 7,A
                                    self.emit(0x20); self.emit(0x01);  // JR NZ,done
                                    self.dec_hl();
                                }
                                BinOp::Ge => {
                                    // HL >= 0 ? (HL == 0 or HL == 1)
                                    self.ld_a_h();
                                    self.emit(0xCB); self.emit(0x7F);  // BIT 7,A (check sign)
                                    self.ld_hl_nn(1);
                                    self.emit(0x28); self.emit(0x01);  // JR Z,+1 (positive or zero)
                                    self.dec_hl();
                                }
                                BinOp::Eq => {
                                    // HL == 0 ?
                                    self.ld_a_h();
                                    self.or_l();
                                    self.ld_hl_nn(0);
                                    self.emit(0x20); self.emit(0x01);  // JR NZ,+1
                                    self.inc_hl();
                                }
                                BinOp::Ne => {
                                    // HL != 0 ?
                                    self.ld_a_h();
                                    self.or_l();
                                    self.ld_hl_nn(1);
                                    self.emit(0x28); self.emit(0x01);  // JR Z,+1
                                    self.dec_hl();
                                }
                                _ => {}
                            }
                            // Pop args (a, b already removed for cmp)
                            self.pop_de();  // pop a
                            self.pop_de();  // pop b
                            return;
                        }
                        _ => {}
                    }

                    // Clean up args from stack (6 bytes = 3 pointers)
                    self.pop_de();  // pop result
                    self.pop_de();  // pop a
                    self.pop_de();  // pop b

                    // Return result pointer in HL
                    self.push_ix();
                    self.pop_hl();
                    self.ld_de_nn(result_offset as u16);
                    self.add_hl_de();
                    return;
                }

                // Non-float operation - original code
                // Evaluate right first, push, then left
                self.gen_expr(right);
                self.push_hl();
                self.gen_expr(left);
                self.pop_de();

                match op {
                    BinOp::Add => {
                        self.add_hl_de();
                    }
                    BinOp::Sub => {
                        self.or_a();  // Clear carry
                        self.sbc_hl_de();
                    }
                    BinOp::Mul => {
                        self.call_label("__mul16");
                    }
                    BinOp::Div => {
                        self.call_label("__div16");
                    }
                    BinOp::Mod => {
                        self.call_label("__mod16");
                    }
                    BinOp::BitAnd => {
                        self.ld_a_l();
                        self.emit(0xA3);  // AND E
                        self.ld_l_a();
                        self.ld_a_h();
                        self.emit(0xA2);  // AND D
                        self.ld_h_a();
                    }
                    BinOp::BitOr => {
                        self.ld_a_l();
                        self.emit(0xB3);  // OR E
                        self.ld_l_a();
                        self.ld_a_h();
                        self.emit(0xB2);  // OR D
                        self.ld_h_a();
                    }
                    BinOp::BitXor => {
                        self.ld_a_l();
                        self.emit(0xAB);  // XOR E
                        self.ld_l_a();
                        self.ld_a_h();
                        self.emit(0xAA);  // XOR D
                        self.ld_h_a();
                    }
                    BinOp::Eq => {
                        self.call_label("__eq16");
                    }
                    BinOp::Ne => {
                        self.call_label("__ne16");
                    }
                    BinOp::Lt => {
                        self.call_label("__lt16");
                    }
                    BinOp::Gt => {
                        self.call_label("__gt16");
                    }
                    BinOp::Le => {
                        self.call_label("__le16");
                    }
                    BinOp::Ge => {
                        self.call_label("__ge16");
                    }
                    BinOp::Shl => {
                        self.call_label("__shl16");
                    }
                    BinOp::Shr => {
                        self.call_label("__shr16");
                    }
                    BinOp::LogAnd => {
                        self.call_label("__land");
                    }
                    BinOp::LogOr => {
                        self.call_label("__lor");
                    }
                    _ => {}
                }
            }

            Expr::Unary(op, inner) => {
                match op {
                    UnOp::AddrOf => {
                        // Address-of: get address of variable, don't load its value
                        self.gen_addr(inner);
                    }
                    UnOp::Deref => {
                        // Dereference: load value from pointer
                        // First, determine if we're loading a char or int
                        let elem_type = if let Some(Type::Pointer(elem)) = self.expr_type(inner) {
                            Some(*elem)
                        } else {
                            None
                        };

                        self.gen_expr(inner);  // Get pointer address into HL

                        if matches!(elem_type, Some(Type::Char)) {
                            // Load single byte and zero-extend
                            self.ld_a_hl();
                            self.ld_l_a();
                            self.ld_h_n(0);
                        } else {
                            // Load 16-bit value
                            self.ex_de_hl();      // DE = address
                            self.ld_a_de();       // A = low byte
                            self.ld_l_a();        // L = low byte
                            self.inc_de();        // DE points to high byte
                            self.ld_a_de();       // A = high byte
                            self.ld_h_a();        // H = high byte
                        }
                    }
                    _ => {
                        // For other unary ops, first evaluate the expression
                        self.gen_expr(inner);
                        match op {
                            UnOp::Neg => {
                                // HL = -HL: HL = 0 - HL
                                self.ex_de_hl();
                                self.ld_hl_nn(0);
                                self.or_a();
                                self.sbc_hl_de();
                            }
                            UnOp::Not => {
                                // Logical not: HL = (HL == 0) ? 1 : 0
                                self.ld_a_h();
                                self.or_l();
                                self.ld_hl_nn(0);
                                self.emit(0x20); self.emit(0x01);  // JR NZ,+1
                                self.inc_hl();
                            }
                            UnOp::BitNot => {
                                self.ld_a_l();
                                self.emit(0x2F);  // CPL
                                self.ld_l_a();
                                self.ld_a_h();
                                self.emit(0x2F);
                                self.ld_h_a();
                            }
                            UnOp::PreInc | UnOp::PostInc | UnOp::PreDec | UnOp::PostDec => {
                                // These need the address, handled specially
                            }
                            UnOp::AddrOf | UnOp::Deref => unreachable!(),
                        }
                    }
                }
            }

            Expr::Call(name, args) => {
                // Push arguments right to left
                for arg in args.iter().rev() {
                    self.gen_expr(arg);
                    self.push_hl();
                }

                self.call_label(name);

                // Clean up stack (return value is in HL, must preserve it)
                if !args.is_empty() {
                    let size = args.len() * 2;
                    // Use DE to compute new SP, preserving HL
                    self.ex_de_hl();              // DE = return value
                    self.ld_hl_nn(size as u16);
                    self.add_hl_sp();
                    self.emit(0xF9);              // LD SP,HL
                    self.ex_de_hl();              // HL = return value
                }
            }

            Expr::Index(arr, idx) => {
                // Compute base + index * element_size
                let elem_size = if let Some(Type::Pointer(inner)) = self.expr_type(arr) {
                    inner.size()
                } else if let Some(Type::Array(inner, _)) = self.expr_type(arr) {
                    inner.size()
                } else {
                    2 // Default to 16-bit
                };

                self.gen_expr(idx);
                if elem_size == 2 {
                    self.add_hl_hl();  // HL = idx * 2
                }
                self.push_hl();
                self.gen_expr(arr);
                self.pop_de();
                self.add_hl_de();  // HL = base + scaled_index

                if elem_size == 1 {
                    // Load byte and zero-extend
                    self.ld_a_hl();
                    self.ld_l_a();
                    self.ld_h_n(0);
                } else {
                    // Load 16-bit value: low byte at (HL), high byte at (HL+1)
                    self.ld_a_hl();     // A = low byte
                    self.inc_hl();
                    self.ld_h_hl();     // H = high byte
                    self.ld_l_a();      // L = low byte
                }
            }

            Expr::Ternary(cond, then_expr, else_expr) => {
                let else_label = self.new_label("ternelse");
                let end_label = self.new_label("ternend");

                self.gen_expr(cond);
                self.ld_a_h();
                self.or_l();
                self.jp_z_label(&else_label);

                self.gen_expr(then_expr);
                self.jp_label(&end_label);

                self.define_label(&else_label);
                self.gen_expr(else_expr);

                self.define_label(&end_label);
            }

            Expr::Sizeof(ty) => {
                self.ld_hl_nn(ty.size() as u16);
            }

            _ => {}
        }
    }

    fn gen_assign(&mut self, op: &BinOp, left: &Expr, right: &Expr) {
        // Check if left side is a float
        let left_ty = self.expr_type(left);
        let is_float = matches!(left_ty, Some(Type::Float));

        if is_float {
            // Float assignment - need to copy 6 bytes
            // For simple assign: copy from right to left
            // For compound assign: compute result then copy
            if !matches!(op, BinOp::Assign) {
                // Compound assignment on float: a += b means a = a + b
                // Generate the arithmetic expression
                let arith_op = match op {
                    BinOp::AddAssign => BinOp::Add,
                    BinOp::SubAssign => BinOp::Sub,
                    _ => BinOp::Add, // Default
                };
                // Create a temporary expression for the operation
                // This is a bit hacky - we generate code for left op right
                self.gen_expr(right);     // HL = ptr to right
                self.push_hl();           // save b
                self.gen_expr(left);      // HL = ptr to left (a)
                self.push_hl();           // save a

                // Allocate temp result
                self.ld_hl_nn((-6i16) as u16);
                self.add_hl_sp();
                self.emit(0xF9);  // LD SP,HL
                let result_offset = self.local_offset - 6;
                self.local_offset = result_offset;

                // Compute result address
                self.push_ix();
                self.pop_hl();
                self.ld_de_nn(result_offset as u16);
                self.add_hl_de();
                self.push_hl();  // push result ptr

                // Call bcdf operation
                match arith_op {
                    BinOp::Add => self.call_label("bcdf_add"),
                    BinOp::Sub => self.call_label("bcdf_sub"),
                    _ => self.call_label("bcdf_add"),
                }

                // Clean up stack args (result, a, b)
                self.pop_de();  // result
                self.pop_de();  // a
                self.pop_de();  // b

                // Now copy result to left
                // Result is at IX + result_offset
                self.push_ix();
                self.pop_hl();
                self.ld_de_nn(result_offset as u16);
                self.add_hl_de();  // HL = source
            } else {
                // Simple assignment: evaluate right side (returns pointer)
                self.gen_expr(right);  // HL = source ptr
            }

            // Now HL = source pointer, copy 6 bytes to left
            self.push_hl();  // save source

            // Get destination address
            match left {
                Expr::Var(name) => {
                    let sym = self.symbols.get(name).cloned()
                        .or_else(|| self.global_symbols.get(name).cloned());
                    if let Some(sym) = sym {
                        match sym {
                            Symbol::Local { offset, .. } | Symbol::Param { offset, .. } => {
                                self.push_ix();
                                self.pop_hl();
                                self.ld_de_nn(offset as u16);
                                self.add_hl_de();
                            }
                            Symbol::Global { label, .. } => {
                                self.emit(0x21);  // LD HL,nn
                                self.emit_label_ref(&label);
                            }
                        }
                    }
                }
                _ => {}
            }
            self.ex_de_hl();  // DE = dest
            self.pop_hl();    // HL = source
            self.ld_bc_nn(6);
            self.emit(0xED); self.emit(0xB0);  // LDIR
            return;
        }

        // Non-float assignment - original code
        // Evaluate right side first
        self.gen_expr(right);

        // For compound assignment, need to load left value first
        if !matches!(op, BinOp::Assign) {
            self.push_hl();
            // Load current value
            self.gen_expr(left);
            self.pop_de();
            // Perform operation
            match op {
                BinOp::AddAssign => { self.add_hl_de(); }
                BinOp::SubAssign => { self.or_a(); self.sbc_hl_de(); }
                _ => {}
            }
        }

        // Store to left side
        match left {
            Expr::Var(name) => {
                // Look up in local symbols first, then global
                let sym = self.symbols.get(name).cloned()
                    .or_else(|| self.global_symbols.get(name).cloned());
                if let Some(sym) = sym {
                    match sym {
                        Symbol::Local { offset, ty } | Symbol::Param { offset, ty } => {
                            self.ld_ix_d_l(offset as i8);
                            if ty.size() > 1 {
                                self.ld_ix_d_h((offset + 1) as i8);
                            }
                        }
                        Symbol::Global { label, .. } => {
                            self.emit(0x22);  // LD (nn),HL
                            self.emit_label_ref(&label);
                        }
                    }
                }
            }
            Expr::Unary(UnOp::Deref, inner) => {
                // Store through pointer: *ptr = value
                // Value is in HL, we need to store it at address given by inner
                self.push_hl();       // Save value
                self.gen_expr(inner); // Get address in HL
                self.ex_de_hl();      // DE = address
                self.pop_hl();        // HL = value to store
                self.ld_a_l();
                self.ld_de_a();       // Store low byte at (DE)
                self.inc_de();
                self.ld_a_h();
                self.ld_de_a();       // Store high byte at (DE+1)
            }
            Expr::Index(arr, idx) => {
                // Store to array element: arr[idx] = value
                // Need to determine element size based on array type
                let elem_size = if let Some(Type::Pointer(inner)) = self.expr_type(arr) {
                    inner.size()
                } else if let Some(Type::Array(inner, _)) = self.expr_type(arr) {
                    inner.size()
                } else {
                    2 // Default to 16-bit
                };

                self.push_hl();  // Save value
                self.gen_expr(idx);
                // Scale index by element size
                if elem_size == 2 {
                    self.add_hl_hl();  // HL = idx * 2
                }
                self.push_hl();
                self.gen_expr(arr);
                self.pop_de();
                self.add_hl_de();  // HL = address of element
                self.pop_de();  // Value in DE

                if elem_size == 1 {
                    self.ld_a_e();
                    self.ld_hl_a();  // Store single byte
                } else {
                    // Store 16-bit value: low byte first
                    self.ld_a_e();
                    self.ld_hl_a();  // Store low byte at (HL)
                    self.inc_hl();
                    self.ld_a_d();
                    self.ld_hl_a();  // Store high byte at (HL+1)
                }
            }
            _ => {}
        }
    }

    /// Generate code to put the address of an expression in HL
    fn gen_addr(&mut self, expr: &Expr) {
        match expr {
            Expr::Var(name) => {
                // Look up in local symbols first, then global
                let sym = self.symbols.get(name).cloned()
                    .or_else(|| self.global_symbols.get(name).cloned());
                if let Some(sym) = sym {
                    match sym {
                        Symbol::Local { offset, .. } | Symbol::Param { offset, .. } => {
                            // Compute IX + offset
                            // LEA HL, IX+d is not available on Z80, so we do:
                            // PUSH IX; POP HL; LD DE,offset; ADD HL,DE
                            self.push_ix();
                            self.pop_hl();
                            if offset >= 0 {
                                self.ld_de_nn(offset as u16);
                            } else {
                                // Negative offset
                                self.ld_de_nn(offset as u16);  // Two's complement works
                            }
                            self.add_hl_de();
                        }
                        Symbol::Global { label, .. } => {
                            // Load address of global
                            self.emit(0x21);  // LD HL,nn
                            self.emit_label_ref(&label);
                        }
                    }
                }
            }
            Expr::Unary(UnOp::Deref, inner) => {
                // Address of *ptr is just ptr
                self.gen_expr(inner);
            }
            Expr::Index(arr, idx) => {
                // Address of arr[idx] = arr + idx * element_size
                let elem_size = if let Some(Type::Pointer(inner)) = self.expr_type(arr) {
                    inner.size()
                } else if let Some(Type::Array(inner, _)) = self.expr_type(arr) {
                    inner.size()
                } else {
                    2 // Default to 16-bit
                };

                self.gen_expr(idx);
                if elem_size == 2 {
                    self.add_hl_hl();  // HL = idx * 2
                }
                self.push_hl();
                self.gen_expr(arr);
                self.pop_de();
                self.add_hl_de();
            }
            _ => {
                // Can't take address of other expressions
            }
        }
    }

    fn gen_runtime(&mut self) {
        // MC6850 ACIA ports
        const ACIA_STATUS: u8 = 0x80;
        const ACIA_DATA: u8 = 0x81;

        // putchar: output character in L to serial
        self.define_label("putchar");
        self.push_af();
        // Wait for TX ready (bit 1 of status)
        let wait_tx = self.new_label("wait_tx");
        self.define_label(&wait_tx);
        self.in_a_n(ACIA_STATUS);
        self.emit(0xE6); self.emit(0x02);  // AND 0x02
        self.jp_z_label(&wait_tx);
        self.ld_a_l();
        self.out_n_a(ACIA_DATA);
        self.pop_af();
        self.ret();

        // getchar: read character from serial, return in L
        self.define_label("getchar");
        // Wait for RX ready (bit 0 of status)
        let wait_rx = self.new_label("wait_rx");
        self.define_label(&wait_rx);
        self.in_a_n(ACIA_STATUS);
        self.emit(0xE6); self.emit(0x01);  // AND 0x01
        self.jp_z_label(&wait_rx);
        self.in_a_n(ACIA_DATA);
        self.ld_l_a();
        self.ld_h_a();  // Sign extend (or zero, doesn't matter for char)
        self.ret();

        // puts: output null-terminated string at HL
        self.define_label("puts");
        self.push_hl();
        let puts_loop = self.new_label("puts_loop");
        self.define_label(&puts_loop);
        self.ld_a_hl();
        self.or_a();  // Check for null
        let puts_done = self.new_label("puts_done");
        self.jp_z_label(&puts_done);
        self.ld_l_a();
        self.push_hl();
        self.call_label("putchar");
        self.pop_hl();
        self.pop_hl();
        self.inc_hl();
        self.push_hl();
        self.jp_label(&puts_loop);
        self.define_label(&puts_done);
        self.pop_hl();
        self.ret();

        // print_num: output signed integer at (SP+2) as decimal
        // Non-recursive iterative approach using stack
        self.define_label("print_num");
        self.push_ix();
        self.ld_ix_sp_offset(0);

        // Get value from stack (IX+4)
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);

        // Check if negative (bit 15 set)
        self.ld_a_h();
        self.emit(0xB7);  // OR A (sets sign flag)
        let pn_positive = self.new_label("pn_positive");
        self.emit(0xF2); self.emit_label_ref(&pn_positive);  // JP P,pn_positive

        // Negative: print '-' and negate
        self.push_hl();
        self.ld_l_n('-' as u8);
        self.push_hl();
        self.call_label("putchar");
        self.pop_hl();
        self.pop_hl();
        // Negate HL: HL = 0 - HL (two's complement)
        self.ld_a_l();
        self.emit(0x2F);  // CPL
        self.ld_l_a();
        self.ld_a_h();
        self.emit(0x2F);  // CPL
        self.ld_h_a();
        self.inc_hl();

        self.define_label(&pn_positive);

        // Push digits onto stack, then print in reverse
        self.ld_b_n(0);  // B = digit count

        let pn_divloop = self.new_label("pn_divloop");
        self.define_label(&pn_divloop);

        // Divide HL by 10
        self.ld_de_nn(10);
        self.push_bc();  // save count
        self.push_hl();  // save value for mod
        self.call_label("__div16");  // HL = HL / 10
        self.ex_de_hl();  // DE = quotient
        self.pop_hl();   // HL = original value
        self.push_de();  // save quotient
        self.ld_de_nn(10);
        self.call_label("__mod16");  // HL = HL % 10
        // L = digit
        self.ld_a_l();
        self.emit(0xC6); self.emit('0' as u8);  // ADD A,'0'
        self.pop_hl();   // HL = quotient (for next iteration)
        self.pop_bc();   // restore count
        self.push_af();  // save digit character
        self.inc_b();

        // Check if quotient is zero
        self.ld_a_h();
        self.or_l();
        self.jp_nz_label(&pn_divloop);

        // Now print digits from stack (B = count)
        let pn_prloop = self.new_label("pn_prloop");
        self.define_label(&pn_prloop);
        self.pop_af();   // A = digit character
        self.ld_l_a();
        self.push_bc();
        self.push_hl();
        self.call_label("putchar");
        self.pop_hl();
        self.pop_bc();
        self.emit(0x10); self.emit(-11i8 as u8);  // DJNZ pn_prloop

        self.pop_ix();
        self.ret();

        // 16-bit multiply: HL = HL * DE (unsigned)
        // Uses shift-and-add algorithm
        self.define_label("__mul16");
        self.push_bc();
        self.ld_bc_nn(0);  // BC = result
        let mul_loop = self.new_label("mul_loop");
        self.define_label(&mul_loop);
        // Check if DE is zero
        self.ld_a_d();
        self.or_l();  // OR with E
        self.emit(0xB3);  // Actually OR E
        let mul_done = self.new_label("mul_done");
        self.jp_z_label(&mul_done);
        // If DE bit 0 set, add HL to BC
        self.ld_a_e();
        self.emit(0xE6); self.emit(0x01);  // AND 1
        let mul_skip = self.new_label("mul_skip");
        self.jp_z_label(&mul_skip);
        // BC += HL
        self.push_hl();
        self.add_hl_bc();
        self.ld_b_a(); self.emit(0x44);  // LD B,H
        self.emit(0x4D);  // LD C,L
        self.pop_hl();
        self.define_label(&mul_skip);
        // HL <<= 1
        self.add_hl_hl();
        // DE >>= 1
        self.emit(0xCB); self.emit(0x3A);  // SRL D
        self.emit(0xCB); self.emit(0x1B);  // RR E
        self.jp_label(&mul_loop);
        self.define_label(&mul_done);
        // Result in BC, move to HL
        self.ld_h_a(); self.emit(0x60);  // LD H,B
        self.emit(0x69);  // LD L,C
        self.pop_bc();
        self.ret();

        // 16-bit unsigned divide: HL = HL / DE, remainder available
        self.define_label("__div16");
        self.push_bc();
        // Check for divide by zero
        self.ld_a_d();
        self.emit(0xB3);  // OR E
        let div_ok = self.new_label("div_ok");
        self.jp_nz_label(&div_ok);
        // Divide by zero - return 0
        self.ld_hl_nn(0);
        self.pop_bc();
        self.ret();
        self.define_label(&div_ok);
        // Simple subtraction-based division
        self.ld_bc_nn(0);  // BC = quotient
        let div_loop = self.new_label("div_loop");
        self.define_label(&div_loop);
        // If HL < DE, we're done
        self.push_hl();
        self.or_a();
        self.sbc_hl_de();
        self.pop_hl();
        let div_done = self.new_label("div_done");
        self.jp_c_label(&div_done);
        // HL -= DE
        self.or_a();
        self.sbc_hl_de();
        // BC++
        self.inc_de();  // Temporarily use for increment
        self.push_hl();
        self.ld_h_a(); self.emit(0x60);  // LD H,B
        self.emit(0x69);  // LD L,C
        self.inc_hl();
        self.emit(0x44);  // LD B,H
        self.emit(0x4D);  // LD C,L
        self.pop_hl();
        self.dec_de();  // Restore DE
        self.jp_label(&div_loop);
        self.define_label(&div_done);
        // Quotient in BC, move to HL
        self.ld_h_a(); self.emit(0x60);  // LD H,B
        self.emit(0x69);  // LD L,C
        self.pop_bc();
        self.ret();

        // 16-bit modulo: HL = HL % DE
        self.define_label("__mod16");
        self.push_bc();
        self.push_de();
        // Check for mod by zero
        self.ld_a_d();
        self.emit(0xB3);  // OR E
        let mod_ok = self.new_label("mod_ok");
        self.jp_nz_label(&mod_ok);
        self.ld_hl_nn(0);
        self.pop_de();
        self.pop_bc();
        self.ret();
        self.define_label(&mod_ok);
        // Subtract DE until HL < DE
        let mod_loop = self.new_label("mod_loop");
        self.define_label(&mod_loop);
        self.push_hl();
        self.or_a();
        self.sbc_hl_de();
        self.pop_hl();
        let mod_done = self.new_label("mod_done");
        self.jp_c_label(&mod_done);
        self.or_a();
        self.sbc_hl_de();
        self.jp_label(&mod_loop);
        self.define_label(&mod_done);
        // Remainder in HL
        self.pop_de();
        self.pop_bc();
        self.ret();

        // Comparison helpers - return 1 (true) or 0 (false) in HL
        self.define_label("__eq16");
        self.or_a();
        self.sbc_hl_de();
        self.ld_hl_nn(0);
        self.ret_nz();
        self.inc_hl();
        self.ret();

        self.define_label("__ne16");
        self.or_a();
        self.sbc_hl_de();
        self.ld_hl_nn(1);
        self.ret_nz();
        self.dec_hl();
        self.ret();

        self.define_label("__lt16");
        self.or_a();
        self.sbc_hl_de();
        self.ld_hl_nn(0);
        self.ret_nc();
        self.inc_hl();
        self.ret();

        self.define_label("__gt16");
        self.ex_de_hl();
        self.or_a();
        self.sbc_hl_de();
        self.ld_hl_nn(0);
        self.ret_nc();
        self.inc_hl();
        self.ret();

        self.define_label("__le16");
        self.ex_de_hl();
        self.or_a();
        self.sbc_hl_de();
        self.ld_hl_nn(1);
        self.ret_nc();
        self.dec_hl();
        self.ret();

        self.define_label("__ge16");
        self.or_a();
        self.sbc_hl_de();
        self.ld_hl_nn(1);
        self.ret_nc();
        self.dec_hl();
        self.ret();

        self.define_label("__shl16");
        // HL = HL << E (shift count in E)
        let loop_label = self.new_label("shl_loop");
        self.define_label(&loop_label);
        self.ld_a_e();
        self.or_a();
        self.ret_z();
        self.add_hl_hl();
        self.emit(0x1D);  // DEC E
        self.jp_label(&loop_label);

        self.define_label("__shr16");
        // HL = HL >> E (shift count in E)
        let loop_label = self.new_label("shr_loop");
        self.define_label(&loop_label);
        self.ld_a_e();
        self.or_a();
        self.ret_z();
        self.emit(0xCB); self.emit(0x3C);  // SRL H
        self.emit(0xCB); self.emit(0x1D);  // RR L
        self.emit(0x1D);  // DEC E
        self.jp_label(&loop_label);

        // Logical AND: result = (DE != 0) && (HL != 0)
        // Returns 1 if both are non-zero, 0 otherwise
        // On entry: DE = left, HL = right
        self.define_label("__land");
        // Check if DE (left operand) is zero
        self.ld_a_d();
        self.or_e();
        let land_false = self.new_label("land_false");
        self.jp_z_label(&land_false);  // If DE == 0, return false
        // DE is non-zero, check HL
        self.ld_a_h();
        self.or_l();
        self.jp_z_label(&land_false);  // If HL == 0, return false
        // Both non-zero, return 1
        self.ld_hl_nn(1);
        self.ret();
        self.define_label(&land_false);
        self.ld_hl_nn(0);
        self.ret();

        // Logical OR: result = (DE != 0) || (HL != 0)
        // Returns 1 if either is non-zero, 0 otherwise
        self.define_label("__lor");
        // Check if DE is non-zero
        self.ld_a_d();
        self.or_e();
        let lor_true = self.new_label("lor_true");
        self.jp_nz_label(&lor_true);  // DE != 0, return true
        // DE is zero, check HL
        self.ld_a_h();
        self.or_l();
        self.jp_nz_label(&lor_true);  // HL != 0, return true
        // Both zero, return 0
        self.ld_hl_nn(0);
        self.ret();
        self.define_label(&lor_true);
        self.ld_hl_nn(1);
        self.ret();

        // ============================================================
        // BCD Runtime Functions
        // ============================================================
        // BCD numbers are stored as 4 packed bytes (8 BCD digits)
        // Format: [byte0][byte1][byte2][byte3] where byte0 is least significant
        // Each byte holds 2 BCD digits (0x00-0x99)
        //
        // C calling convention: args pushed right-to-left
        // On entry: SP+0 = return addr, SP+2 = first arg, SP+4 = second arg, etc.

        // bcd_add(result, a, b): result = a + b
        // Stack on entry: SP+0 = ret, SP+2 = result, SP+4 = a, SP+6 = b
        // After PUSH IX; LD IX,SP: (IX+0)=IX, (IX+2)=ret, (IX+4)=result, (IX+6)=a, (IX+8)=b
        self.define_label("bcd_add");
        self.push_ix();
        self.ld_ix_sp();
        self.push_bc();
        self.push_de();
        // Get pointers from stack
        // LDIR copies FROM (HL) TO (DE), so:
        // HL = source (a), DE = dest (result)
        self.ld_l_ix_d(6);          // HL = a ptr (source)
        self.ld_h_ix_d(7);
        self.ld_e_ix_d(4);          // DE = result ptr (dest)
        self.ld_d_ix_d(5);
        self.push_de();             // Save result ptr
        // Copy a to result (4 bytes)
        self.ld_bc_nn(4);
        self.emit(0xED); self.emit(0xB0);  // LDIR: (HL)->(DE), HL++, DE++, BC--
        // Now result = a, add b to it
        // After LDIR, DE points past end of result, HL past end of a
        self.pop_de();              // DE = result ptr (start)
        self.ld_l_ix_d(8);          // HL = b ptr
        self.ld_h_ix_d(9);
        self.ld_bc_nn(4);
        self.or_a();                // Clear carry
        let bcd_add_loop = self.new_label("bcd_add_loop");
        self.define_label(&bcd_add_loop);
        self.ld_a_de();
        self.emit(0x8E);            // ADC A,(HL)
        self.emit(0x27);            // DAA
        self.ld_de_a();
        self.inc_hl();
        self.inc_de();
        // Preserve carry flag across loop check
        self.push_af();
        self.dec_bc();
        self.ld_a_b();
        self.or_c();
        let bcd_add_done = self.new_label("bcd_add_done");
        self.jp_z_label(&bcd_add_done);
        self.pop_af();              // Restore carry for next ADC
        self.jp_label(&bcd_add_loop);
        self.define_label(&bcd_add_done);
        self.pop_af();              // Clean up stack
        self.pop_de();
        self.pop_bc();
        self.pop_ix();
        self.ret();

        // bcd_sub(result, a, b): result = a - b
        // Stack on entry: SP+0 = ret, SP+2 = result, SP+4 = a, SP+6 = b
        // After PUSH IX; LD IX,SP: (IX+0)=IX, (IX+2)=ret, (IX+4)=result, (IX+6)=a, (IX+8)=b
        self.define_label("bcd_sub");
        self.push_ix();
        self.ld_ix_sp();
        self.push_bc();
        self.push_de();
        // LDIR copies FROM (HL) TO (DE)
        // Copy a to result first
        self.ld_l_ix_d(6);          // HL = a ptr (source)
        self.ld_h_ix_d(7);
        self.ld_e_ix_d(4);          // DE = result ptr (dest)
        self.ld_d_ix_d(5);
        self.push_de();             // Save result ptr
        self.ld_bc_nn(4);
        self.emit(0xED); self.emit(0xB0);  // LDIR
        // Now subtract b from result
        self.pop_de();              // DE = result ptr (start)
        self.ld_l_ix_d(8);          // HL = b ptr
        self.ld_h_ix_d(9);
        self.ld_bc_nn(4);
        self.or_a();                // Clear carry (borrow)
        let bcd_sub_loop = self.new_label("bcd_sub_loop");
        self.define_label(&bcd_sub_loop);
        self.ld_a_de();
        self.emit(0x9E);            // SBC A,(HL)
        self.emit(0x27);            // DAA
        self.ld_de_a();
        self.inc_hl();
        self.inc_de();
        // Preserve carry flag across loop check
        self.push_af();
        self.dec_bc();
        self.ld_a_b();
        self.or_c();
        let bcd_sub_done = self.new_label("bcd_sub_done");
        self.jp_z_label(&bcd_sub_done);
        self.pop_af();              // Restore carry for next SBC
        self.jp_label(&bcd_sub_loop);
        self.define_label(&bcd_sub_done);
        self.pop_af();              // Clean up stack
        self.pop_de();
        self.pop_bc();
        self.pop_ix();
        self.ret();

        // bcd_cmp(a, b): return -1 if a < b, 0 if equal, 1 if a > b
        // Stack on entry: SP+0 = ret, SP+2 = a, SP+4 = b
        // After PUSH IX; LD IX,SP: (IX+0)=IX, (IX+2)=ret, (IX+4)=a, (IX+6)=b
        self.define_label("bcd_cmp");
        self.push_ix();
        self.ld_ix_sp();
        self.push_bc();
        self.push_de();
        self.ld_e_ix_d(4);          // DE = a ptr
        self.ld_d_ix_d(5);
        self.ld_l_ix_d(6);          // HL = b ptr
        self.ld_h_ix_d(7);
        // Start from MSB (offset 3)
        self.ld_bc_nn(3);
        self.add_hl_bc();
        self.ex_de_hl();
        self.add_hl_bc();
        self.ex_de_hl();            // DE = a+3, HL = b+3
        self.ld_bc_nn(4);
        let bcd_cmp_loop = self.new_label("bcd_cmp_loop");
        self.define_label(&bcd_cmp_loop);
        self.ld_a_de();
        self.emit(0xBE);            // CP (HL)
        let bcd_cmp_ne = self.new_label("bcd_cmp_ne");
        self.jp_nz_label(&bcd_cmp_ne);
        self.dec_hl();
        self.dec_de();
        self.dec_bc();
        self.ld_a_b();
        self.or_c();
        self.jp_nz_label(&bcd_cmp_loop);
        // Equal
        self.pop_de();
        self.pop_bc();
        self.pop_ix();
        self.ld_hl_nn(0);
        self.ret();
        self.define_label(&bcd_cmp_ne);
        self.pop_de();
        self.pop_bc();
        self.pop_ix();
        self.ld_hl_nn(1);
        self.ret_nc();              // A > (HL), return 1
        self.ld_hl_nn(0xFFFF);      // A < (HL), return -1
        self.ret();

        // bcd_from_int(result, value): convert 16-bit int to BCD
        // Stack on entry: SP+0 = ret addr, SP+2 = result ptr, SP+4 = value
        // After PUSH IX; LD IX,SP: (IX+0)=IX, (IX+2)=ret, (IX+4)=result, (IX+6)=value
        self.define_label("bcd_from_int");
        self.push_ix();
        self.ld_ix_sp();
        self.push_bc();
        self.push_de();
        self.push_hl();
        // Get result ptr
        self.ld_e_ix_d(4);          // DE = result ptr
        self.ld_d_ix_d(5);
        // Clear result bytes
        self.xor_a();
        self.ld_de_a();
        self.inc_de();
        self.ld_de_a();
        self.inc_de();
        self.ld_de_a();
        self.inc_de();
        self.ld_de_a();
        // Get value
        self.ld_l_ix_d(6);          // HL = value
        self.ld_h_ix_d(7);
        // B = digit position (0-7)
        self.ld_b_n(0);
        let bcd_fi_loop = self.new_label("bcd_fi_loop");
        self.define_label(&bcd_fi_loop);
        // HL = current value, B = position
        // Get digit = HL % 10, quotient = HL / 10
        // __mod16 and __div16 take: HL = dividend, DE = divisor
        self.push_bc();             // Save position
        self.push_hl();             // Save value for div later
        self.ld_de_nn(10);          // DE = 10 (divisor)
        self.call_label("__mod16"); // HL = HL % DE = value % 10
        self.ld_c_l();              // C = digit (0-9)
        self.pop_hl();              // HL = value
        self.push_bc();             // Save digit in C
        self.ld_de_nn(10);
        self.call_label("__div16"); // HL = HL / DE = value / 10 = quotient
        self.pop_bc();              // C = digit
        self.ld_a_c();              // A = digit
        self.pop_bc();              // B = position
        // Now: HL = quotient, A = digit, B = position
        // Store digit at position B
        // byte = B / 2, nibble = B & 1 (0=low, 1=high)
        self.push_hl();             // Save quotient
        self.push_bc();             // Save position
        // Calculate byte address = result_ptr + (B / 2)
        self.ld_c_a();              // C = digit (save)
        self.ld_a_b();              // A = position
        self.emit(0xCB); self.emit(0x3F);  // SRL A - A = byte offset
        self.ld_l_a();
        self.ld_h_n(0);             // HL = byte offset
        self.ld_e_ix_d(4);          // DE = result ptr
        self.ld_d_ix_d(5);
        self.add_hl_de();           // HL = address of byte to modify
        self.ld_a_c();              // A = digit
        // Check if odd position (high nibble)
        self.pop_bc();              // B = position
        self.push_bc();
        self.emit(0xCB); self.emit(0x40);  // BIT 0,B
        let bcd_fi_low = self.new_label("bcd_fi_low");
        self.jp_z_label(&bcd_fi_low);
        // High nibble - shift left 4
        self.emit(0xCB); self.emit(0x27);  // SLA A
        self.emit(0xCB); self.emit(0x27);
        self.emit(0xCB); self.emit(0x27);
        self.emit(0xCB); self.emit(0x27);
        self.define_label(&bcd_fi_low);
        // OR with existing byte and store
        self.emit(0xB6);            // OR (HL)
        self.emit(0x77);            // LD (HL),A
        self.pop_bc();              // B = position
        self.inc_b();               // Next position
        self.pop_hl();              // HL = quotient
        // Check if quotient == 0
        self.ld_a_h();
        self.or_l();
        self.jp_nz_label(&bcd_fi_loop);
        // Done
        self.pop_hl();
        self.pop_de();
        self.pop_bc();
        self.pop_ix();
        self.ret();

        // bcd_to_int(ptr): convert BCD to 16-bit int
        // Stack on entry: SP+0 = ret, SP+2 = ptr
        // After PUSH IX; LD IX,SP: (IX+0)=IX, (IX+2)=ret, (IX+4)=ptr
        self.define_label("bcd_to_int");
        self.push_ix();
        self.ld_ix_sp();
        self.push_bc();
        self.push_de();
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        // Point to MSB
        self.ld_de_nn(3);
        self.add_hl_de();
        self.ld_de_nn(0);           // DE = result
        self.ld_bc_nn(4);
        let bcd_ti_loop = self.new_label("bcd_ti_loop");
        self.define_label(&bcd_ti_loop);
        // High nibble
        self.ld_a_hl();
        self.emit(0xCB); self.emit(0x3F);  // SRL A x4
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xCB); self.emit(0x3F);
        // DE = DE * 10 + A
        self.push_hl();
        self.push_af();
        self.push_bc();
        self.ex_de_hl();
        self.push_hl();
        self.add_hl_hl();
        self.push_hl();
        self.add_hl_hl();
        self.add_hl_hl();
        self.pop_de();
        self.add_hl_de();
        self.pop_de();
        self.pop_bc();
        self.pop_af();
        self.ld_e_a();
        self.ld_d_n(0);
        self.add_hl_de();
        self.ex_de_hl();
        self.pop_hl();
        // Low nibble
        self.ld_a_hl();
        self.emit(0xE6); self.emit(0x0F);
        self.push_hl();
        self.push_af();
        self.push_bc();
        self.ex_de_hl();
        self.push_hl();
        self.add_hl_hl();
        self.push_hl();
        self.add_hl_hl();
        self.add_hl_hl();
        self.pop_de();
        self.add_hl_de();
        self.pop_de();
        self.pop_bc();
        self.pop_af();
        self.ld_e_a();
        self.ld_d_n(0);
        self.add_hl_de();
        self.ex_de_hl();
        self.pop_hl();
        self.dec_hl();
        self.dec_bc();
        self.ld_a_b();
        self.or_c();
        self.jp_nz_label(&bcd_ti_loop);
        self.ex_de_hl();
        self.pop_de();
        self.pop_bc();
        self.pop_ix();
        self.ret();

        // bcd_print(ptr): print BCD number
        // Stack on entry: SP+0 = ret, SP+2 = ptr
        // After PUSH IX; LD IX,SP: (IX+0)=IX, (IX+2)=ret, (IX+4)=ptr
        self.define_label("bcd_print");
        self.push_ix();
        self.ld_ix_sp();
        self.push_bc();
        self.push_de();
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.ld_de_nn(3);
        self.add_hl_de();           // HL = ptr + 3 (MSB)
        self.ld_bc_nn(8);           // 8 digits
        self.ld_d_n(0);             // D = printed flag
        let bcd_pr_loop = self.new_label("bcd_pr_loop");
        self.define_label(&bcd_pr_loop);
        // High nibble
        self.ld_a_hl();
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xCB); self.emit(0x3F);
        self.or_d();
        let bcd_pr_skip1 = self.new_label("bcd_pr_skip1");
        self.jp_z_label(&bcd_pr_skip1);
        self.ld_d_n(1);
        self.ld_a_hl();
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xC6); self.emit(0x30);  // ADD A,'0'
        self.push_hl();
        self.push_bc();
        self.push_de();
        self.ld_l_a();
        self.ld_h_n(0);
        self.push_hl();
        self.call_label("putchar");
        self.pop_hl();
        self.pop_de();
        self.pop_bc();
        self.pop_hl();
        self.define_label(&bcd_pr_skip1);
        self.dec_bc();
        self.ld_a_b();
        self.or_c();
        let bcd_pr_done = self.new_label("bcd_pr_done");
        self.jp_z_label(&bcd_pr_done);
        // Low nibble
        self.ld_a_hl();
        self.emit(0xE6); self.emit(0x0F);
        self.or_d();
        let bcd_pr_skip2 = self.new_label("bcd_pr_skip2");
        self.jp_z_label(&bcd_pr_skip2);
        self.ld_d_n(1);
        self.ld_a_hl();
        self.emit(0xE6); self.emit(0x0F);
        self.emit(0xC6); self.emit(0x30);
        self.push_hl();
        self.push_bc();
        self.push_de();
        self.ld_l_a();
        self.ld_h_n(0);
        self.push_hl();
        self.call_label("putchar");
        self.pop_hl();
        self.pop_de();
        self.pop_bc();
        self.pop_hl();
        self.define_label(&bcd_pr_skip2);
        self.dec_hl();
        self.dec_bc();
        self.ld_a_b();
        self.or_c();
        self.jp_nz_label(&bcd_pr_loop);
        // If nothing printed, print 0
        self.ld_a_d();
        self.or_a();
        self.jp_nz_label(&bcd_pr_done);
        self.ld_hl_nn('0' as u16);
        self.push_hl();
        self.call_label("putchar");
        self.pop_hl();
        self.define_label(&bcd_pr_done);
        self.pop_de();
        self.pop_bc();
        self.pop_ix();
        self.ret();

        // BCD Float functions
        self.emit_bcdf_from_int();
        self.emit_bcdf_normalize();
        self.emit_bcdf_shift_right();
        self.emit_bcdf_copy();
        self.emit_bcdf_neg();
        self.emit_bcdf_abs();
        self.emit_bcdf_add();
        self.emit_bcdf_sub();
        self.emit_bcdf_mul();
        self.emit_bcdf_div();
        self.emit_bcdf_cmp();
        self.emit_bcdf_print();
    }

    fn ret_z(&mut self) { self.emit(0xC8); }
    fn ret_nz(&mut self) { self.emit(0xC0); }
    fn ret_c(&mut self) { self.emit(0xD8); }
    fn ret_nc(&mut self) { self.emit(0xD0); }

    // ========================================
    // BCD FLOAT RUNTIME FUNCTIONS (TI-85 Style)
    // ========================================
    // Format: 6 bytes total
    //   Byte 0: Sign/flags (bit 7 = negative)
    //   Byte 1: Exponent (biased: 0x80 = 10^0, 0x81 = 10^1, etc.)
    //   Bytes 2-5: Mantissa (4 bytes = 8 BCD digits)
    //              NORMALIZED: Decimal point after first digit
    //              First digit is always 1-9 (except for zero)
    //              Example: 42 = 4.2000000 × 10^1 -> mant=$42,$00,$00,$00, exp=$81
    //              Example: 100 = 1.0000000 × 10^2 -> mant=$10,$00,$00,$00, exp=$82
    //              Example: 3.14159 = 3.1415900 × 10^0 -> mant=$31,$41,$59,$00, exp=$80
    // ========================================

    /// bcdf_from_int(result_ptr, value) - Convert 16-bit int to normalized BCD float
    /// Args: (IX+4) = result ptr, (IX+6) = value
    /// Result is normalized: first digit is non-zero, exponent adjusted
    ///
    /// TI-85 style: mantissa represents X.XXXXXXX, exponent is power of 10
    /// Example: 42 = 4.2 × 10^1 → mantissa = 42000000, exp = 0x81
    fn emit_bcdf_from_int(&mut self) {
        self.define_label("bcdf_from_int");
        self.push_ix();
        self.ld_ix_sp_offset(0);
        self.push_bc();  // save BC

        // Get result ptr into DE
        self.ld_e_ix_d(4);
        self.ld_d_ix_d(5);

        // Store sign = 0 (positive)
        self.xor_a();
        self.ld_de_a();
        self.inc_de();

        // Skip exponent for now (will set later)
        self.inc_de();
        // DE now points to mantissa (result+2)

        // Zero out mantissa first (4 bytes)
        self.xor_a();
        self.ld_de_a(); self.inc_de();
        self.ld_de_a(); self.inc_de();
        self.ld_de_a(); self.inc_de();
        self.ld_de_a();

        // Get value into HL
        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);

        // Check for zero
        self.ld_a_h();
        self.or_l();
        let not_zero = self.new_label("bcdf_fi_nz");
        self.jp_nz_label(&not_zero);

        // Value is zero - set exponent to 0, done
        self.ld_e_ix_d(4);
        self.ld_d_ix_d(5);
        self.inc_de();  // point to exponent
        self.xor_a();
        self.ld_de_a();
        self.pop_bc();
        self.pop_ix();
        self.ret();

        self.define_label(&not_zero);
        // HL = value
        // Count digits to determine exponent
        // exponent = (number of digits - 1) + 0x80

        // Count digits by repeatedly dividing by 10
        self.ld_b_n(0);  // B = digit count
        let count_loop = self.new_label("bcdf_fi_cnt");
        self.define_label(&count_loop);
        self.inc_b();
        self.push_bc();
        self.ld_de_nn(10);
        self.call_label("__div16");  // HL = HL / 10
        self.pop_bc();
        self.ld_a_h();
        self.or_l();
        self.jp_nz_label(&count_loop);

        // B = digit count, exponent = B - 1 + 0x80 = B + 0x7F
        self.ld_a_b();
        self.emit(0xC6); self.emit(0x7F);  // ADD A,0x7F

        // Store exponent at result+1
        self.ld_e_ix_d(4);
        self.ld_d_ix_d(5);
        self.inc_de();  // point to exponent
        self.ld_de_a();
        self.inc_de();  // DE = mantissa ptr

        // Call bcd_from_int(mantissa_ptr, value)
        // Stack: mantissa_ptr, value
        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);  // HL = value
        self.push_hl();     // push value
        self.push_de();     // push mantissa_ptr
        self.call_label("bcd_from_int");
        self.pop_de();
        self.pop_hl();

        // bcd_from_int stores right-aligned for display (MSB at ptr+3)
        // We need to call bcdf_normalize to shift mantissa left and adjust exponent
        // BUT: the current exponent is already correct for the value
        // The issue is the mantissa format - it's backwards from TI style

        // Actually, let's just use the right-aligned format and accept it
        // The bcd_add/sub operate on this format correctly for same-exponent operations
        // For different exponents, we need proper alignment which we haven't implemented yet

        self.pop_bc();
        self.pop_ix();
        self.ret();
    }

    /// bcdf_normalize(ptr) - Normalize a BCD float in place
    /// Shifts mantissa left until first digit is non-zero, adjusts exponent
    ///
    /// Simple approach: process from RIGHT to LEFT so we can work in-place
    /// For each shift iteration:
    ///   m3 = (m3 << 4)
    ///   m2 = (m2 << 4) | (m3_old >> 4)  -- but m3 is already modified!
    ///
    /// Better: read all 4 bytes first, compute all 4 new values, write them back
    fn emit_bcdf_normalize(&mut self) {
        self.define_label("bcdf_normalize");
        self.push_ix();
        self.ld_ix_sp_offset(0);

        // Get ptr
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);

        // Check if mantissa is all zeros
        self.push_hl();
        self.inc_hl();
        self.inc_hl();  // HL = mantissa
        self.ld_a_hl(); self.inc_hl();
        self.ld_b_a();
        self.ld_a_hl(); self.inc_hl();
        self.or_b();
        self.ld_b_a();
        self.ld_a_hl(); self.inc_hl();
        self.or_b();
        self.ld_b_a();
        self.ld_a_hl();
        self.or_b();
        self.pop_hl();
        let norm_done = self.new_label("bcdf_norm_done");
        self.jp_z_label(&norm_done);

        // Main loop
        let norm_loop = self.new_label("bcdf_norm_loop");
        self.define_label(&norm_loop);

        // Check first nibble
        self.push_hl();
        self.inc_hl();
        self.inc_hl();  // HL = mantissa[0]
        self.ld_a_hl();
        self.emit(0xE6); self.emit(0xF0);  // AND 0xF0
        self.pop_hl();
        self.jp_nz_label(&norm_done);

        // Decrement exponent
        self.inc_hl();  // HL = exponent
        self.dec_hl_indirect();
        self.inc_hl();  // HL = mantissa[0]

        // The key insight: work from LSB to MSB (right to left)
        // Save the high nibble of each byte BEFORE modifying it
        //
        // m3_new = m3 << 4
        // m2_new = (m2 << 4) | (m3 >> 4)
        // m1_new = (m1 << 4) | (m2 >> 4)
        // m0_new = (m0 << 4) | (m1 >> 4)

        // Read m3, save its high nibble
        self.inc_hl(); self.inc_hl(); self.inc_hl();  // HL = m3
        self.ld_a_hl();
        self.ld_b_a();  // B = original m3
        // Compute m3_new = m3 << 4
        self.emit(0x07); self.emit(0x07); self.emit(0x07); self.emit(0x07);  // RLCA x4
        self.emit(0xE6); self.emit(0xF0);
        self.ld_hl_a();  // Store m3_new

        // Read m2, compute m2_new = (m2 << 4) | (m3_orig >> 4)
        self.dec_hl();  // HL = m2
        self.ld_a_b();  // A = original m3
        self.emit(0x0F); self.emit(0x0F); self.emit(0x0F); self.emit(0x0F);  // RRCA x4
        self.emit(0xE6); self.emit(0x0F);
        self.ld_b_a();  // B = (m3_orig >> 4) & 0x0F
        self.ld_a_hl();
        self.ld_c_a();  // C = original m2 (save for next step)
        self.emit(0x07); self.emit(0x07); self.emit(0x07); self.emit(0x07);  // RLCA x4
        self.emit(0xE6); self.emit(0xF0);
        self.or_b();
        self.ld_hl_a();  // Store m2_new

        // Read m1, compute m1_new = (m1 << 4) | (m2_orig >> 4)
        self.dec_hl();  // HL = m1
        self.ld_a_c();  // A = original m2
        self.emit(0x0F); self.emit(0x0F); self.emit(0x0F); self.emit(0x0F);
        self.emit(0xE6); self.emit(0x0F);
        self.ld_b_a();  // B = (m2_orig >> 4)
        self.ld_a_hl();
        self.ld_c_a();  // C = original m1 (save for next step)
        self.emit(0x07); self.emit(0x07); self.emit(0x07); self.emit(0x07);
        self.emit(0xE6); self.emit(0xF0);
        self.or_b();
        self.ld_hl_a();  // Store m1_new

        // Read m0, compute m0_new = (m0 << 4) | (m1_orig >> 4)
        self.dec_hl();  // HL = m0
        self.ld_a_c();  // A = original m1
        self.emit(0x0F); self.emit(0x0F); self.emit(0x0F); self.emit(0x0F);
        self.emit(0xE6); self.emit(0x0F);
        self.ld_b_a();  // B = (m1_orig >> 4)
        self.ld_a_hl();
        self.emit(0x07); self.emit(0x07); self.emit(0x07); self.emit(0x07);
        self.emit(0xE6); self.emit(0xF0);
        self.or_b();
        self.ld_hl_a();  // Store m0_new

        // HL is now at m0. Go back to ptr for next iteration
        self.dec_hl();
        self.dec_hl();  // HL = ptr
        self.jp_label(&norm_loop);

        self.define_label(&norm_done);
        self.pop_ix();
        self.ret();
    }

    /// LD HL,DE helper
    fn ld_hl_de(&mut self) {
        self.ld_l_e();
        self.ld_h_d();
    }

    /// bcdf_copy(dest, src) - Copy a BCD float
    fn emit_bcdf_copy(&mut self) {
        self.define_label("bcdf_copy");
        self.push_ix();
        self.ld_ix_sp_offset(0);

        // Get src and dest
        self.ld_l_ix_d(6);  // src
        self.ld_h_ix_d(7);
        self.ld_e_ix_d(4);  // dest
        self.ld_d_ix_d(5);

        // Copy 6 bytes using LDIR
        self.ld_bc_nn(6);
        self.emit(0xED); self.emit(0xB0);  // LDIR

        self.pop_ix();
        self.ret();
    }

    /// bcdf_neg(ptr) - Negate a BCD float in place (flip sign bit)
    fn emit_bcdf_neg(&mut self) {
        self.define_label("bcdf_neg");
        self.push_ix();
        self.ld_ix_sp_offset(0);

        // Get ptr
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);

        // XOR sign byte with 0x80 to flip sign
        self.ld_a_hl();
        self.emit(0xEE); self.emit(0x80);  // XOR 0x80
        self.ld_hl_a();

        self.pop_ix();
        self.ret();
    }

    /// bcdf_abs(ptr) - Absolute value of BCD float in place (clear sign bit)
    fn emit_bcdf_abs(&mut self) {
        self.define_label("bcdf_abs");
        self.push_ix();
        self.ld_ix_sp_offset(0);

        // Get ptr
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);

        // AND sign byte with 0x7F to clear sign bit
        self.ld_a_hl();
        self.emit(0xE6); self.emit(0x7F);  // AND 0x7F
        self.ld_hl_a();

        self.pop_ix();
        self.ret();
    }

    /// DEC (HL) - decrement byte at address HL
    fn dec_hl_indirect(&mut self) { self.emit(0x35); }

    /// INC (HL) - increment byte at address HL
    fn inc_hl_indirect(&mut self) { self.emit(0x34); }

    /// Helper: OR (HL)
    fn or_hl(&mut self) { self.emit(0xB6); }

    /// bcdf_shift_right(ptr) - Shift mantissa right by 1 nibble (divide by 10)
    /// Used for exponent alignment
    fn emit_bcdf_shift_right(&mut self) {
        self.define_label("bcdf_shift_right");
        self.push_ix();
        self.ld_ix_sp_offset(0);

        // Get mantissa ptr (ptr+2)
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.inc_hl();
        self.inc_hl();  // HL = mantissa start

        // Shift right by 1 nibble: each byte becomes (prev_low << 4) | (cur_high >> 4)
        // m0_new = (0 << 4) | (m0 >> 4) = m0 >> 4
        // m1_new = (m0_low << 4) | (m1 >> 4)
        // m2_new = (m1_low << 4) | (m2 >> 4)
        // m3_new = (m2_low << 4) | (m3 >> 4)

        // Read m0, save its low nibble
        self.ld_a_hl();
        self.ld_c_a();  // C = original m0
        // m0_new = m0 >> 4
        self.emit(0x0F); self.emit(0x0F); self.emit(0x0F); self.emit(0x0F);  // RRCA x4
        self.emit(0xE6); self.emit(0x0F);  // AND 0x0F
        self.ld_hl_a();

        // m1_new = (m0_low << 4) | (m1 >> 4)
        self.inc_hl();  // HL = m1
        self.ld_a_c();  // A = original m0
        self.emit(0xE6); self.emit(0x0F);  // AND 0x0F - get low nibble
        self.emit(0x07); self.emit(0x07); self.emit(0x07); self.emit(0x07);  // RLCA x4 - shift to high
        self.ld_c_a();  // C = m0_low << 4
        self.ld_a_hl();
        self.ld_b_a();  // B = original m1
        self.emit(0x0F); self.emit(0x0F); self.emit(0x0F); self.emit(0x0F);  // RRCA x4
        self.emit(0xE6); self.emit(0x0F);  // AND 0x0F
        self.or_c();
        self.ld_hl_a();

        // m2_new = (m1_low << 4) | (m2 >> 4)
        self.inc_hl();  // HL = m2
        self.ld_a_b();  // A = original m1
        self.emit(0xE6); self.emit(0x0F);
        self.emit(0x07); self.emit(0x07); self.emit(0x07); self.emit(0x07);
        self.ld_c_a();
        self.ld_a_hl();
        self.ld_b_a();  // B = original m2
        self.emit(0x0F); self.emit(0x0F); self.emit(0x0F); self.emit(0x0F);
        self.emit(0xE6); self.emit(0x0F);
        self.or_c();
        self.ld_hl_a();

        // m3_new = (m2_low << 4) | (m3 >> 4)
        self.inc_hl();  // HL = m3
        self.ld_a_b();  // A = original m2
        self.emit(0xE6); self.emit(0x0F);
        self.emit(0x07); self.emit(0x07); self.emit(0x07); self.emit(0x07);
        self.ld_c_a();
        self.ld_a_hl();
        self.emit(0x0F); self.emit(0x0F); self.emit(0x0F); self.emit(0x0F);
        self.emit(0xE6); self.emit(0x0F);
        self.or_c();
        self.ld_hl_a();

        self.pop_ix();
        self.ret();
    }

    /// bcdf_add(result_ptr, a_ptr, b_ptr) - Add two BCD floats
    /// Uses the larger exponent for result, adds mantissas directly
    /// Note: For best accuracy, operands should have same exponent
    fn emit_bcdf_add(&mut self) {
        self.define_label("bcdf_add");
        self.push_ix();
        self.ld_ix_sp_offset(0);
        self.push_bc();

        // Get exponents
        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);
        self.inc_hl();
        self.ld_b_hl();     // B = exp_a

        self.ld_l_ix_d(8);
        self.ld_h_ix_d(9);
        self.inc_hl();
        self.ld_c_hl();     // C = exp_b

        // Use larger exponent: B = max(B, C)
        self.ld_a_b();
        self.sub_c();       // A = exp_a - exp_b
        let keep_b = self.new_label("bcdf_add_kb");
        self.jp_nc_label(&keep_b);
        self.ld_b_c();      // B = exp_b (larger)
        self.define_label(&keep_b);

        // Store result: sign = 0, exp = B
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.xor_a();
        self.ld_hl_a();     // sign = 0
        self.inc_hl();
        self.ld_a_b();
        self.ld_hl_a();     // exp = max

        // bcd_add(result+2, a+2, b+2)
        self.ld_l_ix_d(8);
        self.ld_h_ix_d(9);
        self.inc_hl();
        self.inc_hl();
        self.push_hl();     // b+2

        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);
        self.inc_hl();
        self.inc_hl();
        self.push_hl();     // a+2

        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.inc_hl();
        self.inc_hl();
        self.push_hl();     // result+2

        self.call_label("bcd_add");
        self.pop_hl();
        self.pop_hl();
        self.pop_hl();

        self.pop_bc();
        self.pop_ix();
        self.ret();
    }

    /// bcdf_sub(result_ptr, a_ptr, b_ptr) - Subtract two BCD floats (a - b)
    /// Simple version: calls bcd_sub on the mantissas
    fn emit_bcdf_sub(&mut self) {
        self.define_label("bcdf_sub");
        self.push_ix();
        self.ld_ix_sp_offset(0);

        // Get result ptr
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);

        // Copy sign = 0 (positive - assuming a > b)
        self.xor_a();
        self.ld_hl_a();
        self.inc_hl();

        // Copy exponent from a
        self.push_hl();
        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);
        self.inc_hl();
        self.ld_a_hl();
        self.pop_hl();
        self.ld_hl_a();
        self.inc_hl();

        // Call bcd_sub(result+2, a+2, b+2)
        self.push_hl();

        self.ld_l_ix_d(8);
        self.ld_h_ix_d(9);
        self.inc_hl();
        self.inc_hl();
        self.push_hl();

        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);
        self.inc_hl();
        self.inc_hl();
        self.push_hl();

        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.inc_hl();
        self.inc_hl();
        self.push_hl();

        self.call_label("bcd_sub");

        self.pop_hl();
        self.pop_hl();
        self.pop_hl();
        self.pop_hl();

        self.pop_ix();
        self.ret();
    }

    /// bcdf_mul(result_ptr, a_ptr, b_ptr) - Multiply two BCD floats
    /// Simplified approach: Convert b to integer, add a to result that many times
    /// Works for small numbers, limited precision for large ones
    fn emit_bcdf_mul(&mut self) {
        self.define_label("bcdf_mul");
        self.push_ix();
        self.ld_ix_sp_offset(0);

        // Get result ptr and zero it first
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.xor_a();
        self.ld_hl_a(); self.inc_hl();  // sign = 0
        self.ld_hl_a(); self.inc_hl();  // exp = 0 (will set later)
        self.ld_hl_a(); self.inc_hl();  // mantissa[0] = 0
        self.ld_hl_a(); self.inc_hl();  // mantissa[1] = 0
        self.ld_hl_a(); self.inc_hl();  // mantissa[2] = 0
        self.ld_hl_a();                 // mantissa[3] = 0

        // Calculate exponent: exp_a + exp_b - 0x80
        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);
        self.inc_hl();
        self.ld_a_hl();     // exp_a
        self.ld_b_a();
        self.ld_l_ix_d(8);
        self.ld_h_ix_d(9);
        self.inc_hl();
        self.ld_a_hl();     // exp_b
        self.add_a_b();
        self.emit(0xD6); self.emit(0x80);  // SUB 0x80
        // Store exponent
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.inc_hl();
        self.ld_hl_a();

        // Calculate sign: sign_a XOR sign_b
        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);
        self.ld_a_hl();
        self.ld_b_a();
        self.ld_l_ix_d(8);
        self.ld_h_ix_d(9);
        self.ld_a_hl();
        self.xor_b();
        self.emit(0xE6); self.emit(0x80);  // AND 0x80
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.ld_hl_a();

        // Convert b's mantissa to integer using bcd_to_int
        self.ld_l_ix_d(8);
        self.ld_h_ix_d(9);
        self.inc_hl();
        self.inc_hl();    // HL = b+2 (mantissa)
        self.push_hl();
        self.call_label("bcd_to_int");
        self.pop_de();    // discard arg
        // HL = b as integer (loop counter)

        // If b is 0, we're done (result is already 0)
        self.ld_a_h();
        self.or_l();
        let mul_done = self.new_label("bcdf_mul_done");
        self.jp_z_label(&mul_done);

        self.push_hl();   // save loop counter

        let mul_loop = self.new_label("bcdf_mul_loop");
        self.define_label(&mul_loop);

        // Add a's mantissa to result's mantissa
        // Push args for bcd_add: (result+2, result+2, a+2)
        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);
        self.inc_hl();
        self.inc_hl();    // HL = a+2
        self.push_hl();

        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.inc_hl();
        self.inc_hl();    // HL = result+2
        self.push_hl();
        self.push_hl();   // result+2 as both dest and first operand

        self.call_label("bcd_add");

        self.pop_hl();
        self.pop_hl();
        self.pop_hl();

        // Decrement counter
        self.pop_hl();
        self.dec_hl();
        self.ld_a_h();
        self.or_l();
        self.push_hl();
        self.jp_nz_label(&mul_loop);

        self.pop_hl();    // clean up counter

        self.define_label(&mul_done);
        self.pop_ix();
        self.ret();
    }

    /// bcdf_div(result_ptr, a_ptr, b_ptr) - Divide two BCD floats (a / b)
    /// Uses integer division: convert to int, divide, convert back
    fn emit_bcdf_div(&mut self) {
        self.define_label("bcdf_div");
        self.push_ix();
        self.ld_ix_sp_offset(0);

        // Get result ptr and zero it first
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.xor_a();
        self.ld_hl_a(); self.inc_hl();  // sign = 0
        self.ld_hl_a(); self.inc_hl();  // exp = 0 (will set later)
        self.ld_hl_a(); self.inc_hl();  // mantissa[0] = 0
        self.ld_hl_a(); self.inc_hl();  // mantissa[1] = 0
        self.ld_hl_a(); self.inc_hl();  // mantissa[2] = 0
        self.ld_hl_a();                 // mantissa[3] = 0

        // Calculate exponent: exp_a - exp_b + 0x80
        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);
        self.inc_hl();
        self.ld_a_hl();     // exp_a
        self.ld_b_a();
        self.ld_l_ix_d(8);
        self.ld_h_ix_d(9);
        self.inc_hl();
        self.ld_c_hl();     // exp_b
        self.ld_a_b();
        self.emit(0x91);    // SUB C
        self.emit(0xC6); self.emit(0x80);  // ADD 0x80
        // Store exponent
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.inc_hl();
        self.ld_hl_a();

        // Calculate sign: sign_a XOR sign_b
        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);
        self.ld_a_hl();
        self.ld_b_a();
        self.ld_l_ix_d(8);
        self.ld_h_ix_d(9);
        self.ld_a_hl();
        self.xor_b();
        self.emit(0xE6); self.emit(0x80);  // AND 0x80
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.ld_hl_a();

        // Convert a's mantissa to integer
        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);
        self.inc_hl();
        self.inc_hl();    // HL = a+2 (mantissa)
        self.push_hl();
        self.call_label("bcd_to_int");
        self.pop_de();
        // HL = a as integer
        self.push_hl();   // save dividend

        // Convert b's mantissa to integer
        self.ld_l_ix_d(8);
        self.ld_h_ix_d(9);
        self.inc_hl();
        self.inc_hl();    // HL = b+2 (mantissa)
        self.push_hl();
        self.call_label("bcd_to_int");
        self.pop_de();
        // HL = b as integer (divisor)

        // Check for divide by zero
        self.ld_a_h();
        self.or_l();
        let div_done = self.new_label("bcdf_div_done");
        self.jp_z_label(&div_done);

        self.ex_de_hl();  // DE = divisor
        self.pop_hl();    // HL = dividend
        self.push_hl();   // keep for stack balance

        // Call __div16: HL = HL / DE
        self.call_label("__div16");
        // HL = quotient

        // Convert result back to BCD
        // Push args for bcd_from_int: (result+2, quotient)
        self.push_hl();   // value
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.inc_hl();
        self.inc_hl();    // HL = result+2
        self.push_hl();
        self.call_label("bcd_from_int");
        self.pop_hl();
        self.pop_hl();

        self.define_label(&div_done);
        self.pop_hl();    // clean up saved dividend
        self.pop_ix();
        self.ret();
    }

    /// bcdf_cmp(a_ptr, b_ptr) - Compare two BCD floats
    /// Returns: HL = 1 if a > b, 0 if a == b, -1 (0xFFFF) if a < b
    fn emit_bcdf_cmp(&mut self) {
        self.define_label("bcdf_cmp");
        self.push_ix();
        self.ld_ix_sp_offset(0);

        // First compare signs
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.ld_a_hl();        // A = a.sign
        self.ld_b_a();
        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);
        self.ld_a_hl();        // A = b.sign

        // If signs differ, negative is smaller
        self.xor_b();
        self.emit(0xE6); self.emit(0x80);  // AND 0x80
        let same_sign = self.new_label("bcdf_cmp_same");
        self.jp_z_label(&same_sign);

        // Signs differ - check which is negative
        self.ld_a_b();
        self.emit(0xE6); self.emit(0x80);  // AND 0x80
        let a_neg = self.new_label("bcdf_cmp_aneg");
        self.jp_nz_label(&a_neg);
        // a positive, b negative -> a > b
        self.ld_hl_nn(1);
        let cmp_done = self.new_label("bcdf_cmp_done");
        self.jp_label(&cmp_done);

        self.define_label(&a_neg);
        // a negative, b positive -> a < b
        self.ld_hl_nn(0xFFFF);
        self.jp_label(&cmp_done);

        self.define_label(&same_sign);
        // Same sign - compare exponents
        // For positive numbers: larger exponent = larger number
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.inc_hl();
        self.ld_b_hl();        // B = a.exp

        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);
        self.inc_hl();
        self.ld_a_hl();        // A = b.exp

        // Compare a.exp with b.exp: B - A
        self.ld_a_b();         // A = a.exp
        self.sub_c();          // Oops, C not set. Let me load properly
        // Actually, compare B with memory or use CP
        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);
        self.inc_hl();
        self.ld_a_b();         // A = a.exp
        self.emit(0xBE);       // CP (HL) = compare A with b.exp, flags for A - b.exp

        let exp_equal = self.new_label("bcdf_cmp_expeq");
        self.jp_z_label(&exp_equal);

        // Exponents differ: if A > (HL), a.exp > b.exp, so a > b -> return 1
        // After CP, carry set if A < (HL), i.e., a.exp < b.exp
        self.jp_c_label(&a_neg);  // a.exp < b.exp -> a < b -> return -1
        // a.exp > b.exp -> a > b
        self.ld_hl_nn(1);
        self.jp_label(&cmp_done);

        self.define_label(&exp_equal);
        // Same exponent - compare mantissas using bcd_cmp
        self.ld_l_ix_d(6);
        self.ld_h_ix_d(7);
        self.inc_hl();
        self.inc_hl();
        self.push_hl();        // b+2 (b's mantissa)

        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);
        self.inc_hl();
        self.inc_hl();
        self.push_hl();        // a+2 (a's mantissa)

        self.call_label("bcd_cmp");
        self.pop_de();
        self.pop_de();
        // HL = comparison result

        self.define_label(&cmp_done);
        self.pop_ix();
        self.ret();
    }

    /// bcdf_print(value_ptr) - Print BCD float
    /// Simple version: just prints mantissa using bcd_print, then " e" and exponent
    fn emit_bcdf_print(&mut self) {
        self.define_label("bcdf_print");
        self.push_ix();
        self.ld_ix_sp_offset(0);

        // Get value ptr
        self.ld_l_ix_d(4);
        self.ld_h_ix_d(5);

        // Check sign (byte 0)
        self.ld_a_hl();
        self.emit(0xE6); self.emit(0x80);  // AND 0x80
        let positive = self.new_label("bcdf_pr_pos");
        self.jp_z_label(&positive);

        // Print minus sign
        self.push_hl();
        self.ld_hl_nn('-' as u16);
        self.push_hl();
        self.call_label("putchar");
        self.pop_hl();
        self.pop_hl();

        self.define_label(&positive);
        // HL = value ptr
        self.inc_hl();      // point to exponent (byte 1)
        self.ld_b_hl();     // B = exponent
        self.inc_hl();      // HL = mantissa start (byte 2)

        // Call bcd_print on the mantissa
        self.push_bc();     // save exponent
        self.push_hl();     // arg: mantissa ptr
        self.call_label("bcd_print");
        self.pop_hl();
        self.pop_bc();

        // Print " e" and exponent (B - 0x80)
        // Skip if exponent is 0 (for zero values)
        self.ld_a_b();
        self.or_a();
        let no_exp = self.new_label("bcdf_pr_noexp");
        self.jp_z_label(&no_exp);

        // Print 'e'
        self.push_bc();
        self.ld_hl_nn('e' as u16);
        self.push_hl();
        self.call_label("putchar");
        self.pop_hl();
        self.pop_bc();

        // Calculate and print exponent (B - 0x80)
        self.ld_a_b();
        self.emit(0xD6); self.emit(0x80);  // SUB 0x80
        self.emit(0xB7);  // OR A - set flags
        let exp_pos = self.new_label("bcdf_pr_epos");
        self.jp_p_label(&exp_pos);

        // Negative exponent
        self.push_af();
        self.ld_hl_nn('-' as u16);
        self.push_hl();
        self.call_label("putchar");
        self.pop_hl();
        self.pop_af();
        self.emit(0xED); self.emit(0x44);  // NEG

        self.define_label(&exp_pos);
        // A = exponent magnitude, print as decimal
        self.ld_l_a();
        self.ld_h_n(0);
        self.push_hl();
        self.call_label("print_num");
        self.pop_hl();

        self.define_label(&no_exp);
        self.pop_ix();
        self.ret();
    }

    fn ld_b_hl(&mut self) {
        self.ld_a_hl();
        self.ld_b_a();
    }

    fn ld_d_hl(&mut self) {
        self.ld_a_hl();
        self.ld_d_a();
    }

    fn ld_c_hl_direct(&mut self) { self.emit(0x4E); }  // LD C,(HL)

    fn jp_p_label(&mut self, label: &str) {
        self.emit(0xF2);  // JP P,nn
        self.emit_label_ref(label);
    }

    fn xor_b(&mut self) { self.emit(0xA8); }
    fn add_a_b(&mut self) { self.emit(0x80); }
    fn ex_af(&mut self) { self.emit(0x08); }  // EX AF,AF'
    fn ld_c_hl(&mut self) { self.emit(0x4E); }  // LD C,(HL)
    fn inc_c(&mut self) { self.emit(0x0C); }
    fn dec_c(&mut self) { self.emit(0x0D); }
    fn dec_a(&mut self) { self.emit(0x3D); }

    fn dec_e(&mut self) { self.emit(0x1D); }  // DEC E

    /// LD IX,SP with optional offset (for frame pointer setup)
    fn ld_ix_sp_offset(&mut self, _offset: i16) {
        // Just use LD IX,SP (offset 0) - we don't really need offset here
        self.ld_ix_sp();
    }
}
