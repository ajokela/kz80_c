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
    fn ld_l_a(&mut self) { self.emit(0x6F); }
    fn ld_l_n(&mut self, n: u8) { self.emit(0x2E); self.emit(n); }
    fn ld_a_h(&mut self) { self.emit(0x7C); }
    fn ld_a_l(&mut self) { self.emit(0x7D); }
    fn ld_a_e(&mut self) { self.emit(0x7B); }
    fn ld_a_d(&mut self) { self.emit(0x7A); }
    fn ld_b_a(&mut self) { self.emit(0x47); }
    fn ld_c_a(&mut self) { self.emit(0x4F); }
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
    fn and_l(&mut self) { self.emit(0xA5); }
    fn or_l(&mut self) { self.emit(0xB5); }
    fn xor_l(&mut self) { self.emit(0xAD); }
    fn cp_l(&mut self) { self.emit(0xBD); }
    fn or_a(&mut self) { self.emit(0xB7); }
    fn xor_a(&mut self) { self.emit(0xAF); }

    fn inc_hl(&mut self) { self.emit(0x23); }
    fn dec_hl(&mut self) { self.emit(0x2B); }
    fn inc_de(&mut self) { self.emit(0x13); }
    fn dec_de(&mut self) { self.emit(0x1B); }

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
        // First pass: process globals to populate global_symbols
        for decl in &unit.decls {
            if let Decl::Global(global) = decl {
                self.gen_global(global);
            }
        }

        // Initialize global variables in RAM
        // We need to store initial values to RAM addresses
        let mut ram_addr = 0x2000u16;
        for (_, ty, init) in &self.globals.clone() {
            let size = ty.size();
            if let Some(val) = init {
                // LD HL, value
                self.ld_hl_nn(*val as u16);
                // LD (ram_addr), HL or LD (ram_addr), L for char
                if size == 1 {
                    self.ld_a_l();
                    self.emit(0x32);  // LD (nn),A
                    self.emit16(ram_addr);
                } else {
                    self.emit(0x22);  // LD (nn),HL
                    self.emit16(ram_addr);
                }
            }
            ram_addr += size as u16;
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

        // Global variables are now in RAM at addresses >= 0x2000
        // No need to emit them in the binary - RAM is writable

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

    fn gen_global(&mut self, global: &Global) {
        // Allocate RAM address for this global
        let addr = self.global_addr;
        let size = global.ty.size();
        self.global_addr += size as u16;

        // Record initial value for startup initialization
        let init_value = match &global.init {
            Some(Expr::IntLit(n)) => Some(*n),
            Some(Expr::CharLit(c)) => Some(*c as i32),
            _ => None,
        };
        self.globals.push((global.name.clone(), global.ty.clone(), init_value));

        // Store the absolute RAM address as the "label"
        // We'll handle this specially in emit_label_ref
        let addr_label = format!("__global_{:04x}", addr);
        self.labels.insert(addr_label.clone(), addr);

        // Add to global_symbols with the address label
        self.global_symbols.insert(global.name.clone(), Symbol::Global {
            label: addr_label,
            ty: global.ty.clone(),
        });
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
                self.symbols.insert(name.clone(), Symbol::Local {
                    offset: self.local_offset,
                    ty: ty.clone(),
                });

                // Adjust SP
                self.ld_hl_nn((-size) as u16);
                self.add_hl_sp();
                self.emit(0xF9);  // LD SP,HL

                // Initialize if needed
                if let Some(init_expr) = init {
                    self.gen_expr(init_expr);
                    // Store result
                    self.ld_ix_d_l(self.local_offset as i8);
                    if size > 1 {
                        self.ld_ix_d_h((self.local_offset + 1) as i8);
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

    fn gen_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntLit(n) => {
                self.ld_hl_nn(*n as u16);
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
                            // For arrays, return address (like a pointer)
                            if matches!(ty, Type::Array(_, _)) {
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
                            if matches!(ty, Type::Array(_, _)) {
                                // For arrays, load address
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
                        self.and_l();
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
                            UnOp::Deref => {
                                // Load 16-bit value from address in HL
                                // HL points to memory, load (HL) into HL
                                self.ex_de_hl();      // DE = address
                                self.ld_a_de();       // A = low byte
                                self.ld_l_a();        // L = low byte
                                self.inc_de();        // DE points to high byte
                                self.ld_a_de();       // A = high byte
                                self.ld_h_a();        // H = high byte
                                // Now HL = value at original address
                            }
                            UnOp::PreInc | UnOp::PostInc | UnOp::PreDec | UnOp::PostDec => {
                                // These need the address, handled specially
                            }
                            UnOp::AddrOf => unreachable!(),
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
                self.gen_expr(idx);
                self.push_hl();
                self.gen_expr(arr);
                self.pop_de();
                self.add_hl_de();  // For now, assume element size 1
                // Load byte from address and zero-extend to 16-bit
                self.ld_a_hl();
                self.ld_l_a();
                self.ld_h_n(0);  // Zero-extend: H = 0
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
                self.push_hl();  // Save value
                self.gen_expr(idx);
                self.push_hl();
                self.gen_expr(arr);
                self.pop_de();
                self.add_hl_de();  // HL = address of element
                self.pop_de();  // Value in DE
                self.ld_a_e();
                self.ld_hl_a();  // Store byte (char arrays)
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
                // Address of arr[idx] = arr + idx
                self.gen_expr(idx);
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
        // HL = HL << E
        self.ld_a_e();
        let loop_label = self.new_label("shl_loop");
        self.define_label(&loop_label);
        self.or_a();
        self.ret_z();
        self.add_hl_hl();
        self.dec_de();
        self.jp_label(&loop_label);

        self.define_label("__shr16");
        // HL = HL >> E
        self.ld_a_e();
        let loop_label = self.new_label("shr_loop");
        self.define_label(&loop_label);
        self.or_a();
        self.ret_z();
        self.emit(0xCB); self.emit(0x3C);  // SRL H
        self.emit(0xCB); self.emit(0x1D);  // RR L
        self.dec_de();
        self.jp_label(&loop_label);

        self.define_label("__land");
        self.ret();

        self.define_label("__lor");
        self.ret();
    }

    fn ret_z(&mut self) { self.emit(0xC8); }
    fn ret_nz(&mut self) { self.emit(0xC0); }
    fn ret_c(&mut self) { self.emit(0xD8); }
    fn ret_nc(&mut self) { self.emit(0xD0); }
}
