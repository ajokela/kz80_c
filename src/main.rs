//! kz80_c - A self-hosting C compiler for RetroShield Z80
//!
//! A minimal C compiler targeting the Z80 processor, inspired by
//! Small-C (Ron Cain, 1980). The goal is to eventually compile
//! itself on a Z80.

mod token;
mod lexer;
mod ast;
mod parser;
mod codegen;
mod preprocess;

use std::env;
use std::fs;
use std::io::{self, Read, Write};
use std::path::Path;
use std::process;

use lexer::Lexer;
use parser::Parser;
use codegen::CodeGen;
use preprocess::Preprocessor;

fn print_usage() {
    eprintln!("Usage: kz80_c [OPTIONS] <input.c>");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -o <file>     Output file (default: a.out)");
    eprintln!("  -S            Output hex dump of generated code");
    eprintln!("  -E            Preprocess only");
    eprintln!("  -I <dir>      Add include search path");
    eprintln!("  -D <name=val> Define preprocessor macro");
    eprintln!("  --tokens      Show tokens only");
    eprintln!("  --ast         Show AST only");
    eprintln!("  -h, --help    Show this help");
    eprintln!();
    eprintln!("If no input file is given, reads from stdin.");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut input_file: Option<String> = None;
    let mut output_file = "a.out".to_string();
    let mut show_tokens = false;
    let mut show_ast = false;
    let mut show_asm = false;
    let mut preprocess_only = false;
    let mut include_paths: Vec<String> = Vec::new();
    let mut defines: Vec<(String, String)> = Vec::new();

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: -o requires an argument");
                    process::exit(1);
                }
                output_file = args[i].clone();
            }
            "-I" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: -I requires an argument");
                    process::exit(1);
                }
                include_paths.push(args[i].clone());
            }
            "-D" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: -D requires an argument");
                    process::exit(1);
                }
                let def = &args[i];
                if let Some(eq_pos) = def.find('=') {
                    defines.push((def[..eq_pos].to_string(), def[eq_pos+1..].to_string()));
                } else {
                    defines.push((def.clone(), "1".to_string()));
                }
            }
            "-S" => show_asm = true,
            "-E" => preprocess_only = true,
            "--tokens" => show_tokens = true,
            "--ast" => show_ast = true,
            "-h" | "--help" => {
                print_usage();
                return;
            }
            arg if arg.starts_with("-I") => {
                include_paths.push(arg[2..].to_string());
            }
            arg if arg.starts_with("-D") => {
                let def = &arg[2..];
                if let Some(eq_pos) = def.find('=') {
                    defines.push((def[..eq_pos].to_string(), def[eq_pos+1..].to_string()));
                } else {
                    defines.push((def.to_string(), "1".to_string()));
                }
            }
            arg if arg.starts_with('-') => {
                eprintln!("Unknown option: {}", arg);
                print_usage();
                process::exit(1);
            }
            _ => {
                input_file = Some(args[i].clone());
            }
        }
        i += 1;
    }

    // Read input
    let filename = input_file.clone().unwrap_or_else(|| "<stdin>".to_string());
    let source = if let Some(ref path) = input_file {
        match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error reading {}: {}", path, e);
                process::exit(1);
            }
        }
    } else {
        let mut s = String::new();
        io::stdin().read_to_string(&mut s).expect("Failed to read stdin");
        s
    };

    // Preprocess
    let mut preprocessor = Preprocessor::new();
    for path in &include_paths {
        preprocessor.add_include_path(path);
    }
    // Add directory of input file to include path
    if let Some(ref path) = input_file {
        if let Some(parent) = Path::new(path).parent() {
            if let Some(dir) = parent.to_str() {
                if !dir.is_empty() {
                    preprocessor.add_include_path(dir);
                }
            }
        }
    }
    for (name, value) in &defines {
        preprocessor.define(name, value);
    }

    let processed = match preprocessor.process(&source, &filename) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Preprocessor error: {}", e);
            process::exit(1);
        }
    };

    if preprocess_only {
        print!("{}", processed);
        return;
    }

    // Tokenize
    let mut lexer = Lexer::new(&processed);
    let tokens = match lexer.tokenize() {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Lexer error at line {}, col {}: {}", e.line, e.col, e.message);
            process::exit(1);
        }
    };

    if show_tokens {
        for tok in &tokens {
            println!("{:?}", tok);
        }
        return;
    }

    // Parse
    let mut parser = Parser::new(tokens);
    let ast = match parser.parse() {
        Ok(a) => a,
        Err(e) => {
            eprintln!("Parse error: {}", e.message);
            process::exit(1);
        }
    };

    if show_ast {
        println!("{:#?}", ast);
        return;
    }

    // Generate code
    let mut codegen = CodeGen::new();
    let code = codegen.generate(&ast);

    if show_asm {
        // Hex dump
        for (i, chunk) in code.chunks(16).enumerate() {
            print!("{:04X}:", i * 16);
            for byte in chunk {
                print!(" {:02X}", byte);
            }
            println!();
        }
        return;
    }

    // Write binary
    let mut file = match fs::File::create(&output_file) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Error creating {}: {}", output_file, e);
            process::exit(1);
        }
    };

    if let Err(e) = file.write_all(&code) {
        eprintln!("Error writing {}: {}", output_file, e);
        process::exit(1);
    }

    println!("Wrote {} bytes to {}", code.len(), output_file);
}
