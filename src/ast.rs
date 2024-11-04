#![warn(clippy::pedantic)]
use crate::errors::SDKErr;
use macro_lib::ast_node;
use std::collections::HashMap;

fn parse_file(file_name: &str, content: &mut str) -> Result<syn::File, SDKErr> {
    if let Ok(ast) = syn::parse_file(content) {
        Ok(ast)
    } else {
        Err(SDKErr::AstParseError(file_name.to_string()))
    }
}

#[derive(Clone, Default)]
pub struct Codebase {
    ast_map: HashMap<String, syn::File>,
    free_functions: Vec<Function>,
    contracts: Vec<Contract>,
    structs: Vec<Struct>,
    enums: Vec<Enum>,
}

impl Codebase {
    pub fn new() -> Self {
        Codebase {
            ast_map: HashMap::new(),
            free_functions: Vec::new(),
            contracts: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
        }
    }

    pub fn parse_and_add_file(&mut self, file_name: &str, content: &mut str) -> Result<(), SDKErr> {
        let file = parse_file(file_name, content)?;
        self.ast_map.insert(file_name.to_string(), file);
        Ok(())
    }

    pub fn build_api(&mut self) {
        for (file_name, file) in &self.ast_map {
            for item in &file.items {
                match item {
                    syn::Item::Fn(f) => {
                        println!("Function: {}", f.sig.ident);
                    }
                    syn::Item::Struct(s) => {
                        println!("Struct: {}", s.ident);
                    }
                    syn::Item::Enum(e) => {
                        println!("Enum: {}", e.ident);
                    }
                    _ => {}
                }
            }
        }
    }
}

#[ast_node]
#[derive(Clone)]
pub struct Contract {
    name: String,
    functions: Vec<Function>,
    structs: Vec<Struct>,
    enums: Vec<Enum>,
}

impl Contract {
    pub fn new(name: &str, start_line: u32, start_col: u32, end_line: u32, end_col: u32) -> Self {
        Contract {
            name: name.to_string(),
            functions: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            start_line,
            start_col,
            end_line,
            end_col,
        }
    }
}

#[ast_node]
#[derive(Clone)]
pub struct Function {}

impl Function {
    pub fn new(start_line: u32, start_col: u32, end_line: u32, end_col: u32) -> Self {
        Function {
            start_line,
            start_col,
            end_line,
            end_col,
        }
    }
}

#[ast_node]
#[derive(Clone)]
pub struct Struct {}

impl Struct {
    pub fn new(start_line: u32, start_col: u32, end_line: u32, end_col: u32) -> Self {
        Struct {
            start_line,
            start_col,
            end_line,
            end_col,
        }
    }
}

#[ast_node]
#[derive(Clone)]
pub struct Enum {}

impl Enum {
    pub fn new(start_line: u32, start_col: u32, end_line: u32, end_col: u32) -> Self {
        Enum {
            start_line,
            start_col,
            end_line,
            end_col,
        }
    }
}
