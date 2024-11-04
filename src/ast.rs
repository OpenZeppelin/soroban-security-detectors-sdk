#![warn(clippy::pedantic)]
use crate::errors::SDKErr;
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
}

impl Codebase {
    pub fn new() -> Self {
        Codebase {
            ast_map: HashMap::new(),
            free_functions: Vec::new(),
            contracts: Vec::new(),
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

#[derive(Clone)]
pub struct Contract {
    name: String,
    functions: Vec<Function>,
    structs: Vec<Struct>,
    enums: Vec<Enum>,
}

impl Contract {
    pub fn new(name: &str) -> Self {
        Contract {
            name: name.to_string(),
            functions: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
        }
    }
}

#[derive(Clone)]
pub struct Function {}

impl Function {
    pub fn new() -> Self {
        Function {}
    }
}

#[derive(Clone)]
pub struct Struct {}

impl Struct {
    pub fn new() -> Self {
        Struct {}
    }
}

#[derive(Clone)]
pub struct Enum {}

impl Enum {
    pub fn new() -> Self {
        Enum {}
    }
}
