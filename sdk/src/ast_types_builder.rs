use std::path::{Path, PathBuf};
use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

use quote::ToTokens;
use syn::{Attribute, ExprBlock, ItemEnum, ItemFn, ItemStruct, PointerMutability};

use crate::ast::custom_type::{Type, TypeAlias, Typename};
use crate::ast::definition::{Module, Plane, Static, Trait};
use crate::ast::expression::{
    Addr, Array, Assign, Binary, Break, Cast, Closure, ConstBlock, Continue, EBlock, EStruct,
    ForLoop, FunctionCall, Identifier, If, IndexAccess, LetGuard, Lit, Loop, Match, MatchArm,
    MemberAccess, MethodCall, Parenthesized, Range, Reference, Repeat, Return, Try, Tuple, Unary,
    Unsafe, While,
};
use crate::ast::misc::{Field, Macro, Misc};
use crate::ast::node::Mutability;
use crate::definition::Implementation;
use crate::expression::{BinOp, UnOp};
use crate::file::File;
use crate::node_type::NodeType;
use crate::symbol_table::{process_definition, Scope, ScopeRef};
use crate::utils::project::{find_submodule_path, FileProvider};
use crate::{location, source_code, NodesStorage, SymbolTable};

use crate::ast::contract::Struct;
use crate::ast::definition::{Const, Definition, Enum};
use crate::ast::directive::{Directive, Use};
use crate::ast::expression::Expression;
use crate::ast::function::{FnParameter, Function};
use crate::ast::literal::{
    LBString, LBool, LByte, LCString, LChar, LFloat, LInt, LString, Literal,
};
use crate::ast::node::Visibility;
use crate::ast::node_type::NodeKind;
use crate::ast::pattern::Pattern;
use crate::ast::statement::{Block, Statement};

fn extract_attrs(attrs: &[Attribute]) -> Vec<String> {
    attrs
        .iter()
        .filter_map(|a| a.path().segments.first())
        .map(|seg| seg.ident.to_string())
        .collect()
}

pub(crate) struct ParserCtx<'a> {
    id: u32,
    file_provider: FileProvider,
    queue: Vec<(ScopeRef, PathBuf)>,
    storage: &'a mut NodesStorage,
    table: &'a mut SymbolTable,
}

impl<'a> ParserCtx<'a> {
    pub(crate) fn new(
        id: u32,
        file_provider: FileProvider,
        scope: ScopeRef,
        storage: &'a mut NodesStorage,
        table: &'a mut SymbolTable,
        root_path: PathBuf,
    ) -> Self {
        let queue = vec![(scope, root_path)];
        ParserCtx {
            id,
            file_provider,
            queue,
            storage,
            table,
        }
    }

    pub(crate) fn push(&mut self, scope: ScopeRef, file_path: PathBuf) -> &mut Self {
        self.queue.push((scope, file_path));
        self
    }

    pub(crate) fn pop(&mut self) -> Option<(ScopeRef, PathBuf)> {
        self.queue.pop()
    }

    fn get_node_id(&mut self) -> u32 {
        let id = self.id;
        self.id += 1;
        id
    }

    pub(crate) fn parse(&mut self) {
        while let Some((scope, file_path)) = self.pop() {
            let ast_file = self.parse_file(&file_path);
            for child in ast_file.children.borrow().iter() {
                match child {
                    NodeKind::Directive(Directive::Use(u)) => {
                        scope.borrow_mut().imports.push(u.clone());
                    }
                    NodeKind::Definition(def) => {
                        // process_definition(&scope, def.clone(), self.table);
                        if let Definition::Module(m) = def {
                            self.process_module_rec(m, &scope, &file_path);
                        } else {
                            process_definition(&scope, def.clone(), self.table);
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn process_module_rec(&mut self, m: &Rc<Module>, scope: &ScopeRef, file_path: &PathBuf) {
        if m.definitions.is_none() {
            let mod_name = m.name.clone();
            let mod_path = find_submodule_path(file_path, &mod_name, &self.file_provider);
            if mod_path.is_err() {
                return;
            }
            let mod_path = mod_path.unwrap();
            let mod_scope = Scope::new(
                m.id,
                format!("{}::{}", scope.borrow().name, mod_name),
                Some(scope.clone()),
            );
            scope.borrow_mut().children.push(mod_scope.clone());
            self.table.insert_scope(mod_scope.clone());
            self.push(mod_scope, mod_path);
        } else {
            let mod_scope = Scope::new(
                m.id,
                format!("{}::{}", scope.borrow().name, m.name),
                Some(scope.clone()),
            );
            scope.borrow_mut().children.push(mod_scope.clone());
            self.table.insert_scope(mod_scope.clone());
            for inner_def in m.definitions.as_ref().unwrap() {
                if let Definition::Module(inner_mod) = inner_def {
                    self.process_module_rec(inner_mod, &mod_scope, file_path);
                    continue;
                }
                process_definition(&mod_scope, inner_def.clone(), self.table);
            }
        }
    }

    #[allow(clippy::unnecessary_debug_formatting)]
    fn parse_file(&mut self, path: &Path) -> Rc<File> {
        let content = self
            .file_provider
            .read(path)
            .unwrap_or_else(|_| panic!("Failed to parse file at path: {path:?}"));

        let ast_file = syn::parse_file(content.as_str())
            .unwrap_or_else(|_| panic!("Failed to parse file at path: {path:?}"));
        let ast: Rc<File> = self.build_file(path.to_string_lossy().to_string(), ast_file);
        ast
    }

    fn build_file(&mut self, file_path: String, file_item: syn::File) -> Rc<File> {
        let mut file_name = String::new();
        let path = Path::new(&file_path);
        if let Some(filename) = path.file_name() {
            file_name = filename.to_string_lossy().to_string();
        }
        let rc_file = Rc::new(File {
            id: self.get_node_id(),
            children: RefCell::new(Vec::new()),
            name: file_name.clone(),
            path: file_path,
            attributes: File::attributes_from_file_item(&file_item),
            source_code: source_code!(file_item),
            location: location!(file_item),
        });
        let file_node = NodeKind::File(rc_file.clone());
        self.storage.add_node(file_node, 0);
        for item in file_item.items {
            if let syn::Item::Use(item_use) = &item {
                let directive = self.build_use_directive(item_use, rc_file.id);
                rc_file
                    .children
                    .borrow_mut()
                    .push(NodeKind::Directive(directive));
            } else {
                let definition = self.build_definition(&item, rc_file.id);
                rc_file
                    .children
                    .borrow_mut()
                    .push(NodeKind::Definition(definition));
            }
        }
        rc_file
    }

    fn build_literal_expression(
        &mut self,
        lit_expr: &syn::ExprLit,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let literal = self.build_literal(&lit_expr.lit);
        let expr = Expression::Literal(Rc::new(Lit {
            id,
            location: location!(lit_expr),
            value: literal,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_literal(&mut self, lit: &syn::Lit) -> Literal {
        let id = self.get_node_id();
        let location = location!(lit);
        match lit {
            syn::Lit::Bool(lit_bool) => Literal::Bool(Rc::new(LBool {
                id,
                location,
                value: lit_bool.value,
            })),
            syn::Lit::Byte(lit_byte) => Literal::Byte(Rc::new(LByte {
                id,
                location,
                value: lit_byte.value(),
            })),
            syn::Lit::Char(lit_char) => Literal::Char(Rc::new(LChar {
                id,
                location,
                value: lit_char.value(),
            })),
            syn::Lit::Float(lit_float) => Literal::Float(Rc::new(LFloat {
                id,
                location,
                value: lit_float.base10_digits().parse().unwrap(),
            })),
            syn::Lit::Int(lit_int) => Literal::Int(Rc::new(LInt {
                id,
                location,
                value: lit_int.base10_digits().parse().unwrap(),
            })),
            syn::Lit::Str(lit_str) => Literal::String(Rc::new(LString {
                id,
                location,
                value: lit_str.value(),
            })),
            syn::Lit::ByteStr(lit_bstr) => Literal::BString(Rc::new(LBString {
                id,
                location,
                value: String::from_utf8(lit_bstr.value()).expect("Invalid UTF-8 sequence"),
            })),
            syn::Lit::CStr(lit_cstr) => Literal::CString(Rc::new(LCString {
                id,
                location,
                value: lit_cstr.value().to_string_lossy().into_owned(),
            })),
            _ => panic!("Unsupported literal type"),
        }
    }

    fn build_definition(&mut self, item: &syn::Item, parent_id: u32) -> Definition {
        match item {
            syn::Item::Const(item_const) => self.build_const_definition(item_const, parent_id),
            syn::Item::Enum(item_enum) => Definition::Enum(self.build_enum(item_enum, parent_id)),
            syn::Item::ExternCrate(item_extern_crate) => {
                self.build_extern_crate_definition(item_extern_crate, parent_id)
            }
            syn::Item::Fn(item_fn) => {
                Definition::Function(self.build_function_from_item_fn(item_fn, None, parent_id))
            }
            syn::Item::ForeignMod(_) => todo!("Should not appear"),
            syn::Item::Impl(item_impl) => self.process_item_impl(item_impl, parent_id),
            syn::Item::Macro(item_macro) => self.build_macro_definition(item_macro, parent_id),
            syn::Item::Mod(item_mod) => self.build_mod_definition(item_mod, parent_id),
            syn::Item::Static(item_static) => self.build_static_definition(item_static, parent_id),
            syn::Item::Struct(item_struct) => {
                Definition::Struct(self.build_struct(item_struct, parent_id))
            }
            syn::Item::Trait(item_trait) => self.build_trait_definition(item_trait, parent_id),
            syn::Item::TraitAlias(item_trait_alias) => {
                self.build_trait_alias_definition(item_trait_alias, parent_id)
            }
            syn::Item::Type(item_type) => self.build_type_alias_definition(item_type, parent_id),
            syn::Item::Union(item_union) => self.build_union_definition(item_union, parent_id),
            syn::Item::Verbatim(token_stream) => {
                self.build_plane_definition(token_stream, parent_id)
            }
            _ => panic!(
                "We handle Use as directives, not statements. Unsupported item type: {}",
                item.into_token_stream()
            ),
        }
    }

    fn build_statement(&mut self, stmt: &syn::Stmt, parent_id: u32) -> Statement {
        match stmt {
            syn::Stmt::Expr(stmt_expr, sc) => {
                Statement::Expression(self.build_expression(stmt_expr, sc.is_none(), parent_id))
            }
            syn::Stmt::Local(stmt_let) => self.build_let_statement(stmt_let, parent_id),
            syn::Stmt::Macro(stmt_macro) => self.build_macro_statement(stmt_macro, parent_id),
            syn::Stmt::Item(stmt_item) => {
                Statement::Definition(self.build_definition(stmt_item, parent_id))
            }
        }
    }

    fn build_expression(&mut self, expr: &syn::Expr, is_ret: bool, parent_id: u32) -> Expression {
        match expr {
            syn::Expr::Array(array_expr) => {
                self.build_array_expression(array_expr, is_ret, parent_id)
            }
            syn::Expr::Assign(assign_expr) => {
                self.build_assign_expresison(assign_expr, is_ret, parent_id)
            }
            syn::Expr::Async(_) => {
                panic!("async expressions are not supported");
            }
            syn::Expr::Await(_) => {
                panic!("await expressions are not supported");
            }
            syn::Expr::Binary(expr_binary) => {
                self.build_binary_expression(expr_binary, is_ret, parent_id)
            }
            syn::Expr::Unary(expr_unary) => {
                self.build_unary_expression(expr_unary, is_ret, parent_id)
            }
            syn::Expr::Break(expr_break) => self.build_break_expression(expr_break, parent_id),
            syn::Expr::Block(block_expr) => self.build_block_expression(block_expr, parent_id),
            syn::Expr::Call(expr_call) => {
                self.build_function_call_expression(expr_call, is_ret, parent_id)
            }
            syn::Expr::Cast(expr_cast) => self.build_cast_expression(expr_cast, is_ret, parent_id),
            syn::Expr::Closure(expr_closure) => {
                self.build_closure_expression(expr_closure, is_ret, parent_id)
            }
            syn::Expr::Const(expr_const) => {
                self.build_const_block_expression(expr_const, parent_id)
            }
            syn::Expr::Continue(expr_continue) => {
                self.build_continue_expression(expr_continue, parent_id)
            }
            syn::Expr::ForLoop(expr_forloop) => {
                self.build_for_loop_expression(expr_forloop, is_ret, parent_id)
            }
            syn::Expr::Field(field_expr) => {
                self.build_member_access_expression(field_expr, is_ret, parent_id)
            }
            syn::Expr::If(expr_if) => self.build_if_expression(expr_if, is_ret, parent_id),
            syn::Expr::Index(expr_index) => {
                self.build_index_access_expression(expr_index, is_ret, parent_id)
            }
            syn::Expr::Infer(_) => self.build_discarded_identifier(expr, parent_id),
            syn::Expr::Let(expr_let) => {
                self.build_let_guard_expression(expr_let, is_ret, parent_id)
            }
            syn::Expr::Lit(expr_lit) => self.build_literal_expression(expr_lit, is_ret, parent_id),
            syn::Expr::Loop(expr_loop) => self.build_loop_expression(expr_loop, is_ret, parent_id),
            syn::Expr::Macro(expr_macro) => self.build_macro_expression(expr_macro, parent_id),
            syn::Expr::Match(expr_match) => {
                self.build_match_expression(expr_match, is_ret, parent_id)
            }
            syn::Expr::MethodCall(method_call) => {
                self.build_method_call_expression(method_call, is_ret, parent_id)
            }
            syn::Expr::Paren(expr_paren) => {
                self.build_parenthesied_expression(expr_paren, is_ret, parent_id)
            }
            syn::Expr::Path(expr_path) => self.build_identifier(expr_path, is_ret, parent_id),
            syn::Expr::Range(expr_range) => {
                self.build_range_expression(expr_range, is_ret, parent_id)
            }
            syn::Expr::RawAddr(expr_raddr) => {
                self.build_addr_expression(expr_raddr, is_ret, parent_id)
            }
            syn::Expr::Reference(expr_ref) => {
                self.build_reference_expression(expr_ref, is_ret, parent_id)
            }
            syn::Expr::Repeat(expr_repeat) => {
                self.build_repeat_expression(expr_repeat, is_ret, parent_id)
            }
            syn::Expr::Return(expr_return) => self.build_return_expression(expr_return, parent_id),
            syn::Expr::Yield(_) => panic!("yield expressions are not supported"),
            syn::Expr::Struct(expr_struct) => {
                self.build_struct_expression(expr_struct, is_ret, parent_id)
            }
            syn::Expr::Try(expr_try) => self.build_try_expression(expr_try, is_ret, parent_id),
            syn::Expr::TryBlock(expr_try_block) => {
                self.build_try_block_expression(expr_try_block, parent_id)
            }
            syn::Expr::Tuple(expr_tuple) => {
                self.build_tuple_expression(expr_tuple, is_ret, parent_id)
            }
            syn::Expr::Unsafe(expr_unsafe) => self.build_unsafe_expression(expr_unsafe, parent_id),
            syn::Expr::While(expr_while) => {
                self.build_while_expression(expr_while, is_ret, parent_id)
            }
            _ => panic!("Unsupported expression type {}", expr.into_token_stream()),
        }
    }

    fn build_struct(&mut self, struct_item: &ItemStruct, parent_id: u32) -> Rc<Struct> {
        let attributes = extract_attrs(&struct_item.attrs);
        let mut fields = Vec::new();
        let id = self.get_node_id();
        for field in &struct_item.fields {
            let field_name = match &field.ident {
                Some(ident) => ident.to_string(),
                None => "unnamed".to_string(),
            };
            let field_type = self.build_type(&field.ty, id);
            fields.push((field_name, field_type));
        }
        let rc_struct = Rc::new(Struct {
            id,
            attributes,
            location: location!(struct_item),
            name: Struct::contract_name_from_syn_item(struct_item),
            fields,
            is_contract: Struct::is_struct_contract(struct_item),
            visibility: Visibility::from_syn_visibility(&struct_item.vis),
        });
        self.storage.add_node(
            NodeKind::Definition(Definition::Struct(rc_struct.clone())),
            parent_id,
        );
        rc_struct
    }

    fn build_type(&mut self, ty: &syn::Type, _parent_id: u32) -> Type {
        let id = self.get_node_id();
        let location = location!(ty);
        let name = ty.to_token_stream().to_string();
        let node = Rc::new(Typename { id, location, name });
        Type::Typename(node)
    }

    fn build_enum(&mut self, enum_item: &ItemEnum, parent_id: u32) -> Rc<Enum> {
        let attributes = extract_attrs(&enum_item.attrs);
        let mut variants = Vec::new();
        for variant in &enum_item.variants {
            let variant_name = variant.ident.to_string();
            variants.push(variant_name);
        }
        let rc_enum = Rc::new(Enum {
            id: self.get_node_id(),
            attributes,
            location: location!(enum_item),
            name: enum_item.ident.to_string(),
            visibility: Visibility::from_syn_visibility(&enum_item.vis),
            variants,
        });
        self.storage.add_node(
            NodeKind::Definition(Definition::Enum(rc_enum.clone())),
            parent_id,
        );
        rc_enum
    }

    fn build_array_expression(
        &mut self,
        array_expr: &syn::ExprArray,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let elements = array_expr
            .elems
            .iter()
            .map(|elem| self.build_expression(elem, false, id))
            .collect();
        let expr = Expression::Array(Rc::new(Array {
            id,
            location: location!(array_expr),
            elements,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_function_call_expression(
        &mut self,
        expr_call: &syn::ExprCall,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let parameters = expr_call
            .args
            .iter()
            .map(|arg| self.build_expression(arg, false, id))
            .collect();
        let expr = Expression::FunctionCall(Rc::new(FunctionCall {
            id,
            location: location!(expr_call),
            function_name: FunctionCall::function_name_from_syn_item(expr_call),
            expression: self.build_expression(&expr_call.func, false, id),
            parameters,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_method_call_expression(
        &mut self,
        method_call: &syn::ExprMethodCall,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let method_call_id = self.get_node_id();
        let base = self.build_expression(&method_call.receiver, false, method_call_id);
        let parameters = method_call
            .args
            .iter()
            .map(|arg| self.build_expression(arg, false, method_call_id))
            .collect();
        let expr = Expression::MethodCall(Rc::new(MethodCall {
            id: method_call_id,
            location: location!(method_call),
            method_name: MethodCall::method_name_from_syn_item(method_call),
            base,
            parameters,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn process_item_impl(&mut self, item_impl: &syn::ItemImpl, parent_id: u32) -> Definition {
        let id = self.get_node_id();
        let attributes = extract_attrs(&item_impl.attrs);
        let for_type: Type = self.build_type(item_impl.self_ty.as_ref(), id);
        let mut functions = Vec::new();
        let mut constants = Vec::new();
        let mut types = Vec::new();
        let mut macroses = Vec::new();
        let mut planes = Vec::new();

        for item in &item_impl.items {
            match item {
                syn::ImplItem::Fn(assoc_fn) => {
                    let function = self.build_function_from_impl_item_fn(
                        assoc_fn,
                        item_impl.self_ty.as_ref(),
                        id,
                    );
                    functions.push(function.clone());
                }
                syn::ImplItem::Const(impl_item_const) => {
                    let const_definition =
                        self.build_const_definition_for_impl_item_const(impl_item_const, id);
                    if let Definition::Const(constant) = const_definition {
                        constants.push(constant);
                    }
                }
                syn::ImplItem::Type(impl_item_type) => {
                    let type_alias = self.build_associated_type(impl_item_type, id);
                    types.push(type_alias);
                }
                syn::ImplItem::Macro(impl_item_macro) => {
                    let macro_definition =
                        self.build_marco_definition_for_impl_item_macro(impl_item_macro, id);
                    if let Definition::Macro(macro_definition) = macro_definition {
                        macroses.push(macro_definition);
                    }
                }
                syn::ImplItem::Verbatim(token_stream) => {
                    let def = self.build_plane_definition(token_stream, id);
                    if let Definition::Plane(plane) = def {
                        planes.push(plane);
                    }
                }
                _ => unreachable!(),
            }
        }

        let implementation_definition = Definition::Implementation(Rc::new(Implementation {
            id,
            attributes,
            location: location!(item_impl),
            for_type,
            functions,
            constants,
            type_aliases: types,
            macros: macroses,
            plane_defs: planes,
        }));

        self.storage.add_node(
            NodeKind::Definition(implementation_definition.clone()),
            parent_id,
        );
        implementation_definition
    }

    fn build_reference_expression(
        &mut self,
        expr_ref: &syn::ExprReference,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let inner = self.build_expression(&expr_ref.expr, false, id);
        let expr = Expression::Reference(Rc::new(Reference {
            id,
            location: location!(expr_ref),
            inner,
            is_mutable: expr_ref.mutability.is_some(),
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_return_expression(
        &mut self,
        expr_return: &syn::ExprReturn,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let expression = expr_return
            .expr
            .as_ref()
            .map(|expr| self.build_expression(expr, false, id));
        let expr = Expression::Return(Rc::new(Return {
            id,
            location: location!(expr_return),
            expression,
            is_ret: true,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_repeat_expression(
        &mut self,
        expr_repeat: &syn::ExprRepeat,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let expression = self.build_expression(&expr_repeat.expr, false, id);
        let count = self.build_expression(&expr_repeat.len, false, id);
        let expr = Expression::Repeat(Rc::new(Repeat {
            id,
            location: location!(expr_repeat),
            expression,
            count,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_identifier(
        &mut self,
        expr_path: &syn::ExprPath,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let expr = Expression::Identifier(Rc::new(Identifier {
            id: self.get_node_id(),
            location: location!(expr_path),
            name: quote::quote! {#expr_path}.to_string(),
            is_ret,
        }));
        self.storage
            .add_node(NodeKind::Expression(expr.clone()), parent_id);
        expr
    }

    fn build_parenthesied_expression(
        &mut self,
        parenthesied: &syn::ExprParen,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let expression = self.build_expression(&parenthesied.expr, false, id);
        let expr = Expression::Parenthesized(Rc::new(Parenthesized {
            id,
            location: location!(parenthesied),
            expression,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_range_expression(
        &mut self,
        expr_range: &syn::ExprRange,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let start = match &expr_range.start {
            Some(start) => {
                let expr = self.build_expression(start.as_ref(), false, id);
                Some(expr)
            }
            None => None,
        };
        let end = match &expr_range.end {
            Some(end) => {
                let expr = self.build_expression(end.as_ref(), false, id);
                Some(expr)
            }
            None => None,
        };
        let is_closed = matches!(expr_range.limits, syn::RangeLimits::Closed(_));
        let range = Expression::Range(Rc::new(Range {
            id,
            location: location!(expr_range),
            start,
            end,
            is_closed,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(range.clone())),
            parent_id,
        );
        range
    }

    fn build_member_access_expression(
        &mut self,
        member_access: &syn::ExprField,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let base = self.build_expression(&member_access.base, false, id);
        let expr = Expression::MemberAccess(Rc::new(MemberAccess {
            id,
            location: location!(member_access),
            base,
            member_name: MemberAccess::member_name_from_syn_item(member_access),
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_assign_expresison(
        &mut self,
        assign: &syn::ExprAssign,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let left = self.build_expression(&assign.left, false, id);
        let right = self.build_expression(&assign.right, false, id);
        let expr = Expression::Assign(Rc::new(Assign {
            id,
            location: location!(assign),
            left,
            right,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_binary_expression(
        &mut self,
        binary: &syn::ExprBinary,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let left = self.build_expression(&binary.left, false, id);
        let right = self.build_expression(&binary.right, false, id);
        let expr = Expression::Binary(Rc::new(Binary {
            id,
            location: location!(binary),
            left,
            right,
            operator: BinOp::from_syn_item(&binary.op),
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_unary_expression(
        &mut self,
        unary: &syn::ExprUnary,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let inner = self.build_expression(&unary.expr, false, id);
        let expr = Expression::Unary(Rc::new(Unary {
            id,
            location: location!(unary),
            expression: inner,
            operator: UnOp::from_syn_item(&unary.op),
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_break_expression(
        &mut self,
        expr_break: &syn::ExprBreak,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let expr = if let Some(inner_expr) = &expr_break.expr {
            let expression = Some(self.build_expression(inner_expr, false, id));
            Expression::Break(Rc::new(Break {
                id,
                location: location!(expr_break),
                expression,
                is_ret: false,
            }))
        } else {
            Expression::Break(Rc::new(Break {
                id,
                location: location!(expr_break),
                expression: None,
                is_ret: false,
            }))
        };
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_closure_expression(
        &mut self,
        expr_closure: &syn::ExprClosure,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let body = self.build_expression(expr_closure.body.as_ref(), false, id);
        let captures = &expr_closure
            .capture
            .iter()
            .map(|capture| {
                let name = capture.span.source_text().unwrap();
                let identifier = Rc::new(Identifier {
                    id: self.get_node_id(),
                    location: location!(capture),
                    name,
                    is_ret: false,
                });
                self.storage.add_node(
                    NodeKind::Statement(Statement::Expression(Expression::Identifier(
                        identifier.clone(),
                    ))),
                    id,
                );
                identifier
            })
            .collect::<Vec<_>>();
        let returns = if let syn::ReturnType::Type(_, ty) = &expr_closure.output {
            self.build_type(ty, id)
        } else {
            self.build_type(&syn::parse_str::<syn::Type>("_").unwrap(), id)
        };
        self.storage.add_node(NodeKind::Type(returns.clone()), id);
        self.storage
            .add_node(NodeKind::Statement(Statement::Expression(body.clone())), id);
        let closure = Expression::Closure(Rc::new(Closure {
            id,
            location: location!(expr_closure),
            captures: captures.clone(),
            returns,
            body,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(closure.clone())),
            parent_id,
        );
        closure
    }

    fn build_cast_expression(
        &mut self,
        expr_cast: &syn::ExprCast,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let base = self.build_expression(&expr_cast.expr, false, id);
        // build the AST type node for the cast target
        let ty_node = self.build_type(&expr_cast.ty, id);
        self.storage.add_node(NodeKind::Type(ty_node.clone()), id);
        let expr = Expression::Cast(Rc::new(Cast {
            id,
            location: location!(expr_cast),
            base,
            target_type: ty_node,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_block_statement(&mut self, block: &syn::Block, parent_id: u32) -> Statement {
        let id = self.get_node_id();
        let statements = block
            .stmts
            .iter()
            .filter(|item| !matches!(item, syn::Stmt::Item(syn::Item::Use(_)))) //Won't fix. Use statements inside blocks for smart contracts or soroban_sdk - why?
            .map(|stmt| self.build_statement(stmt, id))
            .collect();
        let stmt = Statement::Block(Rc::new(Block {
            id,
            location: location!(block),
            statements,
        }));
        self.storage
            .add_node(NodeKind::Statement(stmt.clone()), parent_id);
        stmt
    }

    fn build_block_expression(&mut self, block_expr: &ExprBlock, parent_id: u32) -> Expression {
        let id = self.get_node_id();
        let stmt = self.build_block_statement(&block_expr.block, id);
        self.storage
            .add_node(NodeKind::Statement(stmt.clone()), parent_id);
        let block = Expression::EBlock(Rc::new(EBlock {
            id,
            location: location!(block_expr),
            block: match stmt {
                Statement::Block(ref block) => block.clone(),
                _ => panic!("Expected a block statement"),
            },
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(block.clone())),
            parent_id,
        );
        block
    }

    fn build_eblock_expression(block: &Rc<Block>, id: u32) -> Expression {
        Expression::EBlock(Rc::new(EBlock {
            id,
            location: block.location.clone(),
            block: block.clone(),
        }))
    }

    fn build_const_block_expression(
        &mut self,
        expr_const: &syn::ExprConst,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let block_statement = self.build_block_statement(&expr_const.block, parent_id);
        let const_block = Expression::Const(Rc::new(ConstBlock {
            id,
            location: block_statement.location().clone(),
            block: match block_statement {
                Statement::Block(ref block) => block.clone(),
                _ => panic!("Expected a block statement"),
            },
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(const_block.clone())),
            parent_id,
        );
        const_block
    }

    fn build_continue_expression(
        &mut self,
        expr_continue: &syn::ExprContinue,
        parent_id: u32,
    ) -> Expression {
        let expr = Expression::Continue(Rc::new(Continue {
            id: self.get_node_id(),
            location: location!(expr_continue),
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_for_loop_expression(
        &mut self,
        for_loop: &syn::ExprForLoop,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let block_statement = self.build_block_statement(&for_loop.body, parent_id);
        let expression = self.build_expression(&for_loop.expr, false, id);
        let for_loop = Expression::ForLoop(Rc::new(ForLoop {
            id,
            location: location!(for_loop),
            expression,
            block: match block_statement {
                Statement::Block(ref block) => block.clone(),
                _ => panic!("Expected a block statement"),
            },
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(for_loop.clone())),
            parent_id,
        );
        for_loop
    }

    fn build_if_expression(
        &mut self,
        if_expr: &syn::ExprIf,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let then_block = self.build_block_statement(&if_expr.then_branch, parent_id);
        let condition = self.build_expression(&if_expr.cond, false, id);
        let else_branch = if let Some((_, else_expr)) = &if_expr.else_branch {
            Some(self.build_expression(else_expr, false, id))
        } else {
            None
        };
        let expr = Expression::If(Rc::new(If {
            id,
            location: location!(if_expr),
            condition,
            then_branch: match then_block {
                Statement::Block(ref block) => block.clone(),
                _ => panic!("Expected a block statement"),
            },
            else_branch,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_loop_expression(
        &mut self,
        loop_expr: &syn::ExprLoop,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let block_statement = self.build_block_statement(&loop_expr.body, parent_id);
        let eloop = Loop {
            id,
            location: location!(loop_expr),
            block: match block_statement {
                Statement::Block(ref block) => block.clone(),
                _ => panic!("Expected a block statement"),
            },
            is_ret,
        };
        let expr = Expression::Loop(Rc::new(eloop));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_while_expression(
        &mut self,
        expr_while: &syn::ExprWhile,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let label = expr_while
            .label
            .as_ref()
            .map(|label| label.to_token_stream().to_string());
        let block_statement = self.build_block_statement(&expr_while.body, parent_id);
        let condition = self.build_expression(&expr_while.cond, false, id);
        let expr = Expression::While(Rc::new(While {
            id,
            location: location!(expr_while),
            label: label.clone(),
            condition,
            block: match block_statement {
                Statement::Block(block) => block.clone(),
                _ => panic!("Expected a block statement"),
            },
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_discarded_identifier(&mut self, expr: &syn::Expr, parent_id: u32) -> Expression {
        let identifier = Expression::Identifier(Rc::new(Identifier {
            id: self.get_node_id(),
            location: location!(expr),
            name: "_".to_string(),
            is_ret: false,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(identifier.clone())),
            parent_id,
        );
        identifier
    }

    fn build_index_access_expression(
        &mut self,
        index_access: &syn::ExprIndex,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let base = self.build_expression(&index_access.expr, false, id);
        let index = self.build_expression(&index_access.index, false, id);
        let expr = Expression::IndexAccess(Rc::new(IndexAccess {
            id,
            location: location!(index_access),
            base,
            index,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_let_guard_expression(
        &mut self,
        let_guard: &syn::ExprLet,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let guard = self.build_pattern(&let_guard.pat);
        self.storage.add_node(NodeKind::Pattern(guard.clone()), id);
        let value = self.build_expression(&let_guard.expr, false, id);
        let expr = Expression::LetGuard(Rc::new(LetGuard {
            id,
            location: location!(let_guard),
            guard,
            value,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_macro_expression(
        &mut self,
        macro_expr: &syn::ExprMacro,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let name = macro_expr.mac.path.to_token_stream().to_string();
        let text = macro_expr.mac.tokens.clone().to_string();
        let macro_ = Expression::Macro(Rc::new(Macro {
            id,
            location: location!(macro_expr),
            name,
            text,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(macro_.clone())),
            parent_id,
        );
        macro_
    }

    fn build_macro_definition(&mut self, macro_def: &syn::ItemMacro, parent_id: u32) -> Definition {
        let id = self.get_node_id();
        let name = macro_def.ident.to_token_stream().to_string();
        let text = macro_def.mac.tokens.clone().to_string();
        let def = Definition::Macro(Rc::new(Macro {
            id,
            location: location!(macro_def),
            name,
            text,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Definition(def.clone())),
            parent_id,
        );
        def
    }

    fn build_match_arm(&mut self, arm: &syn::Arm, id: u32) -> MatchArm {
        let pattern = self.build_pattern(&arm.pat);
        let expression = self.build_expression(&arm.body, false, id);
        MatchArm {
            pattern,
            expression,
        }
    }

    fn build_match_expression(
        &mut self,
        match_expr: &syn::ExprMatch,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let expression = self.build_expression(&match_expr.expr, false, id);
        let arms = match_expr
            .arms
            .iter()
            .map(|arm| self.build_match_arm(arm, id))
            .collect();
        let expr = Expression::Match(Rc::new(Match {
            id,
            location: location!(match_expr),
            expression,
            arms,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_unsafe_expression(
        &mut self,
        unsafe_expr: &syn::ExprUnsafe,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let block_statement = self.build_block_statement(&unsafe_expr.block, parent_id);
        let expr = Expression::Unsafe(Rc::new(Unsafe {
            id,
            location: location!(unsafe_expr),
            block: match block_statement {
                Statement::Block(ref block) => block.clone(),
                _ => panic!("Expected a block statement"),
            },
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_tuple_expression(
        &mut self,
        expr_tuple: &syn::ExprTuple,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let elements = expr_tuple
            .elems
            .iter()
            .map(|expr| self.build_expression(expr, false, id))
            .collect::<Vec<_>>();
        let expr = Expression::Tuple(Rc::new(Tuple {
            id,
            location: location!(expr_tuple),
            elements,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_struct_expression(
        &mut self,
        expr_struct: &syn::ExprStruct,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let name = expr_struct.path.segments.last().unwrap().ident.to_string();
        let fields = expr_struct
            .fields
            .iter()
            .map(|field| {
                let name = match field.member {
                    syn::Member::Named(ref ident) => ident.to_string(),
                    syn::Member::Unnamed(_) => String::new(),
                };
                let value = self.build_expression(&field.expr, false, parent_id);
                (name, value)
            })
            .collect::<Vec<_>>();
        let rest_dots = expr_struct
            .rest
            .as_ref()
            .map(|expr| self.build_expression(expr, false, parent_id));
        let estruct = Expression::EStruct(Rc::new(EStruct {
            id: self.get_node_id(),
            location: location!(expr_struct),
            name,
            fields,
            rest_dots,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(estruct.clone())),
            parent_id,
        );
        estruct
    }

    fn build_addr_expression(
        &mut self,
        expr_raddr: &syn::ExprRawAddr,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let expression = self.build_expression(&expr_raddr.expr, false, id);
        let mutability = match expr_raddr.mutability {
            PointerMutability::Const(_) => Mutability::Constant,
            PointerMutability::Mut(_) => Mutability::Mutable,
        };
        let addr = Expression::Addr(Rc::new(Addr {
            id,
            location: location!(expr_raddr),
            mutability,
            expression,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(addr.clone())),
            parent_id,
        );
        addr
    }

    fn build_try_expression(
        &mut self,
        expr_try: &syn::ExprTry,
        is_ret: bool,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let expression = self.build_expression(&expr_try.expr, false, id);
        let expr = Expression::Try(Rc::new(Try {
            id,
            location: location!(expr_try),
            expression,
            is_ret,
        }));
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_try_block_expression(
        &mut self,
        expr_try_block: &syn::ExprTryBlock,
        parent_id: u32,
    ) -> Expression {
        let id = self.get_node_id();
        let block_statement = self.build_block_statement(&expr_try_block.block, parent_id);
        let expr = match block_statement {
            Statement::Block(block) => ParserCtx::build_eblock_expression(&block, id),
            _ => panic!(
                "Expected a block statement but got {}",
                serde_json::to_string(&block_statement).unwrap()
            ),
        };
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(expr.clone())),
            parent_id,
        );
        expr
    }

    fn build_pattern(&mut self, pat: &syn::Pat) -> Pattern {
        let id = self.get_node_id();
        let location = location!(pat);
        let pat_string = match pat {
            syn::Pat::Const(expr_const) => expr_const.to_token_stream().to_string(),
            syn::Pat::Ident(pat_ident) => pat_ident.to_token_stream().to_string(),
            syn::Pat::Lit(expr_lit) => expr_lit.to_token_stream().to_string(),
            syn::Pat::Macro(expr_macro) => expr_macro.to_token_stream().to_string(),
            syn::Pat::Or(pat_or) => pat_or.to_token_stream().to_string(),
            syn::Pat::Paren(pat_paren) => pat_paren.to_token_stream().to_string(),
            syn::Pat::Path(expr_path) => expr_path.to_token_stream().to_string(),
            syn::Pat::Range(expr_range) => expr_range.to_token_stream().to_string(),
            syn::Pat::Reference(pat_reference) => pat_reference.to_token_stream().to_string(),
            syn::Pat::Rest(pat_rest) => pat_rest.to_token_stream().to_string(),
            syn::Pat::Slice(pat_slice) => pat_slice.to_token_stream().to_string(),
            syn::Pat::Struct(pat_struct) => pat_struct.to_token_stream().to_string(),
            syn::Pat::Tuple(pat_tuple) => pat_tuple.to_token_stream().to_string(),
            syn::Pat::TupleStruct(pat_tuple_struct) => {
                pat_tuple_struct.to_token_stream().to_string()
            }
            syn::Pat::Type(pat_type) => pat_type.to_token_stream().to_string(),
            syn::Pat::Verbatim(token_stream) => token_stream.to_string(),
            syn::Pat::Wild(_) => "_".to_string(),
            _ => String::new(),
        };
        Pattern {
            id,
            location,
            kind: pat_string.to_string(),
        }
    }

    fn build_macro_statement(&mut self, macro_stmt: &syn::StmtMacro, parent_id: u32) -> Statement {
        let id = self.get_node_id();
        let location = location!(macro_stmt);
        let macro_ = Statement::Macro(Rc::new(Macro {
            id,
            location,
            name: macro_stmt.mac.path.to_token_stream().to_string(),
            text: quote::quote! {#macro_stmt}.to_string().replace(' ', ""),
        }));
        self.storage
            .add_node(NodeKind::Statement(macro_.clone()), parent_id);
        macro_
    }

    fn build_let_statement(&mut self, stmt_let: &syn::Local, parent_id: u32) -> Statement {
        let location = location!(stmt_let);
        let name = match &stmt_let.pat {
            syn::Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
            syn::Pat::Type(pat_type) => match &*pat_type.pat {
                syn::Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
                other => other.to_token_stream().to_string(),
            },
            other => other.to_token_stream().to_string(),
        };
        let pattern = self.build_pattern(&stmt_let.pat);
        let id = self.get_node_id();
        let mut initial_value = None;
        let mut initial_value_alternative = None;
        let ref_stmt = &stmt_let;
        if ref_stmt.init.is_some() {
            let expr = self.build_expression(&stmt_let.init.as_ref().unwrap().expr, false, id);
            initial_value = Some(expr.clone());
            self.storage
                .add_node(NodeKind::Statement(Statement::Expression(expr)), id);

            if stmt_let.init.clone().unwrap().diverge.is_some() {
                let expr = self.build_expression(
                    stmt_let
                        .init
                        .as_ref()
                        .unwrap()
                        .diverge
                        .as_ref()
                        .unwrap()
                        .1
                        .as_ref(),
                    false,
                    id,
                );
                initial_value_alternative = Some(expr.clone());
                self.storage
                    .add_node(NodeKind::Statement(Statement::Expression(expr)), id);
            }
        }

        let statement = Statement::Let(Rc::new(super::statement::Let {
            id,
            location,
            name,
            pattern,
            initial_value,
            initial_value_alternative,
        }));
        self.storage
            .add_node(NodeKind::Statement(statement.clone()), parent_id);
        statement
    }

    fn build_const_definition(
        &mut self,
        item_const: &syn::ItemConst,
        parent_id: u32,
    ) -> Definition {
        let id = self.get_node_id();
        let location = location!(item_const);
        let name = item_const.ident.to_string();
        let value = self.build_expression(&item_const.expr, false, id);
        self.storage.add_node(
            NodeKind::Statement(Statement::Expression(value.clone())),
            id,
        );
        let ty = self.build_type(item_const.ty.as_ref(), id);
        let visibility = Visibility::from_syn_visibility(&item_const.vis);

        let def = Definition::Const(Rc::new(Const {
            id,
            location,
            name,
            visibility,
            type_: ty,
            value: Some(value),
        }));

        self.storage
            .add_node(NodeKind::Definition(def.clone()), parent_id);

        def
    }

    fn build_extern_crate_definition(
        &mut self,
        stmt_extern_crate: &syn::ItemExternCrate,
        parent_id: u32,
    ) -> Definition {
        let def = Definition::ExternCrate(Rc::new(super::definition::ExternCrate {
            id: self.get_node_id(),
            location: location!(stmt_extern_crate),
            name: stmt_extern_crate.ident.to_string(),
            visibility: Visibility::from_syn_visibility(&stmt_extern_crate.vis),
            alias: stmt_extern_crate
                .rename
                .as_ref()
                .map(|(_, ident)| ident.to_string()),
        }));
        self.storage
            .add_node(NodeKind::Definition(def.clone()), parent_id);
        def
    }

    fn build_function_from_item_fn(
        &mut self,
        item_fn: &syn::ItemFn,
        self_name: Option<String>,
        parent_id: u32,
    ) -> Rc<Function> {
        let id: u32 = self.get_node_id();
        let name = item_fn.sig.ident.to_string();
        let mut fn_parameters: Vec<Rc<FnParameter>> = Vec::new();
        for arg in &item_fn.sig.inputs {
            match arg {
                syn::FnArg::Receiver(receiver) => {
                    let rc_param = Rc::new(FnParameter {
                        id: self.get_node_id(),
                        name: "self".to_string(),
                        location: location!(receiver),
                        type_name: FnParameter::type_name_from_syn_item(&receiver.ty),
                        is_self: true,
                        is_mut: receiver.mutability.is_some(),
                    });
                    fn_parameters.push(rc_param.clone());
                    self.storage
                        .add_node(NodeKind::Misc(Misc::FnParameter(rc_param)), id);
                }
                syn::FnArg::Typed(pat_type) => {
                    if let syn::Pat::Ident(pat_ident) = &*pat_type.pat {
                        let name = pat_ident.ident.to_string();
                        let is_self = name == "self";
                        let arg_type = &pat_type.ty;
                        let rc_param = Rc::new(FnParameter {
                            id: self.get_node_id(),
                            name,
                            location: location!(arg),
                            type_name: FnParameter::type_name_from_syn_item(arg_type),
                            is_self,
                            is_mut: pat_ident.mutability.is_some(),
                        });
                        fn_parameters.push(rc_param.clone());
                        self.storage
                            .add_node(NodeKind::Misc(Misc::FnParameter(rc_param)), id);
                    }
                }
            }
        }
        let returns = if let syn::ReturnType::Type(_, ty) = &item_fn.sig.output {
            let mut ty = NodeType::from_syn_item(ty);
            if ty.is_self() {
                ty.replace_path(self_name.unwrap().clone());
                ty
            } else {
                ty
            }
        } else {
            NodeType::Empty
        };
        let block_statement = self.build_block_statement(&item_fn.block, id);
        let block = match block_statement.clone() {
            Statement::Block(block) => {
                self.storage
                    .add_node(NodeKind::Statement(Statement::Block(block.clone())), id);
                Some(block)
            }
            _ => None,
        };

        let generics = item_fn
            .sig
            .generics
            .params
            .iter()
            .map(|p| p.to_token_stream().to_string())
            .collect();
        let attributes = item_fn
            .attrs
            .iter()
            .map(|a| a.path().segments[0].ident.to_string())
            .collect();
        let function = Rc::new(Function {
            id,
            attributes,
            location: location!(item_fn),
            name,
            visibility: Visibility::from_syn_visibility(&item_fn.vis),
            generics,
            parameters: fn_parameters,
            returns: Rc::new(RefCell::new(returns)),
            body: block,
        });
        self.storage.add_node(
            NodeKind::Definition(Definition::Function(function.clone())),
            parent_id,
        );
        function
    }

    fn build_function_from_impl_item_fn(
        &mut self,
        item_fn: &syn::ImplItemFn,
        self_ty: &syn::Type,
        id: u32,
    ) -> Rc<Function> {
        let self_name = self_ty.to_token_stream().to_string().replace(' ', "");

        self.build_function_from_item_fn(
            &ItemFn {
                attrs: item_fn.attrs.clone(),
                vis: item_fn.vis.clone(),
                sig: item_fn.sig.clone(),
                block: Box::new(item_fn.block.clone()),
            },
            Some(self_name),
            id,
        )
    }

    fn build_associated_type(
        &mut self,
        item_type: &syn::ImplItemType,
        parent_id: u32,
    ) -> Rc<TypeAlias> {
        let id = self.get_node_id();
        let type_alias = Rc::new(TypeAlias {
            id,
            location: location!(item_type),
            name: item_type.ident.to_string(),
            visibility: Visibility::from_syn_visibility(&item_type.vis),
            ty: self.build_type(&item_type.ty, parent_id),
        });
        self.storage.add_node(
            NodeKind::Definition(Definition::AssocType(type_alias.clone())),
            parent_id,
        );
        type_alias
    }

    fn build_static_definition(
        &mut self,
        item_static: &syn::ItemStatic,
        parent_id: u32,
    ) -> Definition {
        let id = self.get_node_id();
        let attributes = extract_attrs(&item_static.attrs);
        let location = location!(item_static);
        let name = item_static.ident.to_string();
        let visibility = Visibility::from_syn_visibility(&item_static.vis);
        let mutable = matches!(item_static.mutability, syn::StaticMutability::Mut(_));
        let ty = self.build_type(&item_static.ty, id);
        let expr = self.build_expression(&item_static.expr, false, id);

        let static_def = Definition::Static(Rc::new(Static {
            id,
            attributes,
            location,
            name,
            visibility,
            mutable,
            ty,
            value: expr,
        }));

        self.storage
            .add_node(NodeKind::Definition(static_def.clone()), parent_id);
        static_def
    }

    fn build_mod_definition(&mut self, item_mod: &syn::ItemMod, parent_id: u32) -> Definition {
        let id = self.get_node_id();
        let attributes = extract_attrs(&item_mod.attrs);
        let location = location!(item_mod);
        let name = item_mod.ident.to_string();
        let visibility = Visibility::from_syn_visibility(&item_mod.vis);
        let definitions = item_mod.content.as_ref().map(|(_, items)| {
            items
                .iter()
                .filter(|item| !matches!(item, syn::Item::Use(_)))
                .map(|item| self.build_definition(item, id))
                .collect::<Vec<_>>()
        });
        let imports = item_mod.content.as_ref().map(|(_, items)| {
            items
                .iter()
                .filter_map(|item| {
                    if let syn::Item::Use(use_directive) = item {
                        Some(self.build_use_directive(use_directive, id))
                    } else {
                        None
                    }
                })
                .map(std::convert::Into::into)
                .collect::<Vec<_>>()
        });
        let mod_def = Definition::Module(Rc::new(Module {
            id,
            location,
            attributes,
            name,
            visibility,
            definitions,
            imports,
        }));

        self.storage
            .add_node(NodeKind::Definition(mod_def.clone()), parent_id);
        mod_def
    }

    fn build_plane_definition(
        &mut self,
        token_stream: &proc_macro2::TokenStream,
        parent_id: u32,
    ) -> Definition {
        let id = self.get_node_id();
        let location = location!(token_stream);
        let value = token_stream.to_string();
        let plane_def = Definition::Plane(Rc::new(Plane {
            id,
            location,
            value,
        }));
        self.storage
            .add_node(NodeKind::Definition(plane_def.clone()), parent_id);
        plane_def
    }

    fn build_use_directive(&mut self, use_directive: &syn::ItemUse, parent_id: u32) -> Directive {
        let raw_path = use_directive.tree.to_token_stream().to_string();

        let imported = use_tree_to_full_paths(&use_directive.tree, None);
        let directive = Directive::Use(Rc::new(Use {
            id: self.get_node_id(),
            location: location!(use_directive),
            visibility: Visibility::from_syn_visibility(&use_directive.vis),
            path: raw_path,
            imported_types: imported,
            target: RefCell::new(BTreeMap::new()),
        }));
        self.storage
            .add_node(NodeKind::Directive(directive.clone()), parent_id);
        directive
    }

    fn build_type_alias_definition(
        &mut self,
        item_type: &syn::ItemType,
        parent_id: u32,
    ) -> Definition {
        let id = self.get_node_id();
        let location = location!(item_type);
        let name = item_type.ident.to_string();
        let visibility = Visibility::from_syn_visibility(&item_type.vis);
        let ty = self.build_type(&item_type.ty, id);

        let type_def = Definition::TypeAlias(Rc::new(TypeAlias {
            id,
            location,
            name,
            visibility,
            ty,
        }));

        self.storage
            .add_node(NodeKind::Definition(type_def.clone()), parent_id);
        type_def
    }

    fn build_field(&mut self, field: &syn::Field, parent_id: u32) -> Rc<Field> {
        let id = self.get_node_id();
        let location = location!(field);
        let name = field.ident.as_ref().map(std::string::ToString::to_string);
        let visibility = Visibility::from_syn_visibility(&field.vis);
        let mutability = match field.mutability {
            syn::FieldMutability::None => Mutability::Constant,
            _ => Mutability::Mutable,
        };
        let ty = self.build_type(&field.ty, id);
        let field = Rc::new(Field {
            id,
            location,
            name,
            visibility,
            mutability,
            ty,
        });
        self.storage
            .add_node(NodeKind::Misc(Misc::Field(field.clone())), parent_id);
        field
    }

    fn build_union_definition(
        &mut self,
        item_union: &syn::ItemUnion,
        parent_id: u32,
    ) -> Definition {
        let id = self.get_node_id();
        let attributes = extract_attrs(&item_union.attrs);
        let location = location!(item_union);
        let name = item_union.ident.to_string();
        let visibility = Visibility::from_syn_visibility(&item_union.vis);
        let fields = item_union
            .fields
            .named
            .iter()
            .map(|f| self.build_field(f, id))
            .collect();

        let union_def = Definition::Union(Rc::new(super::definition::Union {
            id,
            location,
            attributes,
            name,
            visibility,
            fields,
        }));

        self.storage
            .add_node(NodeKind::Definition(union_def.clone()), parent_id);
        union_def
    }

    fn build_const_definition_for_impl_item_const(
        &mut self,
        item: &syn::ImplItemConst,
        parent_id: u32,
    ) -> Definition {
        let id = self.get_node_id();
        let location = location!(item);
        let name = item.ident.to_string();
        let visibility = Visibility::from_syn_visibility(&item.vis);

        let ty = self.build_type(&item.ty, id);
        let value = self.build_expression(&item.expr, false, id);

        let constant_def = Definition::Const(Rc::new(Const {
            id,
            location,
            name,
            visibility,
            type_: ty,
            value: Some(value),
        }));

        self.storage
            .add_node(NodeKind::Definition(constant_def.clone()), parent_id);

        constant_def
    }

    fn build_const_definition_from_trait_item(
        &mut self,
        item: &syn::TraitItemConst,
        visibility: Visibility,
        parent_id: u32,
    ) -> Definition {
        let id = self.get_node_id();
        let location = location!(item);
        let name = item.ident.to_string();
        let ty = self.build_type(&item.ty, id);
        let value = item
            .default
            .as_ref()
            .map(|expr| self.build_expression(&expr.1, false, id));

        let constant_def = Definition::Const(Rc::new(Const {
            id,
            location,
            name,
            visibility,
            type_: ty,
            value,
        }));

        self.storage
            .add_node(NodeKind::Definition(constant_def.clone()), parent_id);

        constant_def
    }

    fn build_function_definition_for_trait_item_fn(
        &mut self,
        item: &syn::TraitItemFn,
        visibility: Visibility,
        parent_id: u32,
    ) -> Definition {
        let id = self.get_node_id();
        let name = item.sig.ident.to_string();
        let mut fn_parameters: Vec<Rc<FnParameter>> = Vec::new();
        for arg in &item.sig.inputs {
            match arg {
                syn::FnArg::Receiver(receiver) => {
                    let rc_param = Rc::new(FnParameter {
                        id: self.get_node_id(),
                        name: "self".to_string(),
                        location: location!(receiver),
                        type_name: FnParameter::type_name_from_syn_item(&receiver.ty),
                        is_self: true,
                        is_mut: receiver.mutability.is_some(),
                    });
                    fn_parameters.push(rc_param.clone());
                    self.storage
                        .add_node(NodeKind::Misc(Misc::FnParameter(rc_param)), id);
                }
                syn::FnArg::Typed(pat_type) => {
                    if let syn::Pat::Ident(pat_ident) = &*pat_type.pat {
                        let name = pat_ident.ident.to_string();
                        let is_self = name == "self";
                        let arg_type = &pat_type.ty;
                        let rc_param = Rc::new(FnParameter {
                            id: self.get_node_id(),
                            name,
                            location: location!(arg),
                            type_name: FnParameter::type_name_from_syn_item(arg_type),
                            is_self,
                            is_mut: pat_ident.mutability.is_some(),
                        });
                        fn_parameters.push(rc_param.clone());
                        self.storage
                            .add_node(NodeKind::Misc(Misc::FnParameter(rc_param)), id);
                    }
                }
            }
        }
        let returns = if let syn::ReturnType::Type(_, ty) = &item.sig.output {
            NodeType::from_syn_item(ty)
        } else {
            NodeType::Empty
        };

        let generics = item
            .sig
            .generics
            .params
            .iter()
            .map(|p| p.to_token_stream().to_string())
            .collect();
        let attributes = item
            .attrs
            .iter()
            .map(|a| a.path().segments[0].ident.to_string())
            .collect();
        let function = Rc::new(Function {
            id: self.get_node_id(),
            attributes,
            location: location!(item),
            name,
            visibility,
            generics,
            parameters: fn_parameters.clone(),
            returns: Rc::new(RefCell::new(returns)),
            body: None,
        });
        self.storage.add_node(
            NodeKind::Definition(Definition::Function(function.clone())),
            parent_id,
        );
        Definition::Function(function)
    }

    fn build_impl_trait_type(&mut self, item: &syn::TraitItemType, _: u32) -> Definition {
        Definition::Plane(Rc::new(Plane {
            id: self.get_node_id(),
            location: location!(item),
            value: "TraitItemType".to_string(),
        }))
    }

    fn build_macro_definition_for_trait_item_macro(
        &mut self,
        item: &syn::TraitItemMacro,
        parent_id: u32,
    ) -> Definition {
        let id = self.get_node_id();
        let location = location!(item);
        let name = item.mac.path.to_token_stream().to_string();
        let text = item.mac.tokens.clone().to_string();
        let macro_ = Definition::Macro(Rc::new(Macro {
            id,
            location,
            name,
            text,
        }));
        self.storage
            .add_node(NodeKind::Definition(macro_.clone()), parent_id);
        macro_
    }

    fn build_trait_definition(
        &mut self,
        item_trait: &syn::ItemTrait,
        parent_id: u32,
    ) -> Definition {
        let id = self.get_node_id();
        let attributes = extract_attrs(&item_trait.attrs);
        let location = location!(item_trait);
        let name = item_trait.ident.to_string();
        let visibility = Visibility::from_syn_visibility(&item_trait.vis);
        let supertraits = item_trait.supertraits.to_token_stream().to_string();
        let items = item_trait
            .items
            .iter()
            .map(|item| match item {
                syn::TraitItem::Const(item) => {
                    self.build_const_definition_from_trait_item(item, visibility.clone(), id)
                }
                syn::TraitItem::Fn(item) => {
                    self.build_function_definition_for_trait_item_fn(item, visibility.clone(), id)
                }
                syn::TraitItem::Type(item) => self.build_impl_trait_type(item, id),
                syn::TraitItem::Macro(item) => {
                    self.build_macro_definition_for_trait_item_macro(item, id)
                }
                syn::TraitItem::Verbatim(token_stream) => {
                    self.build_plane_definition(token_stream, id)
                }
                _ => todo!(),
            })
            .collect();

        let trait_def = Definition::Trait(Rc::new(Trait {
            id,
            location,
            attributes,
            name,
            visibility,
            supertraits,
            items,
        }));

        self.storage
            .add_node(NodeKind::Definition(trait_def.clone()), parent_id);
        trait_def
    }

    fn build_trait_alias_definition(
        &mut self,
        item_trait_alias: &syn::ItemTraitAlias,
        parent_id: u32,
    ) -> Definition {
        let id = self.get_node_id();
        let attributes = extract_attrs(&item_trait_alias.attrs);
        let location = location!(item_trait_alias);
        let name = item_trait_alias.ident.to_string();
        let visibility = Visibility::from_syn_visibility(&item_trait_alias.vis);
        let bounds = item_trait_alias.bounds.to_token_stream().to_string();

        let trait_alias_def = Definition::TraitAlias(Rc::new(super::definition::TraitAlias {
            id,
            location,
            attributes,
            name,
            visibility,
            bounds,
        }));

        self.storage
            .add_node(NodeKind::Definition(trait_alias_def.clone()), parent_id);
        trait_alias_def
    }

    fn build_marco_definition_for_impl_item_macro(
        &mut self,
        item: &syn::ImplItemMacro,
        parent_id: u32,
    ) -> Definition {
        let id = self.get_node_id();
        let location = location!(item);
        let name = item.mac.path.to_token_stream().to_string();
        let text = item.mac.tokens.clone().to_string();
        let macro_ = Definition::Macro(Rc::new(Macro {
            id,
            location,
            name,
            text,
        }));
        self.storage
            .add_node(NodeKind::Definition(macro_.clone()), parent_id);
        macro_
    }
}

fn use_tree_to_full_paths(tree: &syn::UseTree, prefix: Option<String>) -> Vec<String> {
    match tree {
        syn::UseTree::Path(syn::UsePath { ident, tree, .. }) => {
            let next = if prefix.is_none() {
                ident.to_string()
            } else {
                format!("{}::{ident}", prefix.unwrap())
            };
            use_tree_to_full_paths(tree, Some(next))
        }
        syn::UseTree::Name(syn::UseName { ident, .. }) => {
            let path = if prefix.is_none() {
                ident.to_string()
            } else {
                format!("{}::{ident}", prefix.as_ref().unwrap())
            };
            vec![path]
        }
        syn::UseTree::Rename(use_rename) => {
            let base = if prefix.is_none() {
                use_rename.ident.to_string()
            } else {
                format!("{}::{}", prefix.unwrap(), use_rename.ident)
            };
            vec![format!("{}%{}", base, use_rename.rename)]
        }
        syn::UseTree::Glob(_) => {
            let path = if prefix.is_none() {
                "*".to_string()
            } else {
                format!("{}::*", prefix.unwrap())
            };
            vec![path]
        }
        syn::UseTree::Group(syn::UseGroup { items, .. }) => items
            .iter()
            .flat_map(|t| use_tree_to_full_paths(t, prefix.clone()))
            .collect(),
    }
}
