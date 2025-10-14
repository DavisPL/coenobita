#![feature(rustc_private)]

mod cx;

use coenobita_middle::ty::{PassError, Type, TypeKind};

use std::collections::{BTreeSet, HashMap, VecDeque};
use std::fmt::Display;

use std::hash::Hash;
use std::cmp::Eq;

use itertools::Itertools;
use rustc_middle::ty::{Ty, TyCtxt, TyKind};
use rustc_span::{ErrorGuaranteed, Ident, Span, Symbol};

use coenobita_middle::set::{Set, SetCtx};

extern crate rustc_abi;
extern crate rustc_ast;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_infer;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_trait_selection;

use rustc_middle::ty::{self, FieldDef, GenericArg, TypeckResults, TypingEnv};
use rustc_hir::def::{CtorOf, DefKind, Res};
use rustc_hir::{
    def_id::DefId, Arm, AttrArgs, AttrKind, Attribute, Block, BodyId, Closure, Expr, ExprField, ExprKind, FnSig, HirId, Impl, ImplItem, ImplItemKind, Item, ItemKind, LangItem, LetExpr, LetStmt, MatchSource, PatField, PatKind, QPath, Stmt, StmtKind
};

use log::{debug, warn};

pub type Result<T = ()> = std::result::Result<T, ErrorGuaranteed>;
type Bound = (String, Set);

use coenobita_parse::{create_parser, create_psess, parse::Parse, Pass, Take};

use crate::cx::InfCtx;

struct Ctx<K, V> {
    scopes: Vec<HashMap<K, V>>
}

impl<K: Hash + Eq, V> Ctx<K, V> {
    fn new() -> Self {
        Self { scopes: vec![] }
    }

    fn enter(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit(&mut self) {
        self.scopes.pop();
    }

    pub fn get(&self, k: &K) -> Option<&V> {
        self.scopes.iter()
            .rev()
            .find_map(|cx| cx.get(k))
    }

    pub fn set(&mut self, k: K, v: V) {
        match self.scopes.last_mut() {
            Some(cx) => cx.insert(k, v),
            None => None
        };
	}
}

pub struct Checker<'tcx> {
	tcx: TyCtxt<'tcx>,
	take_attr: Vec<Symbol>,
    pass_attr: Vec<Symbol>,

    scx: SetCtx,

    str_to_hir: HashMap<String, HirId>,
    hir_to_ty: HashMap<HirId, Type>,

    ty_constructors: HashMap<DefId, Type>,

    vctx: Ctx<HirId, Type>,

    icx: InfCtx
}

impl<'tcx> Checker<'tcx> {
	pub fn new(crate_name: String, tcx: TyCtxt<'tcx>) -> Self {
		Checker {
            tcx,
            take_attr: vec![Symbol::intern("coenobita"), Symbol::intern("take")],
            pass_attr: vec![Symbol::intern("coenobita"), Symbol::intern("pass")],
            scx: SetCtx::new(),

            str_to_hir: HashMap::new(),
            hir_to_ty: HashMap::new(),

            ty_constructors: HashMap::new(),

            vctx: Ctx::new(),

            icx: InfCtx::new(crate_name)
        }
	}

    fn check_item_fn(&mut self, sig: &FnSig, body_id: BodyId) -> Result {
        let def_id = body_id.hir_id.owner.to_def_id();
        let local_def_id = def_id.as_local().unwrap();

        // Enter a new scope in the set context
        self.scx.enter();

        // Collect all set variables introduced by this function
		for attr in self.tcx.get_attrs_by_path(def_id, &self.take_attr) {
            let take = self.parse_attr_take(attr)?;

            debug!("Introducing {} ⊆ {}", take.left.0, take.bound);

            // Make sure this variable hasn't been introduced yet
            if let Some(bound) = self.scx.get(&take.left.0) {
                return Err(self.tcx.dcx().span_err(take.left.1, format!("Variable has already been introduced with upper bound {bound}")))
            }

            // Make sure all variables in the bound exist
            self.ensure_variables_exist(&take.bound, &take.right)?;
            
            // Add upper bound to set variable context
            self.scx.set(take.left.0, take.bound);            
		}

        let body = self.tcx.hir_body(body_id);

        for param in body.params {
            let param_ty = self.tcx.typeck(local_def_id).node_type(param.hir_id);

            let type_constr = self.get_constr_for_ty(&param_ty);

            debug!("- {:#?} : {param_ty}", param.pat.simple_ident());

            self.process_pattern(param.pat.kind, type_constr, param.pat.hir_id);
        }

        self.display_str_to_hir();
        self.display_hir_to_ty();
        
        let mut ret_ty = self.top_type();

        // Collect all set applications
		for attr in self.tcx.get_attrs_by_path(def_id, &self.pass_attr) {
            let Pass { left, set, right } = self.parse_attr_pass(attr)?;

            debug!("PASS {} {}", left.0, set);

            // Make sure all variables on the right exist
            self.ensure_variables_exist(&set, &right)?;

            if left.0 == "->" {
                if let Err(e) = ret_ty.pass(&self.scx, set.clone()) {
                    match e {
                        PassError::BoundMismatch(bound) => return Err(self.tcx.dcx().span_err(left.1, format!("Provided set {set} is not a subset of {bound}"))),
                        PassError::Unexpected => return Err(self.tcx.dcx().span_err(left.1, format!("No more set applications are expected")))
                    }
                }

                continue;
            }

            match self.str_to_hir.get(&left.0) {
                Some(hir_id) => {
                    let ty = self.hir_to_ty.get_mut(hir_id).unwrap();

                    if let Err(e) = ty.pass(&self.scx, set.clone()) {
                        match e {
                            PassError::BoundMismatch(bound) => return Err(self.tcx.dcx().span_err(left.1, format!("Provided set {set} is not a subset of {bound}"))),
                            PassError::Unexpected => return Err(self.tcx.dcx().span_err(left.1, format!("No more set applications are expected")))
                        }
                    }
                },

                None => return Err(self.tcx.dcx().span_err(left.1, format!("Identifier does not exist")))
            }
        }

        let expr = body.value;
        let actual = self.check_expr(&body.value, Some(&ret_ty), false)?;

        debug!("Expect return type of... {:?}", ret_ty);
        debug!("Found return type of... {:?}", actual);

        if !actual.satisfies(&self.scx, &ret_ty) {
            debug!("Type err found");
        }

        // debug!("Params...");
        // for param in body.params {
        //     let param_ty = self.tcx.typeck(local_def_id).node_type(param.hir_id);

            
            
        //     debug!("- {:#?} : {param_ty}", param.pat.simple_ident());
        // }

        // Exit this scope
        self.scx.exit();

        Ok(())
    }

    fn display_str_to_hir(&self) {
        println!("======");
        for (str, hir) in &self.str_to_hir {
            println!("{} ↦ {}", str, hir);
        }
        println!("======");
    }

    fn display_hir_to_ty(&self) {
        println!("======");
        for (hir_id, ty) in &self.hir_to_ty {
            println!("{} ↦ {:?}", hir_id, ty);
        }
        println!("======");
    }

    fn top_type(&self) -> Type {
        Type::opaque()
    }

    fn bottom_type(&self) -> Type {
        // TODO: This type should be influenced by the current influence context and have only the origin in its sets
        Type::opaque()
    }

    fn set_type_constr(&mut self, def_id: DefId, ty: Type) {
        self.ty_constructors.insert(def_id, ty);
    }

    fn get_type_constr(&mut self, def_id: DefId) -> Type {
        match self.ty_constructors.get(&def_id) {
            Some(ty) => ty.clone(),
            None => self.top_type()
        }
    }

    fn get_constr_for_ty(&mut self, ty: &Ty<'tcx>) -> Type {
        // TODO: Actually get the constructor
        match ty.kind() {
            TyKind::Adt(adt_def, substs) => {
                // Structs, enums, unions (Vec, HashMap, your types, etc.)
                let type_def_id = adt_def.did();
                println!("ADT DefId: {:?}", type_def_id);

                return self.get_type_constr(type_def_id);
            }
            TyKind::Int(int_ty) => {
                // i8, i16, i32, i64, i128, isize
                println!("Integer type: {:?}", int_ty);
            }
            TyKind::Uint(uint_ty) => {
                // u8, u16, u32, u64, u128, usize
                println!("Unsigned integer type: {:?}", uint_ty);
            }
            TyKind::Float(float_ty) => {
                // f32, f64
                println!("Float type: {:?}", float_ty);
            }
            TyKind::Bool => {
                println!("Bool type");
            }
            TyKind::Char => {
                println!("Char type");
            }
            TyKind::Str => {
                // The str slice type (not String)
                println!("Str type");
            }
            TyKind::Ref(region, ty, mutability) => {
                // References like &T or &mut T
                println!("Reference to {:?}", ty);
            }
            TyKind::Tuple(types) => {
                // Tuples like (i32, String)
                println!("Tuple with {} elements", types.len());
            }
            _ => {
                println!("Other type kind: {:?}", ty.kind());
            }
        };

        self.top_type()
    }

    fn ensure_variables_exist(&self, set: &Set, spans: &HashMap<String, Span>) -> Result {
        match set {
            Set::Variable(v) => {
                if self.scx.get(&v).is_none() {
                    return Err(self.tcx.dcx().span_err(spans[v], format!("variable {v} has not been introduced")))
                }

                Ok(())
            },

            Set::Concrete(_) => Ok(()),
            Set::Universe => Ok(()),

            Set::Union(s) => {
                for set in s {
                    self.ensure_variables_exist(set, spans)?;
                }

                Ok(())
            },
        }
    }

	pub fn check_item(&mut self, item: &Item) -> Result {
		let def_id = item.owner_id.to_def_id();

		debug!("Getting attributes for item {:?}", def_id);

        match item.kind {
            ItemKind::Fn { sig, body, .. } => self.check_item_fn(&sig, body),
            ItemKind::Struct(var_data, _) => {
                let fields: Vec<FieldDef> = var_data
                    .fields()
                    .iter()
                    .map(|f| FieldDef {
                        did: f.def_id.to_def_id(),
                        name: f.ident.name,
                        vis: self.tcx.visibility(f.def_id),
                        safety: f.safety,
                        value: None,
                    })
                    .collect();

                self.check_item_struct(def_id, &fields)
            }

            _ => Ok(())
        }
	}

    fn check_item_struct(&mut self, def_id: DefId, field_defs: &[FieldDef]) -> Result {
        let struct_name = self.tcx.item_name(def_id);

        // Get type parameters (generics)
        let generics = self.tcx.generics_of(def_id);

        let mut vars = Vec::new();
        
        // Iterate over type parameters
        for param in &generics.own_params {
            let param_name = param.name;
            vars.push(param_name.to_string());

            match param.kind {
                ty::GenericParamDefKind::Type { .. } => {
                    
                    // Do something with type parameter
                    println!("TYPE PARAM: {}", param_name);

                }
                ty::GenericParamDefKind::Lifetime => {
                    // Handle lifetime parameters
                    println!("LIFETIME PARAM: {}", param_name);
                }
                ty::GenericParamDefKind::Const { .. } => {
                    // Handle const parameters
                    println!("CONST: {}", param_name);
                }
            }
        }

        if vars.is_empty() {
            println!("Checking {}", struct_name);
        } else {
            println!("Checking {}<{}>", struct_name, vars.join(", "))
        }

        // Collect all 'take' attributes
                // Enter a new scope in the set context
        self.scx.enter();

        // Collect all set variables introduced by this function
		for attr in self.tcx.get_attrs_by_path(def_id, &self.take_attr) {
            let take = self.parse_attr_take(attr)?;

            debug!("Introducing {} ⊆ {}", take.left.0, take.bound);

            // Make sure this variable hasn't been introduced yet
            if let Some(bound) = self.scx.get(&take.left.0) {
                return Err(self.tcx.dcx().span_err(take.left.1, format!("Variable has already been introduced with upper bound {bound}")))
            }

            // Make sure all variables in the bound exist
            self.ensure_variables_exist(&take.bound, &take.right)?;
            
            // Add upper bound to set variable context
            self.scx.set(take.left.0, take.bound);            
		}

        // Collect all 'pass' attributes on every field
        let mut fields = HashMap::new();

        for field in field_defs {
            let mut ty = Type::opaque();

            for attr in self.tcx.get_attrs_by_path(field.did, &self.pass_attr) {
                let Pass { left, set, right } = self.parse_attr_pass(attr)?;

                self.ensure_variables_exist(&set, &right)?;

                if let Err(e) = ty.pass(&self.scx, set.clone()) {
                    match e {
                        PassError::BoundMismatch(bound) => return Err(self.tcx.dcx().span_err(left.1, format!("Provided set {set} is not a subset of {bound}"))),
                        PassError::Unexpected => return Err(self.tcx.dcx().span_err(left.1, format!("No more set applications are expected")))
                    }
                }
            }

            fields.insert(field.name.to_string(), ty);
        }

        let ty = Type::record(fields);

        println!("STRUCT: {}", ty);

        self.set_type_constr(def_id, ty);

        self.scx.exit();

        Ok(())
    }

    // ======== EXPRESSIONS ======== //

    /// Serves as the type checking entrypoint for all expressions.
    fn check_expr(&mut self, expr: &Expr, expectation: Option<&Type>, is_lvalue: bool) -> Result<Type> {
        debug!("expr is {:#?}", expr);

        let ty = match expr.kind {
            ExprKind::Path(qpath) => return self.check_expr_path(expr.hir_id, &qpath, expectation),

            ExprKind::Block(block, _) => self.check_expr_block(block, expectation)?,
            _ => Type::opaque()
        };

        let actual = if is_lvalue { ty } else { /* influence */ ty };

        debug!("Expected - {:?}", expectation);
        debug!("Actual - {:?}", actual);
        
        match expectation {
            Some(expected) => {
                if actual.satisfies(&self.scx, expected) {
                    Ok(actual)
                } else {
                    let msg = format!("expected '{}' but found '{}'", actual, expected);
                    Err(self.tcx.dcx().span_err(expr.span, msg))
                }
            }

            None => Ok(actual),
        }
    }

    /// Checks the type of a block.
    fn check_expr_block(&mut self, block: &Block, expectation: Option<&Type>) -> Result<Type> {
        // The number of statements in this block;
        let len = block.stmts.len();

        match block.expr {
            Some(expr) => {
                for stmt in block.stmts {
                    self.check_stmt(stmt, None)?;
                }

                // This expression occurs at the very end without a semicolon
                self.check_expr(expr, expectation, false)
            }

            None if !block.stmts.is_empty() => {
                // Check all statements except the last one without expectation
                for stmt in block.stmts[..len - 1].iter() {
                    self.check_stmt(stmt, None)?;
                }

                self.check_stmt(&block.stmts.last().unwrap(), expectation)
            }

            _ => {
                let b = Type::opaque();
                debug!("block has no stmts, so type is bottom: {:?}", b);

                Ok(b)
            }
        }
    }

    /// Checks the type of a path expression.
    fn check_expr_path(&mut self, hir_id: HirId, qpath: &QPath, expectation: Option<&Type>) -> Result<Type> {
        let local_def_id = hir_id.owner.to_def_id().as_local().unwrap();

        let res = self.tcx.typeck(local_def_id).qpath_res(qpath, hir_id);

        let ty = match res {
            // TODO: Since some locals may not have bindings, put this access in a function that returns a default value
            Res::Local(hir_id) => self.hir_to_ty[&hir_id].clone(),

            _ => {
                todo!()
            }
        };

        let actual = ty;

        match expectation {
            Some(ty) => {
                if actual.satisfies(&self.scx, ty) {
                    Ok(actual)
                } else {
                    let msg = format!("expected {} found {}", ty, actual);
                    Err(self.tcx.dcx().span_err(qpath.span(), msg))
                }
            }

            None => Ok(actual),
        }
    }

    /// Checks the type of a statement.
    fn check_stmt(&mut self, stmt: &Stmt, expectation: Option<&Type>) -> Result<Type> {
        // TODO: Think more about what type should be returned by a statement
        match stmt.kind {
            StmtKind::Let(local) => {
                self.check_let_stmt(local)?;
                Ok(self.bottom_type())
            }
            StmtKind::Expr(expr) => self.check_expr(expr, expectation, false),
            StmtKind::Semi(expr) => self.check_expr(expr, expectation, false),
            StmtKind::Item(item_id) => {
                let item = self.tcx.hir_item(item_id);
                self.check_item(item)?;
                Ok(self.bottom_type())
            }
        }
    }

    fn check_let_stmt(&mut self, local: &LetStmt) -> Result {
        let mut processed = false;

        for attr in self.tcx.hir().attrs(local.hir_id) {
            if attr.path_matches(&self.pass_attr) {
                let Pass { left, set, right } = self.parse_attr_pass(attr)?;

                self.ensure_variables_exist(&set, &right)?;

                if let Some(expr) = local.init {
                    let ty = self.check_expr(expr, None, false)?;
                    self.process_pattern(local.pat.kind, ty, local.pat.hir_id);

                } else {
                    self.process_pattern(local.pat.kind, self.top_type(), local.pat.hir_id);
                }

                processed = true;
            }
        }

        if !processed {
            // There was no tag, so we cannot make any assumptions about the intended type
            if let Some(expr) = local.init {
                self.check_expr(expr, None, false)?;
            }

            self.process_pattern(local.pat.kind, self.icx.influence(self.top_type()), local.pat.hir_id);
        }

        Ok(())
    }

    fn parse_attr_take(&self, attr: &Attribute) -> Result<Take> {
        let AttrKind::Normal(normal) = &attr.kind else {
            unreachable!()
        };

        let psess = create_psess(&self.tcx);

        if let AttrArgs::Delimited(delim_args) = normal.args.clone() {
            let mut parser = create_parser(&psess, delim_args.tokens);

            parser
                .parse_take()
                .map_err(|err| self.tcx.dcx().span_err(err.span.clone(), "failed to parse"))
        } else {
            panic!()
        }
    }

    fn parse_attr_pass(&self, attr: &Attribute) -> Result<Pass> {
        let AttrKind::Normal(normal) = &attr.kind else {
            unreachable!()
        };

        let psess = create_psess(&self.tcx);

        if let AttrArgs::Delimited(delim_args) = normal.args.clone() {
            let mut parser = create_parser(&psess, delim_args.tokens);

            parser
                .parse_pass()
                .map_err(|err| self.tcx.dcx().span_err(err.span.clone(), "failed to parse"))
        } else {
            panic!()
        }
    }

    // ======== PATTERNS ======== //

    // fn process_pattern(&mut self, pat_kind: PatKind, ty: Type) {
    //     match pat_kind {
    //         PatKind::Binding(_, hir_id, ident, _) => {
    //             self.str_to_hir.insert(ident.to_string(), hir_id);
    //             self.hir_to_ty.insert(hir_id, ty.clone());
    //         }

    //         _ => {}
    //     };
    // }

    fn process_pattern(&mut self, pat_kind: PatKind, ty: Type, hir_id: HirId) {
        self.icx.enter(&ty.intrinsic[2]);

        match pat_kind {
            PatKind::Binding(_, hir_id, _, _) => {
                // TODO: Figure out the HirId <-> String mapping
                self.vctx.set(hir_id, ty.clone());
            }

            PatKind::Guard(pat, _) => self.process_pattern(pat.kind, ty, pat.hir_id),

            PatKind::Struct(qpath, fields, _) => self.process_pattern_struct(hir_id, &qpath, fields),

            PatKind::Box(pat) => self.process_pattern(pat.kind, ty, pat.hir_id),
            PatKind::Deref(pat) => self.process_pattern(pat.kind, ty, pat.hir_id),
            PatKind::Err(_) => {}

            PatKind::TupleStruct(qpath, pats, _) => {
                let fields: Vec<PatField> = pats
                    .iter()
                    .enumerate()
                    .map(|(i, pat)| PatField {
                        hir_id: pat.hir_id,
                        ident: Ident::from_str(&i.to_string()),
                        pat,
                        is_shorthand: false,
                        span: pat.span,
                    })
                    .collect();

                self.process_pattern_struct(hir_id, &qpath, &fields);
            }

            PatKind::Tuple(pats, _) => match ty.kind {
                TypeKind::Tuple(elements) => {
                    for (pat, ty) in pats.iter().zip(elements) {
                        self.process_pattern(pat.kind, ty, pat.hir_id);
                    }
                }

                _ => {
                    // Either the type is opaque or it's simply incorrect
                    let top = self.top_type();

                    for pat in pats.iter() {
                        self.process_pattern(pat.kind, top.clone(), pat.hir_id);
                    }
                }
            },

            PatKind::Ref(pat, _) => self.process_pattern(pat.kind, ty, pat.hir_id),

            PatKind::Or(pats) => {
                for pat in pats {
                    self.process_pattern(pat.kind, ty.clone(), pat.hir_id);
                }
            }

            // TODO: Test thoroughly, and see what happens when the type isn't an array type
            PatKind::Slice(before_pats, pat, after_pats) => {
                for pat in before_pats {
                    self.process_pattern(pat.kind, ty.clone(), pat.hir_id);
                }

                if let Some(pat) = pat {
                    self.process_pattern(pat.kind, ty.clone(), pat.hir_id);
                }

                for pat in after_pats {
                    self.process_pattern(pat.kind, ty.clone(), pat.hir_id);
                }
            }

            // Nothing to process for now (I've explicitly listed them in case we ever
            // want to know what the other cases are)
            PatKind::Range(_, _, _) | PatKind::Wild | PatKind::Never | PatKind::Expr(_) => {}
        };

        self.icx.exit();
    }

    fn extract(&self, child: &Type, parent: &Type) -> Type {
        todo!()
    }

        /// Recursively process a struct pattern, registering all identifiers with the locals map.
    fn process_pattern_struct(&mut self, hir_id: HirId, qpath: &QPath, fields: &[PatField]) {
        match self.check_expr_path(hir_id, qpath, None) {
            Ok(ty) => match &ty.kind {
                TypeKind::Rec(map) => {
                    for field in fields {
                        let ty = self.extract(&map[&field.ident.to_string()], &ty);
                        self.process_pattern(field.pat.kind, ty, field.pat.hir_id);
                    }
                }

                TypeKind::Opaque => {
                    for field in fields {
                        let ty = self.top_type();
                        self.process_pattern(field.pat.kind, ty, field.pat.hir_id);
                    }
                }

                _ => todo!(),
            },

            Err(_) => {
                warn!("Pattern struct processing failing silently");
            }
        }
    }
}