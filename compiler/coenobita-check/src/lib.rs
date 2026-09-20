#![feature(rustc_private)]

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

mod cx;

use crate::cx::{Ctx, InfCtx};

use coenobita_middle::set::{Set, SetCtx};
use coenobita_middle::ty::{PassError, ProvType, SetVar, Type, TypeKind};
use coenobita_parse::parse::CoenobitaParser;
use coenobita_parse::{create_parser, create_psess, Integrity, Param, Spanned};
use coenobita_parse::{Field, Input, Other};

use std::collections::{BTreeSet, HashMap};
use std::fs;
use std::path::Path;

use itertools::Itertools;
use log::{debug, warn};
use serde::Deserialize;

use rustc_abi::{VariantIdx, FIRST_VARIANT};
use rustc_errors::PResult;
use rustc_hir::def::{CtorOf, DefKind, Res};
use rustc_hir::def_id::DefId;
use rustc_hir::{
    Arm, AttrArgs, AttrKind, Attribute, Block, BodyId, Closure, Expr, ExprField, ExprKind, FnSig, HirId,
    ImplItem, ImplItemKind, Item, ItemKind, LangItem, LetExpr, LetStmt, MatchSource, PatField, PatKind, QPath, Stmt, StmtKind,
};
use rustc_middle::ty::inherent::IntoKind;
use rustc_middle::ty::{ClauseKind, FieldDef, GenericArgKind, Ty, TyCtxt, TyKind};
use rustc_span::{ErrorGuaranteed, Ident, Span, Symbol};

pub type Result<T = ()> = std::result::Result<T, ErrorGuaranteed>;

#[derive(Debug, Deserialize)]
struct AdHocTraitPolicy {
    name: String,
    args: Vec<String>,
    allowed: Vec<String>,
    denied: Vec<String>,
}

fn erase_generic_segments(path: &str) -> String {
    let mut erased = String::with_capacity(path.len());
    let mut depth = 0;

    for ch in path.chars() {
        match ch {
            '<' => depth += 1,
            '>' if depth > 0 => depth -= 1,
            _ if depth == 0 => erased.push(ch),
            _ => {}
        }
    }

    erased.replace("::::", "::")
}

pub struct Checker<'tcx> {
    tcx: TyCtxt<'tcx>,

    param_attr: Vec<Symbol>,
    input_attr: Vec<Symbol>,
    output_attr: Vec<Symbol>,
    field_attr: Vec<Symbol>,
    local_attr: Vec<Symbol>,

    scx: SetCtx,

    str_to_hir: HashMap<String, HirId>,
    hir_to_ty: HashMap<HirId, Type>,

    items: HashMap<DefId, Type>,

    vctx: Ctx<HirId, Type>,

    icx: InfCtx,

    crate_name: String,

    fn_decls: HashMap<String, Type>,

    trait_policies: Vec<AdHocTraitPolicy>,
}

impl<'tcx> Checker<'tcx> {
    pub fn new(crate_name: String, tcx: TyCtxt<'tcx>) -> Self {
        // Collect function declarations
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .join("declarations")
            .join("fn.d.rs");

        // TODO: Handle parsing failures
        let fn_decls_raw = fs::read_to_string(&path).unwrap();
        let fn_decls = coenobita_decl::parse(&fn_decls_raw).unwrap();

        let trait_policies = Self::load_trait_policies();

        Checker {
            tcx,

            param_attr: vec![Symbol::intern("cnbt"), Symbol::intern("parameter")],
            output_attr: vec![Symbol::intern("cnbt"), Symbol::intern("output")],
            input_attr: vec![Symbol::intern("cnbt"), Symbol::intern("input")],
            local_attr: vec![Symbol::intern("cnbt"), Symbol::intern("local")],
            field_attr: vec![Symbol::intern("cnbt"), Symbol::intern("field")],

            scx: SetCtx::new(),

            str_to_hir: HashMap::new(),
            hir_to_ty: HashMap::new(),

            items: HashMap::new(),

            vctx: Ctx::new(),

            icx: InfCtx::new(crate_name.clone()),

            crate_name,

            fn_decls,

            trait_policies,
        }
    }

    fn load_trait_policies() -> Vec<AdHocTraitPolicy> {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .join("intrinsics")
            .join("ad-hoc")
            .join("traits.json");

        let Ok(raw) = fs::read_to_string(&path) else {
            warn!("Could not read ad-hoc trait policy file at {}", path.display());
            return Vec::new();
        };

        match serde_json::from_str(&raw) {
            Ok(policies) => policies,
            Err(err) => {
                warn!("Could not parse ad-hoc trait policy file at {}: {err}", path.display());
                Vec::new()
            }
        }
    }

    // ======== NONLOCAL ITEMS ======== //

    fn check_item_fn_nonlocal(&mut self, def_id: DefId) -> Result {
        let n = self.tcx.fn_sig(def_id).skip_binder().inputs().iter().count();

        let mut default = Type::fun(n);
        let origin = self.origin_for_def(def_id);
        let set = Set::Concrete(BTreeSet::from([origin]));
        default.intrinsic = [set.clone(), set.clone(), set.clone()];

        if let TypeKind::Fn(_, rty) = &mut default.kind {
            rty.intrinsic = [set.clone(), set.clone(), set];
        }

        self.apply_fn_integrity_attrs(def_id, &mut default)?;

        self.items.insert(def_id, default);

        Ok(())
    }

    fn check_item_fn(&mut self, _: &FnSig, body_id: BodyId) -> Result {
        let def_id = body_id.hir_id.owner.to_def_id();
        let local_def_id = def_id.as_local().unwrap();

        // Enter a new scope in the set context
        self.scx.enter();

        let mut binder = Vec::new();
        let mut var_to_idx = HashMap::new();

        // Collect all set variables introduced by this function
        for attr in self.tcx.get_attrs_by_path(def_id, &self.param_attr) {
            let Param { right, bound, left } = self.parse_param(attr)?;

            // Make sure this variable hasn't been introduced yet
            if let Some(bound) = self.scx.get(&left.value) {
                return Err(self.tcx.dcx().span_err(
                    left.span,
                    format!("variable already introduced with upper bound {bound}"),
                ));
            }

            // Make sure all variables in the bound exist
            self.ensure_variables_exist(&bound.value, &right)?;

            // Add upper bound to set variable context
            self.scx.set(left.value.clone(), bound.value.clone());

            // Add set variable to binder
            binder.push(SetVar::new(left.value.clone(), bound.value));
            var_to_idx.insert(left.value, binder.len() - 1);
        }

        let mut params = Vec::new();

        let body = self.tcx.hir_body(body_id);

        // Get the type for every parameter
        for param in body.params.iter() {
            let raw = self.tcx.typeck(local_def_id).node_type(param.hir_id);
            let ty = self.get_constr_for_ty(&raw);

            params.push(ProvType::new(ty.clone()));
        }

        let fn_sig = self.tcx.fn_sig(def_id).skip_binder();
        let return_ty = fn_sig.output();

        let mut rty = self.get_constr_for_ty(&return_ty.skip_binder());

        // Collect all input annotations
        for attr in self.tcx.get_attrs_by_path(def_id, &self.input_attr) {
            let Input {
                index,
                integrity,
                providers,
                variables,
            } = self.parse_input(attr)?;

            for ss in integrity {
                self.pass(&mut params[index.value].ty, index.span, ss.value, &variables)?;

                // TODO: Consider provider tracking more carefully
                self.ensure_variables_exist(&providers.value, &variables)?;
                params[index.value].providers = providers.value.clone();
            }
        }

        for attr in self.compat_attrs_by_path(def_id, "input") {
            let Input {
                index,
                integrity,
                providers,
                variables,
            } = self.parse_input(attr)?;

            for ss in integrity {
                self.pass(&mut params[index.value].ty, index.span, ss.value, &variables)?;

                self.ensure_variables_exist(&providers.value, &variables)?;
                params[index.value].providers = providers.value.clone();
            }
        }

        // TODO: Only collect one output annotation
        for attr in self.tcx.get_attrs_by_path(def_id, &self.output_attr) {
            let Other { integrity, variables } = self.parse_output(attr)?;

            for (i, ss) in integrity.iter().enumerate() {
                self.ensure_variables_exist(&ss.value, &variables)?;
                rty.intrinsic[i] = ss.value.clone();
            }
        }

        for attr in self.compat_attrs_by_path(def_id, "output") {
            let Other { integrity, variables } = self.parse_output(attr)?;

            for (i, ss) in integrity.iter().enumerate() {
                self.ensure_variables_exist(&ss.value, &variables)?;
                rty.intrinsic[i] = ss.value.clone();
            }
        }

        let mut fty_from_public = Type {
            kind: TypeKind::Fn(params.clone(), Box::new(rty.clone())),
            var_to_idx: var_to_idx.clone(),
            binder: binder.clone(),
            binder_idx: 0,
            intrinsic: [Set::Universe, Set::Universe, Set::Universe],
            intrinsic_idx: 0,
        };
        self.apply_fn_integrity_attrs(def_id, &mut fty_from_public)?;
        if let TypeKind::Fn(public_params, public_rty) = fty_from_public.kind {
            params = public_params;
            rty = *public_rty;
        }

        // Now that all the parameter types have their integrity triples filled, process the patterns
        for (i, param) in body.params.iter().enumerate() {
            self.process_pattern(param.pat.kind, params[i].ty.clone(), param.pat.hir_id);
        }

        self.check_expr(&body.value, Some(&rty), false)?;

        // Build the function type from scratch
        let kind = TypeKind::Fn(params, Box::new(rty));
        let set = Set::Concrete(BTreeSet::from([self.crate_name.clone()]));

        let ty = Type {
            kind,
            var_to_idx,
            binder,
            binder_idx: 0,
            intrinsic: [set.clone(), set.clone(), set.clone()],
            intrinsic_idx: 0,
        };

        self.items.insert(def_id, ty);

        // Exit this scope
        self.scx.exit();

        Ok(())
    }

    // ======== TYPES ======== //

    /// Given the `DefId` of a function, return its type.
    fn fn_ty(&mut self, def_id: DefId) -> Type {
        let canonical_path = self.tcx.def_path_str(def_id);

        if self.fn_decls.contains_key(&canonical_path) {
            return self.fn_decls[&canonical_path].clone();
        }

        let erased_path = erase_generic_segments(&canonical_path);
        if self.fn_decls.contains_key(&erased_path) {
            return self.fn_decls[&erased_path].clone();
        }

        if let Some(ty) = self.items.get(&def_id) {
            return ty.clone();
        }

        // We haven't processed the definition of this function yet
        let _ = self.check_item_fn_nonlocal(def_id);
        self.items.get(&def_id).unwrap().clone()
    }

    fn origin_for_def(&self, def_id: DefId) -> String {
        self.tcx
            .def_path_str(def_id)
            .split("::")
            .next()
            .unwrap_or("*")
            .to_string()
    }

    /// Given the parent and variant `DefId`s of an ADT (as well as its index), return its type.
    fn adt_ty(&mut self, def_id_parent: DefId, def_id_variant: DefId, index: VariantIdx) -> Type {
        if let Some(ty) = self.get_type_constr(def_id_variant) {
            return ty.clone();
        }

        // We haven't processed the definition of this function yet
        let field_defs = &self
            .tcx
            .adt_def(def_id_parent)
            .variants()
            .get(index)
            .unwrap()
            .fields
            .raw;

        let _ = self.check_item_struct(def_id_variant, field_defs);
        self.get_type_constr(def_id_variant).unwrap().clone()
    }

    fn top_type(&self) -> Type {
        Type::opaque()
    }

    fn bottom_type(&self) -> Type {
        // TODO: This type should be influenced by the current influence context and have only the origin in its sets
        let mut ty = Type::opaque();

        let set = Set::Concrete(BTreeSet::from([self.crate_name.clone()]));

        ty.intrinsic = [set.clone(), set.clone(), set];

        self.icx.influence(ty)
    }

    fn set_type_constr(&mut self, def_id: DefId, ty: Type) {
        self.items.insert(def_id, ty);
    }

    fn get_type_constr(&mut self, def_id: DefId) -> Option<&Type> {
        self.items.get(&def_id)
    }

    fn get_constr_for_ty(&mut self, ty: &Ty<'tcx>) -> Type {
        // TODO: Actually get the constructor
        match ty.kind() {
            TyKind::Adt(adt_def, _) => {
                // Structs, enums, unions (Vec, HashMap, your types, etc.)
                let type_def_id = adt_def.did();

                return self
                    .get_type_constr(type_def_id)
                    .unwrap_or(&Type::opaque())
                    .clone();
            }
            TyKind::Int(_) => {}
            TyKind::Uint(_) => {}
            TyKind::Float(_) => {}
            TyKind::Bool => {}
            TyKind::Char => {}
            TyKind::Str => {}
            TyKind::Tuple(_) => {}
            TyKind::Ref(_, ty, _) => {
                // References like &T or &mut T
                return self.get_constr_for_ty(ty);
            }
            _ => {
                debug!("Other type kind: {:?}", ty.kind());
            }
        };

        self.top_type()
    }

    fn ensure_variables_exist(&self, set: &Set, spans: &HashMap<String, Span>) -> Result {
        match set {
            Set::Variable(v) => {
                if self.scx.get(&v).is_none() {
                    return Err(self
                        .tcx
                        .dcx()
                        .span_err(spans[v], format!("variable {v} has not been introduced")));
                }

                Ok(())
            }

            Set::Concrete(_) => Ok(()),
            Set::Universe => Ok(()),

            Set::Union(s) => {
                for set in s {
                    self.ensure_variables_exist(set, spans)?;
                }

                Ok(())
            }
        }
    }

    fn compat_attrs_by_path(&self, def_id: DefId, name: &str) -> Vec<&'tcx Attribute> {
        let path = [Symbol::intern("coenobita"), Symbol::intern(name)];
        self.tcx.get_attrs_by_path(def_id, &path).collect()
    }

    fn tool_attrs_by_path(&self, def_id: DefId, name: &str) -> Vec<&'tcx Attribute> {
        let cnbt = [Symbol::intern("cnbt"), Symbol::intern(name)];
        let coenobita = [Symbol::intern("coenobita"), Symbol::intern(name)];

        self.tcx
            .get_attrs_by_path(def_id, &cnbt)
            .chain(self.tcx.get_attrs_by_path(def_id, &coenobita))
            .collect()
    }

    fn attr_matches(&self, attr: &Attribute, name: &str) -> bool {
        let cnbt = [Symbol::intern("cnbt"), Symbol::intern(name)];
        let coenobita = [Symbol::intern("coenobita"), Symbol::intern(name)];

        attr.path_matches(&cnbt) || attr.path_matches(&coenobita)
    }

    fn apply_fn_integrity_attrs(&self, def_id: DefId, fty: &mut Type) -> Result {
        for attr in self.tool_attrs_by_path(def_id, "integrity") {
            let Integrity::Fn {
                intrinsic: _,
                inputs,
                output,
                variables,
            } = self.parse_integrity(attr)?
            else {
                continue;
            };

            let TypeKind::Fn(params, rty) = &mut fty.kind else {
                continue;
            };

            for (param, integrity) in params.iter_mut().zip(inputs.iter()) {
                for (i, ss) in integrity.iter().enumerate() {
                    self.ensure_variables_exist(&ss.value, &variables)?;
                    param.ty.intrinsic[i] = ss.value.clone();
                }
            }

            for (i, ss) in output.iter().enumerate() {
                self.ensure_variables_exist(&ss.value, &variables)?;
                rty.intrinsic[i] = ss.value.clone();
            }
        }

        Ok(())
    }

    pub fn check_item(&mut self, item: &Item) -> Result {
        let def_id = item.owner_id.to_def_id();

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

            _ => Ok(()),
        }
    }

    pub fn check_impl_item(&mut self, item: &ImplItem) -> Result {
        match item.kind {
            ImplItemKind::Fn(sig, body) => self.check_item_fn(&sig, body),
            _ => Ok(()),
        }
    }

    fn pass(&self, ty: &mut Type, span: Span, set: Set, variables: &HashMap<String, Span>) -> Result {
        self.ensure_variables_exist(&set, variables)?;

        if let Err(e) = ty.pass(&self.scx, set.clone()) {
            match e {
                PassError::BoundMismatch(bound) => {
                    return Err(self
                        .tcx
                        .dcx()
                        .span_err(span, format!("Provided set {set} is not a subset of {bound}")))
                }

                PassError::Unexpected => {
                    return Err(self
                        .tcx
                        .dcx()
                        .span_err(span, format!("No more set applications are expected")))
                }
            }
        }

        Ok(())
    }

    fn check_item_struct(&mut self, def_id: DefId, field_defs: &[FieldDef]) -> Result {
        let mut fields = HashMap::new();

        // Collect all 'pass' attributes on every field
        for field in field_defs {
            let mut pty = ProvType::default();

            for attr in self.tcx.get_attrs_by_path(field.did, &self.field_attr) {
                let Field {
                    integrity,
                    providers,
                    variables,
                } = self.parse_field(attr)?;

                for (i, ss) in integrity.iter().enumerate() {
                    self.ensure_variables_exist(&ss.value, &variables)?;

                    pty.ty.intrinsic[i] = ss.value.clone();
                }

                self.ensure_variables_exist(&providers.value, &variables)?;
                pty.providers = providers.value.clone();
            }

            for attr in self.compat_attrs_by_path(field.did, "field") {
                let Field {
                    integrity,
                    providers,
                    variables,
                } = self.parse_field(attr)?;

                for (i, ss) in integrity.iter().enumerate() {
                    self.ensure_variables_exist(&ss.value, &variables)?;

                    pty.ty.intrinsic[i] = ss.value.clone();
                }

                self.ensure_variables_exist(&providers.value, &variables)?;
                pty.providers = providers.value.clone();
            }

            for attr in self.tool_attrs_by_path(field.did, "integrity") {
                let integrity = self.parse_integrity(attr)?;

                if let Integrity::Other(Other { integrity, variables }) = integrity {
                    for (i, ss) in integrity.iter().enumerate() {
                        self.ensure_variables_exist(&ss.value, &variables)?;
                        pty.ty.intrinsic[i] = ss.value.clone();
                    }
                }
            }

            for attr in self.tool_attrs_by_path(field.did, "providers") {
                let providers = self.parse_providers(attr)?;
                pty.providers = providers.value;
            }

            fields.insert(field.name.to_string(), pty);
        }

        self.items.insert(def_id, Type::record(fields));

        Ok(())
    }

    // ======== EXPRESSIONS ======== //

    /// Serves as the type checking entrypoint for all expressions.
    fn check_expr(&mut self, expr: &Expr, expectation: Option<&Type>, is_lvalue: bool) -> Result<Type> {
        let ty = match expr.kind {
            ExprKind::ConstBlock(_) => todo!(),
            ExprKind::Become(_) => todo!(),
            ExprKind::Type(_, _) => todo!(),
            ExprKind::InlineAsm(_) => todo!(),
            ExprKind::OffsetOf(_, _) => todo!(),
            ExprKind::UnsafeBinderCast(_, _, _) => todo!(),
            ExprKind::Err(_) => todo!(),

            ExprKind::Lit(_) => self.bottom_type(),
            ExprKind::Break(_, _) => self.bottom_type(),
            ExprKind::Continue(_) => self.bottom_type(),

            // TODO: Think about the typing rules for `Unary`, `AddrOf`, and `Index`
            ExprKind::Unary(_, expr) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::DropTemps(expr) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::Cast(expr, _) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::Repeat(expr, _) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::Yield(expr, _) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::AddrOf(_, _, expr) => self.check_expr(expr, None, is_lvalue)?,
            ExprKind::Index(expr, _, _) => self.check_expr(expr, None, is_lvalue)?,

            ExprKind::Assign(dest, expr, _) => self.check_expr_assign(dest, expr)?,
            ExprKind::AssignOp(_, dest, expr) => self.check_expr_assign(dest, expr)?,

            ExprKind::Block(block, _) => self.check_expr_block(block, expectation)?,
            ExprKind::Loop(block, _, _, _) => self.check_expr_block(block, expectation)?,

            ExprKind::Path(qpath) => return self.check_expr_path(expr.hir_id, &qpath, expectation),
            ExprKind::Call(func, args) => self.check_expr_call(func, args, expr.span)?,
            ExprKind::Let(let_expr) => self.check_expr_let(let_expr, expectation)?,
            ExprKind::Binary(_, lhs, rhs) => self.check_expr_binary(lhs, rhs, expectation)?,
            ExprKind::Array(exprs) => self.check_expr_array(exprs)?,
            ExprKind::Tup(exprs) => self.check_expr_tup(exprs)?,
            ExprKind::Ret(expr) => self.check_expr_ret(expr, expectation)?,
            ExprKind::Closure(closure) => self.check_expr_closure(closure, expectation)?,

            // TODO: Think about the typing rules for projection
            ExprKind::Field(obj, ident) => self.check_expr_field(obj, ident)?,

            ExprKind::MethodCall(_, receiver, args, _) => {
                self.check_method_call(expr.hir_id, receiver, args, expr.span)?
            }
            ExprKind::If(guard, then_expr, else_expr) => {
                self.check_expr_if(guard, then_expr, else_expr, expectation)?
            }
            ExprKind::Match(guard, arms, source) => {
                self.check_expr_match(guard, arms, source, expectation)?
            }
            ExprKind::Struct(qpath, fields, _) => self.check_expr_struct(expr.hir_id, qpath, fields)?,
        };

        let actual = if is_lvalue { ty } else { self.icx.influence(ty) };

        match expectation {
            Some(expected) => {
                if actual.satisfies(&self.scx, expected) {
                    Ok(actual)
                } else {
                    let msg = format!("expected '{}' but found '{}'", expected, actual);
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

            Res::Def(def_kind, def_id) => {
                match def_kind {
                    // TODO: Check that `AssocFn` is handled properly
                    DefKind::Fn | DefKind::AssocFn => self.fn_ty(def_id),

                    DefKind::Struct => self.adt_ty(def_id, def_id, FIRST_VARIANT),

                    DefKind::Variant => {
                        // The variant DefId
                        let id = def_id;

                        // The parent DefId
                        let def_id = self.tcx.parent(def_id);

                        // The parent ADT definition
                        let adt = self.tcx.adt_def(def_id);

                        // The index of this variant in particular
                        let idx = adt
                            .variants()
                            .iter()
                            .enumerate()
                            .find(|(_, vdef)| vdef.def_id == id)
                            .unwrap()
                            .0;

                        self.adt_ty(def_id, id, idx.into())
                    }

                    // TODO: Test this thoroughly
                    DefKind::Ctor(ctor_of, _) => match ctor_of {
                        CtorOf::Struct => {
                            let def_id = self.tcx.parent(def_id);

                            self.adt_ty(def_id, def_id, FIRST_VARIANT)
                        }

                        CtorOf::Variant => {
                            // Get the DefId of the variant
                            let id = self.tcx.parent(def_id);

                            // Get the DefId of the enum holding this variant
                            let def_id = self.tcx.parent(id);
                            let adt = self.tcx.adt_def(def_id);

                            let idx = adt
                                .variants()
                                .iter()
                                .enumerate()
                                .find(|(_, vdef)| vdef.def_id == id)
                                .unwrap()
                                .0;

                            self.adt_ty(def_id, id, idx.into())
                        }
                    },

                    // TODO: Implement actual logic
                    DefKind::Static { .. } => self.bottom_type(),

                    // TODO: Implement actual logic
                    DefKind::AssocConst | DefKind::Const | DefKind::ConstParam => self.bottom_type(),

                    DefKind::Union => {
                        warn!("Silently skipping union usage");
                        self.bottom_type()
                    }

                    DefKind::TyAlias => {
                        let ty = self.tcx.type_of(def_id).skip_binder();
                        let kind = ty.kind();

                        match kind {
                            TyKind::Adt(adt_def, _) => {
                                self.adt_ty(adt_def.did(), adt_def.did(), FIRST_VARIANT)
                            }
                            _ => todo!(),
                        }
                    }

                    _ => todo!(),
                }
            }

            Res::SelfCtor(alias_to) | Res::SelfTyAlias { alias_to, .. } => {
                match self.tcx.type_of(alias_to).skip_binder().kind() {
                    TyKind::Adt(adt_def, _) => {
                        let did = adt_def.did();
                        self.adt_ty(did, did, FIRST_VARIANT)
                    }

                    _ => todo!(),
                }
            }

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

    /// Checks the type of a call expression.
    fn check_expr_call(&mut self, fun: &Expr, args: &[Expr], span: Span) -> Result<Type> {
        if self.is_trust_call(fun) {
            return self.check_expr_trust(args, span);
        }

        let owner_id = fun.hir_id.owner;
        let owner_def_kind = self.tcx.def_kind(owner_id);

        if owner_def_kind.is_fn_like() {
            let local_def_id = owner_id.to_def_id().as_local().unwrap();

            if let ExprKind::Path(qpath) = fun.kind {
                let typeck_results = self.tcx.typeck(local_def_id);

                if let Res::Def(_def_kind, def_id) = typeck_results.qpath_res(&qpath, fun.hir_id) {
                    self.check_ad_hoc_trait_constraints(def_id, args, fun.hir_id)?;

                    // TODO: Check intrinsic constraints
                    // self.check_intrinsic_constraints(def_id, def_kind, args, typeck_results)?;
                }
            }
        }

        let mut fty = self.check_expr(fun, None, false)?;

        // TODO: Reorganize this logic significantly... there is WAY too much cloning here
        let mut ty = match fty.clone().kind {
            TypeKind::Fn(arg_tys, _) => {
                let mut atys = Vec::new();

                for (expected, expr) in arg_tys.into_iter().zip(args) {
                    let actual = self.check_expr(expr, None, false)?;

                    atys.push(actual.clone());

                    for i in 0..3 {
                        if let Set::Variable(v) = expected.ty.intrinsic[i].clone() {
                            // This set is a variable that needs to be replaced... which one is it?
                            let index = fty.var_to_idx[&v];

                            let set_var = &fty.binder[index];

                            let candidate = actual.intrinsic[i].clone();

                            if !candidate.subset(&self.scx, &set_var.bound) {
                                let msg = format!(
                                    "argument has type {}, but {} ⊈ {} as required",
                                    actual, candidate, set_var.bound
                                );

                                return Err(self.tcx.dcx().span_err(expr.span, msg));
                            }

                            fty.binder[index].value = Some(actual.intrinsic[i].clone());
                            fty.replace(&v, &actual.intrinsic[i]);
                        }
                    }
                }

                let TypeKind::Fn(arg_tys, rty) = fty.kind.clone() else {
                    unreachable!()
                };

                for i in 0..args.len() {
                    let expr = &args[i];
                    let actual = &atys[i];
                    let expected = &arg_tys[i];

                    if !actual.satisfies(&self.scx, &expected.ty) {
                        let msg = format!("expected {} but found {}", expected, actual);

                        return Err(self.tcx.dcx().span_err(expr.span, msg));
                    }

                    // TODO: Construct this set once and access via helper method
                    if !Set::Concrete(BTreeSet::from([self.crate_name.clone()]))
                        .subset(&self.scx, &expected.providers)
                    {
                        let msg = format!(
                            "origin {} absent from set of valid providers {}",
                            self.crate_name, expected.providers
                        );

                        return Err(self.tcx.dcx().span_err(expr.span, msg));
                    }
                }

                *rty
            }

            TypeKind::Rec(elements) => {
                for (i, arg) in args.iter().enumerate() {
                    let ty = elements[&i.to_string()].clone().ty;
                    self.check_expr(arg, Some(&ty), false)?;
                }

                fty.clone()
            }

            _ => {
                warn!("Cannot tell if expression is a function - {:?}", fun);
                fty.clone()
            }
        };

        ty = self.extract(&ty, &fty);

        Ok(ty)
    }

    fn check_ad_hoc_trait_constraints(&self, def_id: DefId, args: &[Expr], fun_hir_id: HirId) -> Result {
        if self.trait_policies.is_empty() {
            return Ok(());
        }

        let typeck_results = self.tcx.typeck(fun_hir_id.owner.to_def_id().as_local().unwrap());
        let generic_args = typeck_results.node_args(fun_hir_id);
        let generics = self.tcx.generics_of(def_id);
        let predicates = self.tcx.predicates_of(def_id);

        for (clause, _) in predicates.predicates {
            let ClauseKind::Trait(trait_predicate) = clause.kind().skip_binder() else {
                continue;
            };

            let trait_ref = trait_predicate.trait_ref;
            let trait_name = self.short_def_name(trait_ref.def_id);

            let Some(policy) = self.trait_policies.iter().find(|policy| {
                policy.name == trait_name
                    && policy.args
                        == trait_ref
                            .args
                            .iter()
                            .skip(1)
                            .filter_map(|arg| match arg.kind() {
                                GenericArgKind::Type(ty) => Some(self.short_ty_name(ty)),
                                _ => None,
                            })
                            .collect::<Vec<_>>()
            }) else {
                continue;
            };

            let self_ty = trait_ref.self_ty();
            let TyKind::Param(param_ty) = self_ty.kind() else {
                continue;
            };

            let Some(param) = generics.own_params.iter().find(|param| param.name == param_ty.name) else {
                continue;
            };

            let Some(generic_arg) = generic_args.get(param.index as usize) else {
                continue;
            };

            let GenericArgKind::Type(actual_ty) = generic_arg.kind() else {
                continue;
            };

            let actual_name = self.short_ty_name(actual_ty);
            if policy.denied.iter().any(|denied| denied == &actual_name)
                || !policy.allowed.iter().any(|allowed| allowed == &actual_name)
            {
                let span = args
                    .get(param.index as usize)
                    .map(|arg| arg.span)
                    .unwrap_or_else(|| self.tcx.def_span(def_id));
                let expected = format!("{}<{}>", policy.name, policy.args.iter().join(", "));
                let allowed = policy
                    .allowed
                    .iter()
                    .map(|ty| format!("'{ty}'"))
                    .join(", ");

                let mut err = self.tcx.dcx().struct_span_err(
                    span,
                    format!(
                        "cannot accept type '{actual_name}' for '{expected}' due to capability safety policy"
                    ),
                );
                err.help(format!("allowed types are {allowed}"));
                return Err(err.emit());
            }
        }

        Ok(())
    }

    fn short_def_name(&self, def_id: DefId) -> String {
        self.tcx
            .def_path_str(def_id)
            .rsplit("::")
            .next()
            .unwrap_or("*")
            .to_string()
    }

    fn short_ty_name(&self, ty: Ty<'tcx>) -> String {
        match ty.kind() {
            TyKind::Adt(adt_def, _) => self.short_def_name(adt_def.did()),
            TyKind::Ref(_, inner, _) => self.short_ty_name(*inner),
            TyKind::Str => "str".to_string(),
            TyKind::Param(param) => param.name.to_string(),
            _ => format!("{ty}"),
        }
    }

    fn is_trust_call(&self, fun: &Expr) -> bool {
        let ExprKind::Path(qpath) = fun.kind else {
            return false;
        };

        let local_def_id = fun.hir_id.owner.to_def_id().as_local().unwrap();
        let typeck_results = self.tcx.typeck(local_def_id);

        let Res::Def(_, def_id) = typeck_results.qpath_res(&qpath, fun.hir_id) else {
            return false;
        };

        erase_generic_segments(&self.tcx.def_path_str(def_id)) == "coenobita::__trust"
    }

    fn check_expr_trust(&mut self, args: &[Expr], span: Span) -> Result<Type> {
        if self.crate_name != "root" {
            return Err(self
                .tcx
                .dcx()
                .span_err(span, "'trust' may only be invoked by the root crate"));
        }

        let Some(expr) = args.first() else {
            return Ok(self.top_type());
        };

        let mut ty = self.check_expr(expr, None, false)?;
        let origin = Set::Concrete(BTreeSet::from([self.crate_name.clone()]));
        ty.intrinsic = [origin.clone(), origin.clone(), origin];

        Ok(ty)
    }

    fn join(&self, types: Vec<Type>) -> Type {
        // TODO: Fail if type kinds are incompatible
        let mut it = types.iter();

        let mut res = it.next().unwrap().clone();

        while let Some(ty) = it.next() {
            res.intrinsic[0] = res.intrinsic[0].clone().union(ty.intrinsic[0].clone());
            res.intrinsic[1] = res.intrinsic[1].clone().union(ty.intrinsic[1].clone());
            res.intrinsic[2] = res.intrinsic[2].clone().union(ty.intrinsic[2].clone());
        }

        res
    }

    /// Checks the type of a method call.
    fn check_method_call(
        &mut self,
        hir_id: HirId,
        receiver: &Expr,
        args: &[Expr],
        _span: Span,
    ) -> Result<Type> {
        let typeck_results = self.tcx.typeck(hir_id.owner.def_id);
        let (def_kind, def_id) = typeck_results.type_dependent_def(hir_id).unwrap();

        match def_kind {
            DefKind::AssocFn => {
                // let args = vec![receiver];
                let mut args_ = vec![receiver.clone()];
                args_.extend_from_slice(args);

                // TODO: Check intrinsic constraints
                // self.check_intrinsic_constraints(def_id, def_kind, &args_, typeck_results)?;

                let fty = self.fn_ty(def_id);
                // KEEP: I can use this to generate intrinsic annotations
                // let s = serde_json::to_string(&fty).unwrap();
                // debug!("{}", s);

                match fty.kind {
                    TypeKind::Fn(arg_tys, ret_ty) => {
                        // We subtract one to account for the receiver
                        if args.len() != arg_tys.len() - 1 {
                            warn!("arg ct doesnt match up");
                        }

                        let args = std::iter::once(receiver).chain(args.iter());
                        for (pty, expr) in arg_tys.into_iter().zip(args) {
                            self.check_expr(expr, Some(&pty.ty), false)?;
                        }

                        Ok(*ret_ty)
                    }

                    _ => todo!(),
                }
            }

            _ => todo!(),
        }
    }

    /// Checks the type of an `if` expression.
    fn check_expr_if(
        &mut self,
        guard: &Expr,
        then_expr: &Expr,
        else_expr: Option<&Expr>,
        expectation: Option<&Type>,
    ) -> Result<Type> {
        let ity = self.check_expr(guard, None, false)?;

        self.icx.enter(&ity.intrinsic[2]);

        let mut rty = self.check_expr(then_expr, expectation, false)?;

        if let Some(else_expr) = else_expr {
            let to_be_joined = self.check_expr(else_expr, expectation, false)?;
            rty = self.join(vec![rty, to_be_joined])
        }

        self.icx.exit();
        Ok(rty)
    }

    /// Checks the type of a `match` expression.
    fn check_expr_match(
        &mut self,
        guard: &Expr,
        arms: &[Arm],
        source: MatchSource,
        expectation: Option<&Type>,
    ) -> Result<Type> {
        let ity = match source {
            MatchSource::ForLoopDesugar => {
                // The guard expression is a function call, and we want the argument
                let ExprKind::Call(_, args) = guard.kind else {
                    unreachable!()
                };

                self.check_expr(&args[0], None, false)?
            }

            _ => self.check_expr(guard, None, false)?,
        };

        self.icx.enter(&ity.intrinsic[2]);

        let mut result = self.bottom_type();

        for arm in arms {
            self.process_pattern(arm.pat.kind, ity.clone(), arm.pat.hir_id);
            let to_be_joined = self.check_expr(arm.body, expectation, false)?;
            result = self.join(vec![result, to_be_joined]);
        }

        self.icx.exit();
        Ok(result)
    }

    /// Checks the type of a `let` expression. These typically appear as the guards of `if` expressions.
    fn check_expr_let(&mut self, let_expr: &LetExpr, expectation: Option<&Type>) -> Result<Type> {
        let ty = self.check_expr(let_expr.init, expectation, false)?;

        self.process_pattern(let_expr.pat.kind, ty.clone(), let_expr.pat.hir_id);

        Ok(ty)
    }

    /// Checks the type of a binary expression.
    fn check_expr_binary(&mut self, lhs: &Expr, rhs: &Expr, expectation: Option<&Type>) -> Result<Type> {
        let lty = self.check_expr(lhs, expectation, false)?;
        let rty = self.check_expr(rhs, expectation, false)?;
        let ty = self.join(vec![lty, rty]);

        Ok(ty)
    }

    /// Checks the type of an assignment expression.
    fn check_expr_assign(&mut self, dest: &Expr, expr: &Expr) -> Result<Type> {
        let expected = self.check_expr(dest, None, true)?;
        let ty = self.check_expr(expr, Some(&expected), false)?;

        Ok(ty)
    }

    /// Checks the type of a struct expression.
    fn check_expr_struct(&mut self, hir_id: HirId, qpath: &QPath, fields: &[ExprField]) -> Result<Type> {
        let ty = match qpath {
            QPath::LangItem(item, _) => match item {
                LangItem::Range => {
                    // NOTE: We handle ranges in a special manner for the time being
                    let mut result = self.bottom_type();

                    for field in fields {
                        let pending = self.check_expr(field.expr, None, false)?;
                        result = self.join(vec![result, pending]);
                    }

                    result
                }

                _ => {
                    warn!("Language item silently ignored");
                    self.bottom_type()
                }
            },

            _ => self.check_expr_path(hir_id, qpath, None)?,
        };

        match &ty.kind {
            TypeKind::Rec(tys) => {
                let mut result = self.bottom_type();
                let mut fields_ty = tys.clone();

                for field in fields {
                    let name = field.ident.to_string();
                    let expected = tys[&name].clone();
                    let actual = self.check_expr(field.expr, Some(&expected.ty), false)?;

                    if let Some(pty) = fields_ty.get_mut(&name) {
                        pty.ty = actual;
                    }
                }

                result.kind = TypeKind::Rec(fields_ty);
                return Ok(result);
            }

            TypeKind::Opaque => {
                for field in fields {
                    self.check_expr(field.expr, None, false)?;
                }
            }

            _ => todo!(),
        }

        Ok(ty)
    }

    /// Checks the type of an array expression.
    fn check_expr_array(&mut self, exprs: &[Expr]) -> Result<Type> {
        let mut result = self.bottom_type();
        let mut item = self.bottom_type();

        for expr in exprs {
            let pending = self.check_expr(expr, None, false)?;
            item = self.join(vec![item, pending]);
        }

        result.kind = TypeKind::Array(Box::new(ProvType::new(item)));

        Ok(result)
    }

    /// Checks the type of a tuple expression.
    fn check_expr_tup(&mut self, exprs: &[Expr]) -> Result<Type> {
        let mut result = self.bottom_type();

        let mut items = vec![];

        for expr in exprs {
            let pty = ProvType::new(self.check_expr(expr, None, false)?);
            items.push(pty);
        }

        result.kind = TypeKind::Tuple(items);
        Ok(result)
    }

    /// Checks the type of a return expression.
    fn check_expr_ret(&mut self, expr: Option<&Expr>, expectation: Option<&Type>) -> Result<Type> {
        match expr {
            Some(expr) => Ok(self.check_expr(expr, expectation, false)?),
            None => Ok(self.bottom_type()),
        }
    }

    /// Checks the type of a closure expression.
    fn check_expr_closure(&mut self, closure: &Closure, expectation: Option<&Type>) -> Result<Type> {
        let body = self.tcx.hir_body(closure.body);

        let mut params = Vec::new();
        let rty = match expectation {
            Some(Type {
                kind: TypeKind::Fn(expected_params, expected_rty),
                ..
            }) => {
                for (param, expected) in body.params.iter().zip(expected_params.iter()) {
                    self.process_pattern(param.pat.kind, expected.ty.clone(), param.pat.hir_id);
                }

                self.check_expr(&body.value, Some(expected_rty), false)?;
                params = expected_params.clone();
                *expected_rty.clone()
            }

            _ => {
                for param in body.params {
                    let ty = self.top_type();
                    self.process_pattern(param.pat.kind, ty.clone(), param.pat.hir_id);
                    params.push(ProvType::new(ty));
                }

                self.check_expr(&body.value, None, false)?
            }
        };

        let intrinsic = expectation
            .map(|ty| ty.intrinsic.clone())
            .unwrap_or_else(|| [Set::Universe, Set::Universe, Set::Universe]);

        Ok(Type {
            kind: TypeKind::Fn(params, Box::new(rty)),
            binder: vec![],
            var_to_idx: HashMap::new(),
            binder_idx: 0,
            intrinsic,
            intrinsic_idx: 0,
        })
    }

    /// Checks the type of a projection.
    fn check_expr_field(&mut self, object: &Expr, field: Ident) -> Result<Type> {
        match self.check_expr(object, None, false)?.kind {
            TypeKind::Rec(field_map) => {
                if !field_map.contains_key(&field.to_string()) {
                    let msg = format!("unknown field '{}'", field.name);
                    self.tcx.dcx().span_err(field.span, msg);
                    Ok(self.top_type())
                } else {
                    Ok(field_map[&field.to_string()].clone().ty)
                }
            }

            TypeKind::Tuple(fields) => {
                let index = field.name.as_str().parse::<usize>().unwrap();

                if index >= fields.len() {
                    let msg = format!("unknown field '{}'", field.name);
                    self.tcx.dcx().span_err(field.span, msg);
                    Ok(self.top_type())
                } else {
                    Ok(fields[index].clone().ty)
                }
            }

            TypeKind::Opaque => {
                // For now, any field access with an unknown receiver will be universal
                // However, in the future, I should refine my type checking to avoid this
                Ok(self.top_type())
            }

            _ => todo!(),
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
        let init_expectation = self.local_integrity_expectation(local)?;

        // TODO: We need to use a type constructor that matches the left type
        let mut ty = if let Some(expr) = local.init {
            self.check_expr(expr, init_expectation.as_ref(), false)?
        } else {
            self.top_type()
        };

        // if let Some(expr) = local.init {
        //     self.check_expr(expr, Some(&ty), false)?;
        //     self.process_pattern(local.pat.kind, ty, local.pat.hir_id);
        // } else {
        //     self.process_pattern(local.pat.kind, self.top_type(), local.pat.hir_id);
        // }

        // TODO: Only collect one such attribute
        for attr in self.tcx.hir().attrs(local.hir_id) {
            if self.attr_matches(attr, "local") {
                let Other { integrity, variables } = self.parse_local(attr)?;

                for (i, ss) in integrity.iter().enumerate() {
                    self.ensure_variables_exist(&ss.value, &variables)?;

                    if !ty.intrinsic[i].subset(&self.scx, &ss.value) {
                        let msg = format!(
                            "value has incompatible integrity {} since {} ⊈ {}",
                            ty.intrinsic.iter().join(" "),
                            ty.intrinsic[i],
                            ss.value
                        );

                        return Err(self.tcx.dcx().span_err(local.init.unwrap().span, msg));
                    }

                    ty.intrinsic[i] = ss.value.clone();
                }
            }
        }

        for attr in self.tcx.hir().attrs(local.hir_id) {
            if self.attr_matches(attr, "integrity") {
                self.apply_local_integrity_attr(local, &mut ty, attr)?;
            }
        }

        self.process_pattern(local.pat.kind, ty, local.pat.hir_id);

        Ok(())
    }

    fn local_integrity_expectation(&self, local: &LetStmt) -> Result<Option<Type>> {
        for attr in self.tcx.hir().attrs(local.hir_id) {
            if !self.attr_matches(attr, "integrity") {
                continue;
            }

            match self.parse_integrity(attr)? {
                Integrity::Fn {
                    inputs,
                    output,
                    variables,
                    ..
                } => {
                    let params = inputs
                        .iter()
                        .map(|integrity| {
                            let mut ty = self.top_type();

                            for (i, ss) in integrity.iter().enumerate() {
                                self.ensure_variables_exist(&ss.value, &variables)?;
                                ty.intrinsic[i] = ss.value.clone();
                            }

                            Ok(ProvType::new(ty))
                        })
                        .collect::<Result<Vec<_>>>()?;

                    let mut rty = self.top_type();
                    for (i, ss) in output.iter().enumerate() {
                        self.ensure_variables_exist(&ss.value, &variables)?;
                        rty.intrinsic[i] = ss.value.clone();
                    }

                    let mut ty = self.bottom_type();
                    ty.kind = TypeKind::Fn(params, Box::new(rty));

                    return Ok(Some(ty));
                }

                _ => {}
            }
        }

        Ok(None)
    }

    fn apply_local_integrity_attr(&self, local: &LetStmt, ty: &mut Type, attr: &Attribute) -> Result {
        match self.parse_integrity(attr)? {
            Integrity::Other(Other { integrity, variables }) => {
                for (i, ss) in integrity.iter().enumerate() {
                    self.ensure_variables_exist(&ss.value, &variables)?;

                    if !ty.intrinsic[i].subset(&self.scx, &ss.value) {
                        let msg = format!(
                            "value has incompatible integrity {} since {} ⊈ {}",
                            ty.intrinsic.iter().join(" "),
                            ty.intrinsic[i],
                            ss.value
                        );

                        return Err(self.tcx.dcx().span_err(local.init.unwrap().span, msg));
                    }

                    ty.intrinsic[i] = ss.value.clone();
                }
            }

            Integrity::Struct {
                intrinsic,
                fields,
                variables,
            } => {
                for (i, ss) in intrinsic.iter().enumerate() {
                    self.ensure_variables_exist(&ss.value, &variables)?;

                    if !ty.intrinsic[i].subset(&self.scx, &ss.value) {
                        let msg = format!(
                            "value has incompatible integrity {} since {} ⊈ {}",
                            ty.intrinsic.iter().join(" "),
                            ty.intrinsic[i],
                            ss.value
                        );

                        return Err(self.tcx.dcx().span_err(local.init.unwrap().span, msg));
                    }

                    ty.intrinsic[i] = ss.value.clone();
                }

                match &mut ty.kind {
                    TypeKind::Rec(field_map) => {
                        for (name, integrity) in fields {
                            if let Some(pty) = field_map.get_mut(&name) {
                                for (i, ss) in integrity.iter().enumerate() {
                                    self.ensure_variables_exist(&ss.value, &variables)?;

                                    if !pty.ty.intrinsic[i].subset(&self.scx, &ss.value) {
                                        let msg = format!(
                                            "value has incompatible integrity {} since {} ⊈ {}",
                                            pty.ty.intrinsic.iter().join(" "),
                                            pty.ty.intrinsic[i],
                                            ss.value
                                        );

                                        return Err(self.tcx.dcx().span_err(local.init.unwrap().span, msg));
                                    }

                                    pty.ty.intrinsic[i] = ss.value.clone();
                                }
                            }
                        }
                    }

                    _ => {}
                }
            }

            Integrity::Fn { .. } => {}
        }

        Ok(())
    }

    fn parse<T, F>(&self, attr: &Attribute, p: F) -> Result<T>
    where
        F: for<'a> Fn(&'a mut CoenobitaParser<'a>) -> PResult<'a, T>,
    {
        let AttrKind::Normal(normal) = &attr.kind else {
            unreachable!()
        };

        let psess = create_psess(&self.tcx);

        if let AttrArgs::Delimited(delim_args) = normal.args.clone() {
            let mut parser = create_parser(&psess, delim_args.tokens);

            p(&mut parser).map_err(|err| self.tcx.dcx().span_err(err.span.clone(), "failed to parse"))
        } else {
            panic!()
        }
    }

    fn parse_field(&self, attr: &Attribute) -> Result<Field> {
        self.parse(attr, |parser| parser.parse_field())
    }

    fn parse_input(&self, attr: &Attribute) -> Result<Input> {
        self.parse(attr, |parser| parser.parse_input())
    }

    fn parse_local(&self, attr: &Attribute) -> Result<Other> {
        self.parse(attr, |parser| parser.parse_local())
    }

    fn parse_output(&self, attr: &Attribute) -> Result<Other> {
        self.parse(attr, |parser| parser.parse_output())
    }

    fn parse_param(&self, attr: &Attribute) -> Result<Param> {
        self.parse(attr, |parser| parser.parse_param())
    }

    fn parse_integrity(&self, attr: &Attribute) -> Result<Integrity> {
        self.parse(attr, |parser| parser.parse_integrity())
    }

    fn parse_providers(&self, attr: &Attribute) -> Result<Spanned<Set>> {
        self.parse(attr, |parser| parser.parse_set())
    }

    // ======== PATTERNS ======== //

    fn process_pattern(&mut self, pat_kind: PatKind, ty: Type, hir_id: HirId) {
        self.icx.enter(&ty.intrinsic[2]);

        match pat_kind {
            PatKind::Binding(_, hir_id, ident, _) => {
                // TODO: Figure out the HirId <-> String mapping
                self.str_to_hir.insert(ident.to_string(), hir_id);
                self.hir_to_ty.insert(hir_id, ty.clone());

                self.vctx.set(hir_id, ty.clone());
            }

            PatKind::Guard(pat, _) => self.process_pattern(pat.kind, ty, pat.hir_id),

            PatKind::Struct(qpath, fields, _) => self.process_pattern_struct(hir_id, &qpath, fields, Some(ty)),

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

                self.process_pattern_struct(hir_id, &qpath, &fields, Some(ty));
            }

            PatKind::Tuple(pats, _) => match ty.kind {
                TypeKind::Tuple(elements) => {
                    for (pat, pty) in pats.iter().zip(elements) {
                        self.process_pattern(pat.kind, pty.ty, pat.hir_id);
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
        let mut child = child.clone();

        child.intrinsic[2] = child.intrinsic[2].clone().union(parent.intrinsic[2].clone());

        child
    }

    /// Recursively process a struct pattern, registering all identifiers with the locals map.
    fn process_pattern_struct(&mut self, hir_id: HirId, qpath: &QPath, fields: &[PatField], scrutinee_ty: Option<Type>) {
        let ty = match scrutinee_ty {
            Some(ty @ Type { kind: TypeKind::Rec(_), .. }) => Ok(ty),
            _ => self.check_expr_path(hir_id, qpath, None),
        };

        match ty {
            Ok(ty) => match &ty.kind {
                TypeKind::Rec(map) => {
                    for field in fields {
                        let ty = self.extract(&map[&field.ident.to_string()].ty, &ty);
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
