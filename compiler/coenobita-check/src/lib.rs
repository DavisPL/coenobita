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

use std::collections::HashMap;
use std::fs;
use std::path::Path;

use clippy_utils::ty::implements_trait_with_env_from_iter;
use coenobita_ast::TyAST;
use coenobita_middle::property::Property;
use coenobita_middle::ty::{Ty, TyKind};
use rustc_abi::{VariantIdx, FIRST_VARIANT};

use coenobita_parse::{create_parser, create_psess, parse::Parse};
use expectation::Expectation;

use rustc_hir::{AttrArgs, AttrKind, Attribute};

use rustc_hir::def::{CtorOf, DefKind, Res};
use rustc_hir::def_id::DefId;
use rustc_hir::{
    Arm, Block, BodyId, Closure, Expr, ExprField, ExprKind, FnSig, HirId, Impl, ImplItem, ImplItemKind, Item,
    ItemKind, LangItem, LetExpr, LetStmt, MatchSource, PatField, PatKind, QPath, Stmt, StmtKind,
};

use rustc_middle::ty::{self, FieldDef, GenericArg, TyCtxt, TypeckResults, TypingEnv};
use rustc_span::symbol::Ident;
use rustc_span::{ErrorGuaranteed, Span, Symbol};

use log::{debug, warn};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};

mod expectation;
mod shared;

pub type Result<T = ()> = std::result::Result<T, ErrorGuaranteed>;

#[derive(Serialize, Deserialize)]
pub struct TraitData {
    name: String,
    args: Vec<String>,
    allowed: Vec<String>,
    denied: Vec<String>,
}

pub trait Check<'tcx, P: Property + Parse> {
    // ======== GETTERS ======== //

    /// Return a shared reference to the intrinsic annotations.
    fn get_type_intrinsics(&self) -> &HashMap<String, Ty<P>>;

    fn get_trait_intrinsics(&self) -> &Vec<TraitData>;

    /// Return a shared reference to the attribute we are seeking.
    fn get_attr(&self) -> &[Symbol];

    /// Return an immutable reference to the global typing context.
    fn get_tcx(&self) -> &TyCtxt<'tcx>;

    /// Return a mutable reference to the global typing context.
    fn get_tcx_mut(&mut self) -> &mut TyCtxt<'tcx>;

    /// Return a shared reference to the item map.
    fn get_items(&self) -> &HashMap<DefId, Ty<P>>;

    /// Return a mutable reference to the item map.
    fn get_items_mut(&mut self) -> &mut HashMap<DefId, Ty<P>>;

    /// Return a shared reference to the local map.
    fn get_locals(&self) -> &HashMap<HirId, Ty<P>>;

    /// Return a mutable reference to the local map.
    fn get_locals_mut(&mut self) -> &mut HashMap<HirId, Ty<P>>;

    fn get_crate_name_owned(&self) -> String;

    // ======== CONTEXT ======== //

    /// Enter the scope influenced by type `ty`.
    fn enter_scope(&mut self, ty: &Ty<P>);

    /// Exit the last entered (innermost) scope.
    fn exit_scope(&mut self);

    /// Influence the provided type given the current context.
    fn influence(&self, ty: Ty<P>) -> Ty<P>;

    /// Return the ⊥ type in this context.
    fn ty_bottom(&self) -> Ty<P>;

    /// Return the ⊤ type in this context.
    fn ty_top(&self) -> Ty<P>;

    // ======== TYPES ======== //

    /// Given the `DefId` of a function, return its type.
    fn fn_ty(&mut self, def_id: DefId) -> Ty<P> {
        let canonical_path = self.get_tcx().def_path_str(def_id);
        let intrinsics = self.get_type_intrinsics();

        if intrinsics.contains_key(&canonical_path) {
            return intrinsics[&canonical_path].clone();
        }

        if let Some(ty) = self.get_items().get(&def_id) {
            return ty.clone();
        }

        // We haven't processed the definition of this function yet
        let _ = self.check_item_fn_nonlocal(def_id);
        self.get_items().get(&def_id).unwrap().clone()
    }

    /// Given the parent and variant `DefId`s of an ADT (as well as its index), return its type.
    fn adt_ty(&mut self, def_id_parent: DefId, def_id_variant: DefId, index: VariantIdx) -> Ty<P> {
        if let Some(ty) = self.get_items().get(&def_id_variant) {
            return ty.clone();
        }

        // We haven't processed the definition of this function yet
        let field_defs = &self
            .get_tcx_mut()
            .adt_def(def_id_parent)
            .variants()
            .get(index)
            .unwrap()
            .fields
            .raw;

        let _ = self.check_item_struct(def_id_variant, field_defs);
        self.get_items().get(&def_id_variant).unwrap().clone()
    }

    /// Fetch the type of a local identifier given its `HirId`
    fn local_ty(&mut self, hir_id: HirId) -> Ty<P> {
        match self.get_locals().get(&hir_id) {
            Some(ty) => ty.clone(),
            None => {
                warn!("Silently failed to get type of local variable");
                self.ty_bottom()
            }
        }
    }

    // ======== PATTERNS ======== //

    /// Recursively process a struct pattern, registering all identifiers with the locals map.
    fn process_pattern_struct(&mut self, hir_id: HirId, qpath: &QPath, fields: &[PatField]) {
        match self.check_expr_path(hir_id, qpath, &Expectation::NoExpectation) {
            Ok(ty) => match ty.kind() {
                TyKind::Adt(map) => {
                    for field in fields {
                        let ty = self.influence(map[&field.ident.to_string()].clone());
                        self.process_pattern(field.pat.kind, ty, field.pat.hir_id);
                    }
                }

                TyKind::Opaque | TyKind::Infer => {
                    for field in fields {
                        let ty = self.ty_top();
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

    fn process_pattern(&mut self, pat_kind: PatKind, ty: Ty<P>, hir_id: HirId) {
        self.enter_scope(&ty);

        match pat_kind {
            PatKind::Binding(_, hir_id, _, _) => {
                self.get_locals_mut().insert(hir_id, ty.clone());
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
                TyKind::Tuple(elements) => {
                    for (pat, ty) in pats.iter().zip(elements) {
                        self.process_pattern(pat.kind, ty, pat.hir_id);
                    }
                }

                _ => {
                    // Either the type is opaque or it's simply incorrect
                    let top = self.ty_top();

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

        self.exit_scope();
    }

    // ======== NONLOCAL ITEMS ======== //
    fn check_item_fn_nonlocal(&mut self, def_id: DefId) -> Result {
        let expected = self
            .get_tcx()
            .fn_sig(def_id)
            .skip_binder()
            .inputs()
            .iter()
            .count();

        let mut default = self.influence(Ty::ty_fn(expected));

        match self.get_tcx().get_attrs_by_path(def_id, &self.get_attr()).next() {
            Some(attr) => {
                if let Ok(ty) = self.parse_attr(attr) {
                    default = ty.into();
                };
            }

            None => {}
        };

        self.get_items_mut().insert(def_id, default);

        Ok(())
    }

    // ======== LOCAL ITEMS ======== //
    fn check_item(&mut self, item: &Item) -> Result {
        let def_id = item.owner_id.to_def_id();

        match item.kind {
            ItemKind::Fn { sig, body, .. } => self.check_item_fn(&sig, body),
            ItemKind::Impl(impl_) => self.check_item_impl(impl_),
            ItemKind::Enum(enum_def, _) => {
                for variant in enum_def.variants {
                    let fields: Vec<FieldDef> = variant
                        .data
                        .fields()
                        .iter()
                        .map(|f| FieldDef {
                            did: f.def_id.to_def_id(),
                            name: f.ident.name,
                            vis: self.get_tcx_mut().visibility(f.def_id),
                            safety: f.safety,
                            value: None,
                        })
                        .collect();

                    self.check_item_struct(def_id, &fields)?
                }

                Ok(())
            }

            ItemKind::Struct(var_data, _) => {
                let fields: Vec<FieldDef> = var_data
                    .fields()
                    .iter()
                    .map(|f| FieldDef {
                        did: f.def_id.to_def_id(),
                        name: f.ident.name,
                        vis: self.get_tcx_mut().visibility(f.def_id),
                        safety: f.safety,
                        value: None,
                    })
                    .collect();

                self.check_item_struct(def_id, &fields)
            }

            _ => Ok(()),
        }
    }

    fn check_item_fn(&mut self, signature: &FnSig, body_id: BodyId) -> Result {
        let def_id = body_id.hir_id.owner.to_def_id();
        let expected = signature.decl.inputs.iter().count();

        let mut default = self.influence(Ty::ty_fn(expected));

        match self
            .get_tcx_mut()
            .get_attrs_by_path(def_id, self.get_attr())
            .next()
        {
            Some(attr) => {
                if let Ok(ty) = self.parse_attr(attr) {
                    match ty.inner.kind() {
                        TyKind::Fn(_, ref arg_tys, _) => {
                            // Make sure the number of arguments matches
                            let actual = arg_tys.len();

                            if actual == expected {
                                // Note that we currently don't check whether types have the correct shape
                                default = self.influence(ty.into());
                            } else {
                                let msg = format!(
                                    "Function must have {expected} arguments, but its Coenobita signature has {actual}"
                                );

                                self.get_tcx().dcx().span_err(ty.span, msg);
                            }
                        }

                        _ => {
                            let msg = format!("Expected function type, found {}", ty);
                            self.get_tcx().dcx().span_err(ty.span, msg);
                        }
                    }
                };
            }

            None => {}
        }

        self.get_items_mut().insert(def_id, default);

        let ty_kind = self.fn_ty(def_id).kind;

        let TyKind::Fn(_, args, expected) = ty_kind else {
            panic!()
        };

        let body = self.get_tcx().hir_body(body_id);

        for (param, arg) in body.params.iter().zip(args) {
            self.get_locals_mut().insert(param.pat.hir_id, arg);
        }

        let expr = body.value;
        let actual = self.check_expr(&body.value, &Expectation::ExpectHasType(*expected.clone()), false)?;

        if !actual.satisfies(&expected) {
            let msg = format!("Expected {expected}, found {actual}");
            self.get_tcx_mut().dcx().span_err(expr.span, msg);
        }

        Ok(())
    }

    fn check_impl_item(&mut self, item: &ImplItem) -> Result {
        let def_id = item.owner_id.to_def_id();

        if self
            .get_tcx_mut()
            .get_attr(def_id, Symbol::intern("ignore"))
            .is_some()
        {
            return Ok(());
        }

        match item.kind {
            ImplItemKind::Fn(signature, body_id) => self.check_item_fn(&signature, body_id),
            _ => Ok(()),
        }
    }

    fn check_item_impl(&mut self, impl_: &Impl) -> Result {
        for item_ref in impl_.items {
            // Get the impl item
            let impl_item = self.get_tcx_mut().hir_impl_item(item_ref.id);
            self.check_impl_item(impl_item)?;
        }

        Ok(())
    }

    fn check_item_struct(&mut self, def_id: DefId, field_defs: &[FieldDef]) -> Result {
        let fields = match self.get_tcx().get_attrs_by_path(def_id, &self.get_attr()).next() {
            Some(attr) => {
                let mut map = HashMap::new();

                if let Ok(ty) = self.parse_attr(attr) {
                    match ty.inner.kind() {
                        TyKind::Adt(elements) => {
                            for (name, ty) in elements {
                                map.insert(name, self.influence(ty.clone().into()));
                            }
                        }

                        TyKind::Tuple(ref elements) => {
                            let len = elements.len();
                            for (i, ty) in (0..len).zip(elements) {
                                map.insert(i.to_string(), ty.clone().into());
                            }
                        }

                        _ => {}
                    }
                };

                map
            }

            None => {
                // There is no attribute on the struct as a whole, so we will check for attributes on its fields
                let mut fields = HashMap::new();

                for field in field_defs {
                    match self
                        .get_tcx()
                        .get_attrs_by_path(field.did, &self.get_attr())
                        .next()
                    {
                        Some(attr) => {
                            if let Ok(ty) = self.parse_attr(attr) {
                                fields.insert(field.name.to_string(), ty.into());
                            };
                        }

                        _ => {
                            fields.insert(field.name.to_string(), self.ty_top());
                        }
                    };
                }

                fields
            }
        };

        // Since this type is a struct definition, its flow pair will never be used and is only here for consistency
        let mut ty = self.ty_bottom();
        ty.kind = TyKind::Adt(fields);

        self.get_items_mut().insert(def_id, ty);

        Ok(())
    }

    fn check_let_stmt(&mut self, local: &LetStmt) -> Result {
        let mut processed = false;

        for attr in self.get_tcx_mut().hir().attrs(local.hir_id) {
            if attr.path_matches(&self.get_attr()) {
                if let Ok(ty) = self.parse_attr(attr) {
                    let expected: Ty<P> = ty.into();
                    self.process_pattern(local.pat.kind, expected.clone(), local.pat.hir_id);

                    if let Some(expr) = local.init {
                        self.check_expr(expr, &Expectation::ExpectHasType(expected.clone()), false)?;
                    } else {
                        self.process_pattern(local.pat.kind, self.ty_top(), local.pat.hir_id);
                    }
                };

                processed = true;
            }
        }

        if !processed {
            // There was no tag, so we cannot make any assumptions about the intended type
            if let Some(expr) = local.init {
                self.check_expr(expr, &Expectation::NoExpectation, false)?;
            }

            self.process_pattern(local.pat.kind, self.influence(self.ty_top()), local.pat.hir_id);
        }

        Ok(())
    }

    // ======== EXPRESSIONS ======== //

    /// Serves as the type checking entrypoint for all expressions.
    fn check_expr(&mut self, expr: &Expr, expectation: &Expectation<P>, is_lvalue: bool) -> Result<Ty<P>> {
        let ty = match expr.kind {
            ExprKind::Lit(_) => self.ty_bottom(),
            ExprKind::Break(_, _) => self.ty_bottom(),
            ExprKind::Continue(_) => self.ty_bottom(),

            // TODO: Think about the typing rules for `Unary`, `AddrOf`, and `Index`
            ExprKind::Unary(_, expr) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::DropTemps(expr) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::Cast(expr, _) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::Repeat(expr, _) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::Yield(expr, _) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::AddrOf(_, _, expr) => self.check_expr(expr, &Expectation::NoExpectation, is_lvalue)?,
            ExprKind::Index(expr, _, _) => self.check_expr(expr, &Expectation::NoExpectation, is_lvalue)?,

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

            _ => {
                warn!("Skipping expression of kind {:?}", expr.kind);
                todo!()
            }
        };

        if is_lvalue {
            expectation.check(*self.get_tcx_mut(), ty, expr.span)
        } else {
            expectation.check(*self.get_tcx_mut(), self.influence(ty), expr.span)
        }
    }

    /// Checks the type of a path expression.
    fn check_expr_path(
        &mut self,
        hir_id: HirId,
        qpath: &QPath,
        expectation: &Expectation<P>,
    ) -> Result<Ty<P>> {
        let local_def_id = hir_id.owner.to_def_id().as_local().unwrap();

        let res = self.get_tcx_mut().typeck(local_def_id).qpath_res(qpath, hir_id);
        let ty = match res {
            Res::Local(hir_id) => self.local_ty(hir_id),
            Res::Def(def_kind, def_id) => {
                match def_kind {
                    // TODO: Check that `AssocFn` is handled properly
                    DefKind::Fn | DefKind::AssocFn => self.fn_ty(def_id),

                    DefKind::Struct => self.adt_ty(def_id, def_id, FIRST_VARIANT),

                    DefKind::Variant => {
                        // The variant DefId
                        let id = def_id;

                        // The parent DefId
                        let def_id = self.get_tcx_mut().parent(def_id);

                        // The parent ADT definition
                        let adt = self.get_tcx_mut().adt_def(def_id);

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
                            let def_id = self.get_tcx_mut().parent(def_id);

                            self.adt_ty(def_id, def_id, FIRST_VARIANT)
                        }

                        CtorOf::Variant => {
                            // Get the DefId of the variant
                            let id = self.get_tcx_mut().parent(def_id);

                            // Get the DefId of the enum holding this variant
                            let def_id = self.get_tcx_mut().parent(id);
                            let adt = self.get_tcx_mut().adt_def(def_id);

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
                    DefKind::Static { .. } => self.ty_bottom(),

                    // TODO: Implement actual logic
                    DefKind::AssocConst | DefKind::Const | DefKind::ConstParam => self.ty_bottom(),

                    DefKind::Union => {
                        warn!("Silently skipping union usage");
                        self.ty_bottom()
                    }

                    DefKind::TyAlias => {
                        let ty = self.get_tcx_mut().type_of(def_id).skip_binder();
                        let kind = ty.kind();

                        match kind {
                            ty::TyKind::Adt(adt_def, _) => {
                                self.adt_ty(adt_def.did(), adt_def.did(), FIRST_VARIANT)
                            }
                            _ => todo!(),
                        }
                    }

                    _ => todo!(),
                }
            }

            Res::SelfCtor(alias_to) | Res::SelfTyAlias { alias_to, .. } => {
                match self.get_tcx_mut().type_of(alias_to).skip_binder().kind() {
                    ty::TyKind::Adt(adt_def, _) => {
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

        expectation.check(*self.get_tcx_mut(), self.influence(ty), qpath.span())
    }

    /// Checks the type of a call expression.
    fn check_expr_call(&mut self, fun: &Expr, args: &[Expr], _span: Span) -> Result<Ty<P>> {
        let owner_id = fun.hir_id.owner;
        let owner_def_kind = self.get_tcx().def_kind(owner_id);

        if owner_def_kind.is_fn_like() {
            let local_def_id = owner_id.to_def_id().as_local().unwrap();

            if let ExprKind::Path(qpath) = fun.kind {
                let typeck_results = self.get_tcx().typeck(local_def_id);

                if let Res::Def(def_kind, def_id) = typeck_results.qpath_res(&qpath, fun.hir_id) {
                    self.check_intrinsic_constraints(def_id, def_kind, args, typeck_results)?;
                }
            }
        }

        let fty = self.check_expr(fun, &Expectation::NoExpectation, false)?;

        let ty = match fty.clone().kind() {
            TyKind::Fn(tup_ty, arg_tys, ret_ty) => {
                if let Some(tty) = tup_ty {
                    if !self.ty_bottom().satisfies(&tty) {
                        let msg = format!("cannot call from \"{}\"", self.get_crate_name_owned());
                        self.get_tcx_mut().dcx().span_err(fun.span, msg);
                    }
                }

                if args.len() != arg_tys.len() {
                    warn!("arg ct doesnt match up");
                }

                for (ty, expr) in arg_tys.into_iter().zip(args) {
                    let expectation = Expectation::ExpectHasType(ty);
                    self.check_expr(expr, &expectation, false)?;
                }

                *ret_ty
            }

            TyKind::Adt(elements) => {
                for (i, arg) in args.iter().enumerate() {
                    let ty = elements[&i.to_string()].clone();
                    let expectation = Expectation::ExpectHasType(ty);
                    self.check_expr(arg, &expectation, false)?;
                }

                fty.clone()
            }

            _ => {
                warn!("Cannot tell if expression is a function - {:?}", fun);
                fty.clone()
            }
        };

        Ok(ty.merge(fty))
    }

    /// Checks the type of a method call.
    fn check_method_call(
        &mut self,
        hir_id: HirId,
        receiver: &Expr,
        args: &[Expr],
        _span: Span,
    ) -> Result<Ty<P>> {
        let typeck_results = self.get_tcx_mut().typeck(hir_id.owner.def_id);
        let (def_kind, def_id) = typeck_results.type_dependent_def(hir_id).unwrap();

        match def_kind {
            DefKind::AssocFn => {
                // let args = vec![receiver];
                let mut args_ = vec![receiver.clone()];
                args_.extend_from_slice(args);

                self.check_intrinsic_constraints(def_id, def_kind, args, typeck_results)?;

                let fty = self.fn_ty(def_id);
                // KEEP: I can use this to generate intrinsic annotations
                // let s = serde_json::to_string(&fty).unwrap();
                // debug!("{}", s);

                match fty.kind() {
                    TyKind::Fn(tup_ty, arg_tys, ret_ty) => {
                        if let Some(tty) = tup_ty {
                            if !self.ty_bottom().satisfies(&tty) {
                                // TODO: Use the correct span
                                let msg = format!("cannot call from \"{}\"", self.get_crate_name_owned());
                                self.get_tcx_mut().dcx().span_err(receiver.span, msg);
                            }
                        }

                        // We subtract one to account for the receiver
                        if args.len() != arg_tys.len() - 1 {
                            warn!("arg ct doesnt match up");
                        }

                        let args = std::iter::once(receiver).chain(args.iter());
                        for (ty, expr) in arg_tys.into_iter().zip(args) {
                            let expectation = Expectation::ExpectHasType(ty);
                            self.check_expr(expr, &expectation, false)?;
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
        expectation: &Expectation<P>,
    ) -> Result<Ty<P>> {
        let ity = self.check_expr(guard, &Expectation::NoExpectation, false)?;

        self.enter_scope(&ity);

        let mut rty = self.check_expr(then_expr, expectation, false)?;

        if let Some(else_expr) = else_expr {
            rty = rty.merge(self.check_expr(else_expr, expectation, false)?);
        }

        self.exit_scope();
        Ok(rty)
    }

    /// Checks the type of a `match` expression.
    fn check_expr_match(
        &mut self,
        guard: &Expr,
        arms: &[Arm],
        source: MatchSource,
        expectation: &Expectation<P>,
    ) -> Result<Ty<P>> {
        let ity = match source {
            MatchSource::ForLoopDesugar => {
                // The guard expression is a function call, and we want the argument
                let ExprKind::Call(_, args) = guard.kind else {
                    unreachable!()
                };

                self.check_expr(&args[0], &Expectation::NoExpectation, false)?
            }

            _ => self.check_expr(guard, &Expectation::NoExpectation, false)?,
        };

        self.enter_scope(&ity);

        let mut result = self.ty_bottom();

        for arm in arms {
            self.process_pattern(arm.pat.kind, ity.clone(), arm.pat.hir_id);
            result = result.merge(self.check_expr(arm.body, expectation, false)?)
        }

        self.exit_scope();
        Ok(result)
    }

    /// Checks the type of a `let` expression. These typically appear as the guards of `if` expressions.
    fn check_expr_let(&mut self, let_expr: &LetExpr, expectation: &Expectation<P>) -> Result<Ty<P>> {
        let ty = self.check_expr(let_expr.init, expectation, false)?;

        self.process_pattern(let_expr.pat.kind, ty.clone(), let_expr.pat.hir_id);

        Ok(ty)
    }

    /// Checks the type of a binary expression.
    fn check_expr_binary(&mut self, lhs: &Expr, rhs: &Expr, expectation: &Expectation<P>) -> Result<Ty<P>> {
        let lty = self.check_expr(lhs, expectation, false)?;
        let rty = self.check_expr(rhs, expectation, false)?;
        let ty = lty.merge(rty);

        Ok(ty)
    }

    /// Checks the type of a block.
    fn check_expr_block(&mut self, block: &Block, expectation: &Expectation<P>) -> Result<Ty<P>> {
        // The number of statements in this block;
        let len = block.stmts.len();

        match block.expr {
            Some(expr) => {
                for stmt in block.stmts {
                    self.check_stmt(stmt, &Expectation::NoExpectation)?;
                }

                // This expression occurs at the very end without a semicolon
                self.check_expr(expr, expectation, false)
            }

            None if !block.stmts.is_empty() => {
                // Check all statements except the last one without expectation
                for stmt in block.stmts[..len - 1].iter() {
                    self.check_stmt(stmt, &Expectation::NoExpectation)?;
                }

                self.check_stmt(&block.stmts.last().unwrap(), expectation)
            }

            _ => Ok(self.ty_bottom()),
        }
    }

    /// Checks the type of an assignment expression.
    fn check_expr_assign(&mut self, dest: &Expr, expr: &Expr) -> Result<Ty<P>> {
        let expected = self.check_expr(dest, &Expectation::NoExpectation, true)?;

        let expectation = &Expectation::ExpectHasType(expected.clone());
        self.check_expr(expr, expectation, false)
    }

    /// Checks the type of a struct expression.
    fn check_expr_struct(&mut self, hir_id: HirId, qpath: &QPath, fields: &[ExprField]) -> Result<Ty<P>> {
        let ty = match qpath {
            QPath::LangItem(item, _) => match item {
                LangItem::Range => {
                    // NOTE: We handle ranges in a special manner for the time being
                    let mut result = self.ty_bottom();

                    for field in fields {
                        result =
                            result.merge(self.check_expr(field.expr, &Expectation::NoExpectation, false)?);
                    }

                    result
                }

                _ => {
                    warn!("Language item silently ignored");
                    self.ty_bottom()
                }
            },

            _ => self.check_expr_path(hir_id, qpath, &Expectation::NoExpectation)?,
        };

        match ty.kind() {
            TyKind::Adt(tys) => {
                for field in fields {
                    let ty = tys[&field.ident.to_string()].clone();
                    let expectation = Expectation::ExpectHasType(ty);
                    self.check_expr(field.expr, &expectation, false)?;
                }
            }

            TyKind::Opaque | TyKind::Infer => {
                for field in fields {
                    self.check_expr(field.expr, &Expectation::NoExpectation, false)?;
                }
            }

            _ => todo!(),
        }

        Ok(ty)
    }

    /// Checks the type of an array expression.
    fn check_expr_array(&mut self, exprs: &[Expr]) -> Result<Ty<P>> {
        let mut result = self.ty_bottom();
        let mut item = self.ty_bottom();

        for expr in exprs {
            item = item.merge(self.check_expr(expr, &Expectation::NoExpectation, false)?);
        }

        result.kind = TyKind::Array(Box::new(item));

        Ok(result)
    }

    /// Checks the type of a tuple expression.
    fn check_expr_tup(&mut self, exprs: &[Expr]) -> Result<Ty<P>> {
        let mut result = self.ty_bottom();

        let mut items = vec![];

        for expr in exprs {
            items.push(self.check_expr(expr, &Expectation::NoExpectation, false)?);
        }

        result.kind = TyKind::Tuple(items);
        Ok(result)
    }

    /// Checks the type of a return expression.
    fn check_expr_ret(&mut self, expr: Option<&Expr>, expectation: &Expectation<P>) -> Result<Ty<P>> {
        match expr {
            Some(expr) => Ok(self.check_expr(expr, expectation, false)?),
            None => Ok(self.ty_bottom()),
        }
    }

    /// Checks the type of a closure expression.
    fn check_expr_closure(&mut self, closure: &Closure, expectation: &Expectation<P>) -> Result<Ty<P>> {
        match expectation {
            // TODO: Account for the argument tuple type
            Expectation::ExpectHasType(Ty {
                property,
                kind: TyKind::Fn(_, args, ret),
            }) => {
                // First, make sure the number of parameters is correct
                let count = closure.fn_decl.inputs.iter().count();

                if count != args.len() {
                    let msg = format!("expected {} parameter(s), found {count}", args.len());
                    self.get_tcx_mut()
                        .dcx()
                        .span_err(closure.fn_arg_span.unwrap(), msg);
                }

                // Then, type check the body
                let body = self.get_tcx_mut().hir_body(closure.body);

                for (param, arg) in body.params.iter().zip(args.clone()) {
                    // TODO: Use `process_pattern` instead
                    self.get_locals_mut().insert(param.pat.hir_id, arg);
                }

                let expectation = Expectation::ExpectHasType(*ret.clone());
                let ret = self.check_expr(&body.value, &expectation, false)?;

                // Finally, return the expected type
                Ok(Ty {
                    property: property.clone(),
                    kind: TyKind::Fn(None, args.to_vec(), Box::new(ret)),
                })
            }

            _ => {
                // Either there is no expectation or the expectation isn't a function
                let count = closure.fn_decl.inputs.iter().count();

                let args: Vec<Ty<P>> = (0..count).map(|_| self.ty_bottom()).collect();

                // Then, type check the body
                let body = self.get_tcx_mut().hir_body(closure.body);

                for (param, arg) in body.params.iter().zip(args.clone()) {
                    // TODO: Use `process_pattern` instead
                    self.get_locals_mut().insert(param.pat.hir_id, arg);
                }

                let ret = self.check_expr(&body.value, &Expectation::NoExpectation, false)?;

                let mut ty = self.ty_bottom();
                ty.kind = TyKind::Fn(None, args, Box::new(ret));

                Ok(ty)
            }
        }
    }

    /// Checks the type of a projection.
    fn check_expr_field(&mut self, object: &Expr, field: Ident) -> Result<Ty<P>> {
        match self.check_expr(object, &Expectation::NoExpectation, false)?.kind {
            TyKind::Adt(field_map) => {
                if !field_map.contains_key(&field.to_string()) {
                    let msg = format!("unknown field '{}'", field.name);
                    self.get_tcx_mut().dcx().span_err(field.span, msg);
                    Ok(self.ty_top())
                } else {
                    Ok(field_map[&field.to_string()].clone())
                }
            }

            TyKind::Tuple(fields) => {
                let index = field.name.as_str().parse::<usize>().unwrap();

                if index >= fields.len() {
                    let msg = format!("unknown field '{}'", field.name);
                    self.get_tcx_mut().dcx().span_err(field.span, msg);
                    Ok(self.ty_top())
                } else {
                    Ok(fields[index].clone())
                }
            }

            TyKind::Opaque | TyKind::Infer => {
                // For now, any field access with an unknown receiver will be universal
                // However, in the future, I should refine my type checking to avoid this
                Ok(self.ty_top())
            }

            _ => todo!(),
        }
    }

    /// Checks the type of a statement.
    fn check_stmt(&mut self, stmt: &Stmt, expectation: &Expectation<P>) -> Result<Ty<P>> {
        // TODO: Think more about what type should be returned by a statement
        match stmt.kind {
            StmtKind::Let(local) => {
                self.check_let_stmt(local)?;
                Ok(self.ty_bottom())
            }
            StmtKind::Expr(expr) => self.check_expr(expr, expectation, false),
            StmtKind::Semi(expr) => self.check_expr(expr, expectation, false),
            StmtKind::Item(item_id) => {
                let item = self.get_tcx_mut().hir_item(item_id);
                self.check_item(item)?;
                Ok(self.ty_bottom())
            }
        }
    }

    // ======== ATTRIBUTE PARSING ======== //

    fn parse_attr(&self, attr: &Attribute) -> Result<TyAST<P>> {
        let AttrKind::Normal(normal) = &attr.kind else {
            unreachable!()
        };

        let psess = create_psess(self.get_tcx());

        if let AttrArgs::Delimited(delim_args) = normal.args.clone() {
            let mut parser = create_parser(&psess, delim_args.tokens);

            parser
                .parse_ty()
                .map_err(|err| self.get_tcx().dcx().span_err(err.span.clone(), "failed to parse"))
        } else {
            panic!()
        }
    }

    // OTHER //
    fn check_intrinsic_constraints(
        &mut self,
        def_id: DefId,
        def_kind: DefKind,
        args: &[Expr],
        typeck_results: &TypeckResults<'tcx>,
    ) -> Result {
        match def_kind {
            DefKind::Fn | DefKind::AssocFn => {
                let fn_sig = self.get_tcx_mut().fn_sig(def_id).skip_binder();

                for (expected_ty, actual_expr) in fn_sig.inputs().iter().zip(args) {
                    let expected_ty = expected_ty.skip_binder();
                    let actual_ty = typeck_results.expr_ty(actual_expr);

                    if let ty::Param(_) = expected_ty.kind() {
                        for trait_data in self.get_trait_intrinsics() {
                            let implements = impls_trait(
                                expected_ty,
                                &trait_data.name,
                                &trait_data.args,
                                self.get_tcx(),
                                def_id,
                            );

                            let allowed_tys: Vec<ty::Ty<'_>> = trait_data
                                .allowed
                                .iter()
                                .map(|name| {
                                    let ty_symbol = Symbol::intern(name);
                                    match self.get_tcx().get_diagnostic_item(ty_symbol) {
                                        Some(def_id) => self.get_tcx().type_of(def_id).skip_binder(),
                                        None => panic!("Type {name} is not a diagnostic item"),
                                    }
                                })
                                .collect();

                            if implements && !allowed_tys.contains(&actual_ty.peel_refs()) {
                                let msg = format!("must be one of: {}", trait_data.allowed.join(", "));
                                return Err(self.get_tcx_mut().dcx().span_err(actual_expr.span, msg));
                            }
                        }
                    }
                }
            }

            DefKind::Ctor(ctor_of, ctor_kind) => {
                let name = self.get_tcx().def_path_str(def_id);
            }

            _ => {}
        }

        Ok(())
    }
}

pub struct Checker<'tcx, P: Property> {
    /// Contains the name of the crate we're checking.
    crate_name: String,

    /// Contains the attribute we are targeting as a sequence of symbols.
    attr: Vec<Symbol>,

    /// Contains type checking results for the entire crate.
    tcx: TyCtxt<'tcx>,

    /// Maps item `DefId`s to their properties.
    items: HashMap<DefId, Ty<P>>,

    /// Maps local `HirId`s to their properties.
    locals: HashMap<HirId, Ty<P>>,

    context: Vec<Ty<P>>,

    pub type_intrinsics: HashMap<String, Ty<P>>,

    trait_intrinsics: Vec<TraitData>,
}

impl<'tcx, P: Property + DeserializeOwned> Checker<'tcx, P> {
    pub fn new(crate_name: String, tcx: TyCtxt<'tcx>) -> Self {
        // Collect intrinsic annotations for this property
        let path = P::intrinsics_path();
        let map = fs::read_to_string(&path).unwrap();

        let type_intrinsics: HashMap<String, Ty<P>> = serde_json::from_str(&map).unwrap();

        // Collect all trait intrinsics
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .join("intrinsics")
            .join("ad-hoc")
            .join("traits")
            .with_extension("json");

        let map = fs::read_to_string(&path).unwrap();

        let trait_intrinsics: Vec<TraitData> = serde_json::from_str(&map).unwrap();

        Self {
            crate_name: crate_name.clone(),
            attr: P::attr(),
            tcx,
            items: HashMap::new(),
            locals: HashMap::new(),
            context: vec![Ty::new(P::bottom(crate_name), TyKind::Infer)],
            type_intrinsics,
            trait_intrinsics,
        }
    }
}

impl<'tcx, P: Property + Parse + DeserializeOwned> Check<'tcx, P> for Checker<'tcx, P> {
    fn get_crate_name_owned(&self) -> String {
        self.crate_name.clone()
    }

    fn get_type_intrinsics(&self) -> &HashMap<String, Ty<P>> {
        &self.type_intrinsics
    }

    fn get_trait_intrinsics(&self) -> &Vec<TraitData> {
        &self.trait_intrinsics
    }

    fn get_attr(&self) -> &[Symbol] {
        &self.attr
    }

    fn get_tcx(&self) -> &TyCtxt<'tcx> {
        &self.tcx
    }

    fn get_tcx_mut(&mut self) -> &mut TyCtxt<'tcx> {
        &mut self.tcx
    }

    fn get_items(&self) -> &HashMap<DefId, Ty<P>> {
        &self.items
    }

    fn get_items_mut(&mut self) -> &mut HashMap<DefId, Ty<P>> {
        &mut self.items
    }

    fn get_locals(&self) -> &HashMap<HirId, Ty<P>> {
        &self.locals
    }

    fn get_locals_mut(&mut self) -> &mut HashMap<HirId, Ty<P>> {
        &mut self.locals
    }

    fn enter_scope(&mut self, ty: &Ty<P>) {
        self.context.push(ty.clone())
    }

    fn exit_scope(&mut self) {
        self.context.pop();
    }

    fn influence(&self, ty: Ty<P>) -> Ty<P> {
        let mut result = ty;

        for infl in self.context.iter().rev() {
            result = infl.clone().influence(result);
        }

        result
    }

    fn ty_bottom(&self) -> Ty<P> {
        self.influence(Ty::new(P::bottom(self.crate_name.clone()), TyKind::Infer))
    }

    fn ty_top(&self) -> Ty<P> {
        Ty::new(P::top(), TyKind::Infer)
    }
}

fn impls_trait<'a, 'tcx, T: IntoIterator<Item = &'a String>>(
    ty: &ty::Ty<'tcx>,
    trait_name: &str,
    arg_ty_names: T,
    tcx: &TyCtxt<'tcx>,
    parent_did: DefId,
) -> bool {
    let trait_symbol = Symbol::intern(trait_name);
    let trait_did = tcx
        .get_diagnostic_item(trait_symbol)
        .expect(&format!("Trait `{trait_name}` is not a diagnostic item"));

    let arg_tys = arg_ty_names.into_iter().map(|name| {
        let ty_symbol = Symbol::intern(&name);
        let ty = match tcx.get_diagnostic_item(ty_symbol) {
            Some(did) => tcx.type_of(did).skip_binder(),
            None => panic!("Type `{trait_name}` is not a diagnostic item"),
        };
        GenericArg::from(ty)
    });

    let typing_env = TypingEnv::post_analysis(*tcx, parent_did);

    implements_trait_with_env_from_iter(*tcx, typing_env, *ty, trait_did, None, arg_tys)
}
