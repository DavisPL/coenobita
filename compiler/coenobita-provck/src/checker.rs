use std::collections::HashMap;

use coenobita_ast::ast::TyKind as ATyKind;
use coenobita_log::debug;
use coenobita_middle::map::Map;
use coenobita_middle::ty::TyKind;

use coenobita_parse::{create_parser, create_psess};
use rustc_ast::AttrKind;
use rustc_hir::def::{CtorOf, DefKind, Res};
use rustc_hir::def_id::DefId;
use rustc_hir::{
    Arm, Block, BodyId, Closure, Expr, ExprField, ExprKind, FnSig, HirId, Impl, ImplItem, ImplItemKind, Item,
    ItemKind, LangItem, LetExpr, LetStmt, MatchSource, PatField, PatKind, QPath, Stmt, StmtKind,
};
use rustc_middle::ty::{self, FieldDef, TyCtxt};
use rustc_span::symbol::Ident;
use rustc_span::{Span, Symbol};
use rustc_target::abi::{VariantIdx, FIRST_VARIANT};

use crate::context::Context;
use crate::expectation::Expectation;
use crate::shared::{Result, Ty};

pub struct Checker<'cnbt, 'tcx> {
    attr: Vec<Symbol>,
    tcx: TyCtxt<'tcx>,
    def_map: Map<DefId, Ty>,
    hir_map: Map<HirId, Ty>,
    context: Context<'cnbt>,
}

impl<'cnbt, 'tcx> Checker<'cnbt, 'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, context: Context<'cnbt>) -> Self {
        Checker {
            context,
            attr: vec![Symbol::intern("cnbt"), Symbol::intern("provenance")],
            tcx,
            def_map: Map::new(),
            hir_map: Map::new(),
        }
    }

    pub fn fn_ty(&mut self, def_id: DefId) -> Ty {
        let ty = match self.def_map.get(&def_id) {
            Some(ty) => ty.clone(),
            None => {
                // We haven't processed the definition of this function yet
                let _ = self.check_external_item_fn(def_id);
                self.def_map.get(&def_id).unwrap().clone()
            }
        };

        ty
    }

    pub fn adt_ty(&mut self, def_id_parent: DefId, def_id_variant: DefId, index: VariantIdx) -> Ty {
        match self.def_map.get(&def_id_variant) {
            Some(ty) => ty.clone(),
            None => {
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
                self.def_map.get(&def_id_variant).unwrap().clone()
            }
        }
    }

    /// Fetches the type of a local variable. Panics if it hasn't been processed, which should be impossible.
    pub fn local(&mut self, hir_id: HirId) -> Ty {
        match self.hir_map.get(&hir_id) {
            Some(ty) => ty.clone(),
            None => {
                debug("[WARN] Silently failed to get type of local var");
                self.context.introduce()
            }
        }
    }

    pub fn process_pattern_struct(&mut self, hir_id: HirId, qpath: &QPath, fields: &[PatField]) {
        match self.check_expr_path(hir_id, qpath, &Expectation::NoExpectation) {
            Ok(ty) => match ty.kind() {
                TyKind::Adt(map) => {
                    for field in fields {
                        let ty = self.context.influence(map[&field.ident.name].clone());
                        self.process_pattern(field.pat.kind, ty, field.pat.hir_id);
                    }
                }

                TyKind::Opaque | TyKind::Infer => {
                    for field in fields {
                        let ty = self.context.universal();
                        self.process_pattern(field.pat.kind, ty, field.pat.hir_id);
                    }
                }

                _ => todo!(),
            },

            Err(_) => {
                debug("[WARN] Pattern struct processing failing silently");
            }
        }
    }

    pub fn process_pattern(&mut self, pat_kind: PatKind, ty: Ty, hir_id: HirId) {
        match pat_kind {
            PatKind::Binding(_, hir_id, _, _) => {
                self.hir_map.insert(hir_id, ty.clone());
            }

            PatKind::Struct(qpath, fields, _) => self.process_pattern_struct(hir_id, &qpath, fields),

            PatKind::Box(pat) => self.process_pattern(pat.kind, ty, pat.hir_id),
            PatKind::Deref(pat) => self.process_pattern(pat.kind, ty, pat.hir_id),
            PatKind::Err(_) | PatKind::Lit(_) => {}

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
                    for pat in pats.iter() {
                        self.process_pattern(pat.kind, self.context.universal(), pat.hir_id);
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

            // Nothing to process
            PatKind::Range(_, _, _) => {}
            PatKind::Path(_) => {}
            PatKind::Wild => {}
            PatKind::Never => {}
        };
    }

    pub fn check_item(&mut self, item: &Item) -> Result {
        let did = item.owner_id.to_def_id();

        match item.kind {
            ItemKind::Fn(signature, _, body_id) => self.check_item_fn(&signature, body_id),
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
                            vis: self.tcx.visibility(f.def_id),
                        })
                        .collect();

                    self.check_item_struct(did, &fields)?
                }

                Ok(())
            }

            ItemKind::Struct(var_data, _) => {
                let fields: Vec<FieldDef> = var_data
                    .fields()
                    .iter()
                    .map(|f| ty::FieldDef {
                        did: f.def_id.to_def_id(),
                        name: f.ident.name,
                        vis: self.tcx.visibility(f.def_id),
                    })
                    .collect();

                self.check_item_struct(did, &fields)
            }
            _ => Ok(()),
        }
    }

    pub fn check_impl_item(&mut self, item: &ImplItem) -> Result {
        let did = item.owner_id.to_def_id();

        if self.tcx.get_attr(did, Symbol::intern("ignore")).is_some() {
            return Ok(());
        }

        match item.kind {
            ImplItemKind::Fn(signature, body_id) => self.check_item_fn(&signature, body_id),
            _ => Ok(()),
        }
    }

    pub fn check_item_fn(&mut self, signature: &FnSig, body_id: BodyId) -> Result {
        let def_id = body_id.hir_id.owner.to_def_id();
        let expected = signature.decl.inputs.iter().count();

        let default = self.context.influence(Ty::ty_fn(expected));

        match self.tcx.get_attrs_by_path(def_id, &self.attr).next() {
            Some(attr) => {
                // The `coenobita::tag` is guaranteed to be a normal attribute
                let AttrKind::Normal(normal) = &attr.kind else {
                    unreachable!()
                };

                let psess = create_psess(&self.tcx);
                let mut parser = create_parser(&psess, normal.item.args.inner_tokens());

                if let Ok(ty) = parser.parse_ty() {
                    match ty.kind {
                        ATyKind::Fn(ref arg_tys, _) => {
                            // Make sure the number of arguments matches
                            let actual = arg_tys.len();

                            if actual == expected {
                                // Note that we currently don't check whether types have the correct shape
                                self.def_map.insert(def_id, self.context.influence(ty.into()));
                            } else {
                                self.def_map.insert(def_id, default);

                                let msg = format!(
                                    "Function must have {expected} arguments, but its Coenobita signature has {actual}"
                                );
                                self.tcx.dcx().span_err(ty.span, msg);
                            }
                        }

                        _ => {
                            self.def_map.insert(def_id, default);

                            let msg = format!("Expected function type, found {}", ty);
                            self.tcx.dcx().span_err(ty.span, msg);
                        }
                    }
                };
            }

            None => {
                self.def_map.insert(def_id, default);
            }
        }

        let TyKind::Fn(args, expected) = self.fn_ty(def_id).kind else {
            panic!()
        };

        let body = self.tcx.hir().body(body_id);

        for (param, arg) in body.params.iter().zip(args) {
            self.hir_map.insert(param.pat.hir_id, arg);
        }

        let expr = body.value;
        let actual = self.check_expr(&body.value, &Expectation::ExpectHasType(*expected.clone()), false)?;

        if !actual.satisfies(&expected) {
            let msg = format!("Expected {expected}, found {actual}");
            self.tcx.dcx().span_err(expr.span, msg);
        }

        Ok(())
    }

    pub fn check_item_impl(&mut self, impl_: &Impl) -> Result {
        for item_ref in impl_.items {
            // Get the impl item
            let impl_item = self.tcx.hir().impl_item(item_ref.id);
            self.check_impl_item(impl_item)?;
        }

        Ok(())
    }

    pub fn check_item_struct(&mut self, def_id: DefId, field_defs: &[FieldDef]) -> Result {
        let fields = match self.tcx.get_attrs_by_path(def_id, &self.attr).next() {
            Some(attr) => {
                // The `coenobita::tag` is guaranteed to be a normal attribute
                let AttrKind::Normal(normal) = &attr.kind else {
                    unreachable!()
                };

                let psess = create_psess(&self.tcx);
                let mut parser = create_parser(&psess, normal.item.args.inner_tokens());

                let mut map = HashMap::new();

                if let Ok(ty) = parser.parse_ty() {
                    match ty.kind {
                        ATyKind::Struct(ref elements) => {
                            for (name, ty) in elements {
                                map.insert(name.name, self.context.influence(ty.clone().into()));
                            }
                        }

                        ATyKind::StructTuple(ref elements) => {
                            let len = elements.len();
                            for (i, ty) in (0..len).zip(elements) {
                                map.insert(Symbol::intern(&i.to_string()), ty.clone().into());
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
                    match self.tcx.get_attrs_by_path(field.did, &self.attr).next() {
                        Some(attr) => {
                            // The `coenobita::tag` is guaranteed to be a normal attribute
                            let AttrKind::Normal(normal) = &attr.kind else {
                                unreachable!()
                            };

                            let psess = create_psess(&self.tcx);
                            let mut parser = create_parser(&psess, normal.item.args.inner_tokens());

                            if let Ok(ty) = parser.parse_ty() {
                                fields.insert(field.name, ty.into());
                            };
                        }

                        _ => {
                            fields.insert(field.name, self.context.universal());
                        }
                    };
                }

                fields
            }
        };

        // Since this type is a struct definition, its flow pair will never be used and is only here for consistency
        let mut ty = self.context.introduce();
        ty.kind = TyKind::Adt(fields);

        self.def_map.insert(def_id, ty);

        Ok(())
    }

    pub fn check_external_item_fn(&mut self, def_id: DefId) -> Result {
        let expected = self.tcx.fn_sig(def_id).skip_binder().inputs().iter().count();

        let default = self.context.influence(Ty::ty_fn(expected));

        match self.tcx.get_attrs_by_path(def_id, &self.attr).next() {
            Some(attr) => {
                // The `coenobita::tag` is guaranteed to be a normal attribute
                let AttrKind::Normal(normal) = &attr.kind else {
                    unreachable!()
                };

                let psess = create_psess(&self.tcx);
                let mut parser = create_parser(&psess, normal.item.args.inner_tokens());

                if let Ok(ty) = parser.parse_ty() {
                    self.def_map.insert(def_id, ty.into());
                };
            }

            None => {
                self.def_map.insert(def_id, default);
            }
        };

        Ok(())
    }

    /// Checks the type of an expression in a `let` statement and maps the `HirId` of the bound
    /// pattern to its type. If an provenance annotation is provided, it's used as an expectation.
    pub fn check_let_stmt(&mut self, local: &LetStmt) -> Result {
        let mut processed = false;

        for attr in self.tcx.hir().attrs(local.hir_id) {
            if attr.path_matches(&self.attr) {
                let AttrKind::Normal(normal) = &attr.kind else {
                    unreachable!()
                };

                let psess = create_psess(&self.tcx);
                let mut parser = create_parser(&psess, normal.item.args.inner_tokens());

                if let Ok(ty) = parser.parse_ty() {
                    let expected: Ty = ty.into();
                    self.process_pattern(local.pat.kind, expected.clone(), local.pat.hir_id);

                    if let Some(expr) = local.init {
                        self.check_expr(expr, &Expectation::ExpectHasType(expected.clone()), false)?;
                    }
                };

                processed = true;
            }
        }

        if !processed {
            // There was no tag, so we cannot make any assumptions about the intended type
            if let Some(expr) = local.init {
                self.check_expr(expr, &Expectation::NoExpectation, false)?;
                self.process_pattern(local.pat.kind, self.context.universal(), local.pat.hir_id);
            } else {
                // TODO: Think about what should happen here
                self.hir_map.insert(local.pat.hir_id, self.context.universal());
            }
        }

        Ok(())
    }

    /// Serves as the type checking entrypoint for all expressions.
    pub fn check_expr(&mut self, expr: &Expr, expectation: &Expectation, is_lvalue: bool) -> Result<Ty> {
        let ty = match expr.kind {
            ExprKind::Lit(_) => self.context.introduce(),
            ExprKind::Break(_, _) => self.context.introduce(),
            ExprKind::Continue(_) => self.context.introduce(),

            // TODO: Think about the typing rules for `Unary`, `AddrOf`, and `Index`
            ExprKind::Unary(_, expr) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::DropTemps(expr) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::Cast(expr, _) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::Repeat(expr, _) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::Yield(expr, _) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::AddrOf(_, _, expr) => self.check_expr(expr, expectation, is_lvalue)?,
            ExprKind::Index(expr, _, _) => self.check_expr(expr, expectation, is_lvalue)?,

            ExprKind::Assign(dest, expr, _) => self.check_expr_assign(dest, expr)?,
            ExprKind::AssignOp(_, dest, expr) => self.check_expr_assign(dest, expr)?,

            ExprKind::Block(block, _) => self.check_expr_block(block, expectation)?,
            ExprKind::Loop(block, _, _, _) => self.check_expr_block(block, expectation)?,

            ExprKind::Path(qpath) => self.check_expr_path(expr.hir_id, &qpath, expectation)?,
            ExprKind::Call(func, args) => self.check_expr_call(func, args, expr.span)?,
            ExprKind::Let(let_expr) => self.check_expr_let(let_expr, expectation)?,
            ExprKind::Binary(_, lhs, rhs) => self.check_expr_binary(lhs, rhs)?,
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
                debug(format!("Skipping expression of kind {:#?}", expr.kind));
                todo!()
            }
        };

        if is_lvalue {
            expectation.check(self.tcx, ty, expr.span)
        } else {
            expectation.check(self.tcx, self.context.influence(ty), expr.span)
        }
    }

    /// Checks the type of a path expression.
    pub fn check_expr_path(&mut self, hir_id: HirId, qpath: &QPath, expectation: &Expectation) -> Result<Ty> {
        let local_def_id = hir_id.owner.to_def_id().as_local().unwrap();

        let res = self.tcx.typeck(local_def_id).qpath_res(qpath, hir_id);
        let ty = match res {
            Res::Local(hir_id) => self.local(hir_id),
            Res::Def(def_kind, def_id) => match def_kind {
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
                DefKind::Static { .. } => self.context.introduce(),

                // TODO: Implement actual logic
                DefKind::AssocConst | DefKind::Const | DefKind::ConstParam => self.context.introduce(),

                DefKind::Union => {
                    debug("[WARN] Silently skipping union usage");
                    self.context.introduce()
                }

                _ => {
                    debug(format!("unsupported def kind {:#?}", res));
                    todo!()
                }
            },

            Res::SelfCtor(alias_to) | Res::SelfTyAlias { alias_to, .. } => {
                match self.tcx.type_of(alias_to).skip_binder().kind() {
                    ty::TyKind::Adt(adt_def, _) => {
                        let did = adt_def.did();
                        self.adt_ty(did, did, FIRST_VARIANT)
                    }

                    _ => todo!(),
                }
            }

            _ => {
                debug(format!("unsupported res {:#?}", res));
                todo!()
            }
        };

        Ok(ty)
    }

    /// Checks the type of a call expression.
    pub fn check_expr_call(&mut self, func: &Expr, args: &[Expr], span: Span) -> Result<Ty> {
        let fty = self.check_expr(func, &Expectation::NoExpectation, true)?;

        match fty.kind() {
            TyKind::Fn(arg_tys, ret_ty) => {
                if args.len() != arg_tys.len() {
                    // let msg = format!(
                    //     "expected {} argument(s), found {}",
                    //     arg_tys.len(),
                    //     args.len()
                    // );

                    // return Err(self.tcx.dcx().span_err(span, msg));
                    debug("[WARN] arg ct doesnt match up");
                }

                for (ty, expr) in arg_tys.into_iter().zip(args) {
                    let expectation = Expectation::ExpectHasType(ty);
                    self.check_expr(expr, &expectation, false)?;
                }

                Ok(*ret_ty)
            }

            TyKind::Adt(elements) => {
                for (i, arg) in args.iter().enumerate() {
                    let ty = elements[&Symbol::intern(&i.to_string())].clone();
                    let expectation = Expectation::ExpectHasType(ty);
                    self.check_expr(arg, &expectation, false)?;
                }

                Ok(fty)
            }

            _ => {
                let msg = "Coenobita cannot tell if this is a function";
                self.tcx.dcx().span_warn(func.span, msg);

                Ok(fty)
            }
        }
    }

    /// Checks the type of a method call.
    pub fn check_method_call(
        &mut self,
        hir_id: HirId,
        receiver: &Expr,
        args: &[Expr],
        span: Span,
    ) -> Result<Ty> {
        let (def_kind, def_id) = self
            .tcx
            .typeck(hir_id.owner.def_id)
            .type_dependent_def(hir_id)
            .unwrap();

        match def_kind {
            DefKind::AssocFn => {
                let fty = self.fn_ty(def_id);

                match fty.kind() {
                    TyKind::Fn(arg_tys, ret_ty) => {
                        // We subtract one to account for the receiver
                        if args.len() != arg_tys.len() - 1 {
                            // let msg = format!(
                            //     "expected {} argument(s), found {}",
                            //     arg_tys.len(),
                            //     args.len()
                            // );

                            // return Err(self.tcx.dcx().span_err(span, msg));
                            debug("[WARN] arg ct doesnt match up");
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
    pub fn check_expr_if(
        &mut self,
        guard: &Expr,
        then_expr: &Expr,
        else_expr: Option<&Expr>,
        expectation: &Expectation,
    ) -> Result<Ty> {
        self.check_expr(guard, &Expectation::NoExpectation, false)?;
        self.check_expr(then_expr, expectation, false)?;

        if let Some(else_expr) = else_expr {
            self.check_expr(else_expr, expectation, false)?;
        }

        Ok(self.context.introduce())
    }

    /// Checks the type of a `match` expression.
    pub fn check_expr_match(
        &mut self,
        guard: &Expr,
        arms: &[Arm],
        source: MatchSource,
        expectation: &Expectation,
    ) -> Result<Ty> {
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

        for arm in arms {
            self.process_pattern(arm.pat.kind, ity.clone(), arm.pat.hir_id);
            self.check_expr(arm.body, expectation, false)?;
        }

        Ok(self.context.introduce())
    }

    /// Checks the type of a `let` expression. These typically appear as the guards of `if` expressions.
    pub fn check_expr_let(&mut self, let_expr: &LetExpr, expectation: &Expectation) -> Result<Ty> {
        let ty = self.check_expr(let_expr.init, expectation, false)?;

        self.process_pattern(let_expr.pat.kind, ty.clone(), let_expr.pat.hir_id);

        Ok(ty)
    }

    /// Checks the type of a binary expression.
    pub fn check_expr_binary(&mut self, lhs: &Expr, rhs: &Expr) -> Result<Ty> {
        self.check_expr(lhs, &Expectation::NoExpectation, false)?;
        self.check_expr(rhs, &Expectation::NoExpectation, false)?;

        Ok(self.context.introduce())
    }

    /// Checks the type of a block.
    pub fn check_expr_block(&mut self, block: &Block, expectation: &Expectation) -> Result<Ty> {
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

            _ => Ok(self.context.introduce()),
        }
    }

    /// Checks the type of an assignment expression.
    pub fn check_expr_assign(&mut self, dest: &Expr, expr: &Expr) -> Result<Ty> {
        let expected = self.check_expr(dest, &Expectation::NoExpectation, true)?;

        //let expectation = &Expectation::ExpectHasType(expected.clone());
        self.check_expr(expr, &Expectation::NoExpectation, false)
    }

    /// Checks the type of a struct expression.
    pub fn check_expr_struct(&mut self, hir_id: HirId, qpath: &QPath, fields: &[ExprField]) -> Result<Ty> {
        let ty = match qpath {
            QPath::LangItem(item, _) => match item {
                LangItem::Range => {
                    // NOTE: We handle ranges in a special manner for the time being
                    for field in fields {
                        self.check_expr(field.expr, &Expectation::NoExpectation, false)?;
                    }

                    self.context.introduce()
                }

                _ => {
                    debug("lang item being silently ignored, ");
                    self.context.introduce()
                }
            },

            _ => self.check_expr_path(hir_id, qpath, &Expectation::NoExpectation)?,
        };

        match ty.kind() {
            TyKind::Adt(tys) => {
                for field in fields {
                    let ty = tys[&field.ident.name].clone();
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
    pub fn check_expr_array(&mut self, exprs: &[Expr]) -> Result<Ty> {
        let mut result = self.context.introduce();
        let item = self.context.introduce();

        for expr in exprs {
            // TODO: Return an error if the expressions don't all have the same (or compatible) provenance pairs
            self.check_expr(expr, &Expectation::NoExpectation, false)?;
        }

        result.kind = TyKind::Array(Box::new(item));

        Ok(result)
    }

    /// Checks the type of a tuple expression.
    pub fn check_expr_tup(&mut self, exprs: &[Expr]) -> Result<Ty> {
        let mut result = self.context.introduce();

        let mut items = vec![];

        for expr in exprs {
            items.push(self.check_expr(expr, &Expectation::NoExpectation, false)?);
        }

        result.kind = TyKind::Tuple(items);
        Ok(result)
    }

    /// Checks the type of a return expression.
    pub fn check_expr_ret(&mut self, expr: Option<&Expr>, expectation: &Expectation) -> Result<Ty> {
        match expr {
            Some(expr) => Ok(self.check_expr(expr, expectation, false)?),
            None => Ok(self.context.introduce()),
        }
    }

    /// Checks the type of a closure expression.
    pub fn check_expr_closure(&mut self, closure: &Closure, expectation: &Expectation) -> Result<Ty> {
        match expectation {
            Expectation::ExpectHasType(Ty {
                property,
                kind: TyKind::Fn(args, ret),
            }) => {
                // First, make sure the number of parameters is correct
                let count = closure.fn_decl.inputs.iter().count();

                if count != args.len() {
                    let msg = format!("expected {} parameter(s), found {count}", args.len());
                    return Err(self.tcx.dcx().span_err(closure.fn_arg_span.unwrap(), msg));
                }

                // Then, type check the body
                let body = self.tcx.hir().body(closure.body);

                for (param, arg) in body.params.iter().zip(args.clone()) {
                    // TODO: Use `process_pattern` instead
                    self.hir_map.insert(param.pat.hir_id, arg);
                }

                let expectation = Expectation::ExpectHasType(*ret.clone());
                let ret = self.check_expr(&body.value, &expectation, false)?;

                // Finally, return the expected type
                Ok(Ty {
                    property: property.clone(),
                    kind: TyKind::Fn(args.to_vec(), Box::new(ret)),
                })
            }
            _ => {
                // Either there is no expectation or the expectation isn't a function
                let count = closure.fn_decl.inputs.iter().count();

                let args: Vec<Ty> = (0..count).map(|_| self.context.universal()).collect();

                // Then, type check the body
                let body = self.tcx.hir().body(closure.body);

                for (param, arg) in body.params.iter().zip(args.clone()) {
                    // TODO: Use `process_pattern` instead
                    self.hir_map.insert(param.pat.hir_id, arg);
                }

                let ret = self.check_expr(&body.value, &Expectation::NoExpectation, false)?;

                let mut ty = self.context.introduce();
                ty.kind = TyKind::Fn(args, Box::new(ret));

                Ok(ty)
            }
        }
    }

    /// Checks the type of a projection.
    pub fn check_expr_field(&mut self, object: &Expr, field: Ident) -> Result<Ty> {
        match self.check_expr(object, &Expectation::NoExpectation, false)?.kind {
            TyKind::Adt(field_map) => {
                if !field_map.contains_key(&field.name) {
                    let msg = format!("unknown field '{}'", field.name);
                    Err(self.tcx.dcx().span_err(field.span, msg))
                } else {
                    Ok(field_map[&field.name].clone())
                }
            }

            TyKind::Tuple(fields) => {
                let index = field.name.as_str().parse::<usize>().unwrap();

                if index >= fields.len() {
                    let msg = format!("unknown field '{}'", field.name);
                    Err(self.tcx.dcx().span_err(field.span, msg))
                } else {
                    Ok(fields[index].clone())
                }
            }

            TyKind::Opaque | TyKind::Infer => {
                // For now, any field access with an unknown receiver will be universal
                // However, in the future, I should refine my type checking to avoid this
                Ok(self.context.universal())
            }

            _ => todo!(),
        }
    }

    /// Checks the type of a statement.
    pub fn check_stmt(&mut self, stmt: &Stmt, expectation: &Expectation) -> Result<Ty> {
        // TODO: Think more about what type should be returned by a statement
        match stmt.kind {
            StmtKind::Let(local) => {
                self.check_let_stmt(local)?;
                Ok(self.context.introduce())
            }
            StmtKind::Expr(expr) => self.check_expr(expr, expectation, false),
            StmtKind::Semi(expr) => self.check_expr(expr, expectation, false),
            StmtKind::Item(item_id) => {
                self.check_item(self.tcx.hir().item(item_id))?;
                Ok(self.context.introduce())
            }
        }
    }
}
