use std::collections::HashMap;

use coenobita_ast::ast::TyKind as ATyKind;
use coenobita_log::debug;
use coenobita_middle::map::Map;
use coenobita_middle::ty::TyKind;

use coenobita_parse::{create_parser, create_psess};
use itertools::Itertools;
use rustc_ast::AttrKind;
use rustc_hir::def::{CtorOf, DefKind, Res};
use rustc_hir::def_id::DefId;
use rustc_hir::{
    Arm, Block, BodyId, Closure, Expr, ExprField, ExprKind, FnSig, HirId, Impl, ImplItem,
    ImplItemKind, Item, ItemKind, LetExpr, LetStmt, MatchSource, PatKind, QPath, Stmt, StmtKind,
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
            attr: vec![Symbol::intern("cnbt"), Symbol::intern("tag")],
            tcx,
            def_map: Map::new(),
            hir_map: Map::new(),
        }
    }

    pub fn fn_ty(&mut self, def_id: DefId) -> Ty {
        debug(format!("Trying to get type of function {:#?}", def_id));

        match self.def_map.get(&def_id) {
            Some(ty) => ty.clone(),
            None => {
                // We haven't processed the definition of this function yet
                let _ = self.check_external_item_fn(def_id);
                self.def_map.get(&def_id).unwrap().clone()
            }
        }
    }

    pub fn adt_ty(&mut self, def_id_parent: DefId, def_id_variant: DefId, index: VariantIdx) -> Ty {
        debug(format!(
            "Trying to get type of adt (struct) {:#?}",
            def_id_variant
        ));

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
        debug(format!("trying to get ty of local w/hir id {:#?}", hir_id));
        match self.hir_map.get(&hir_id) {
            Some(ty) => ty.clone(),
            None => {
                debug("[WARN] Silently failed to get type of local var");
                self.context.introduce()
            }
        }
    }

    // TODO: Rename to something more informative
    pub fn process_pattern(&mut self, pat_kind: PatKind, ty: Ty, hir_id: HirId) {
        let ldid = hir_id.owner.to_def_id().as_local().unwrap();

        match pat_kind {
            PatKind::Binding(_, hir_id, _, _) => {
                self.hir_map.insert(hir_id, ty.clone());
            }

            PatKind::Struct(qpath, fields, _) => {
                debug("Checking patkind struct...");

                let res = self.tcx.typeck(ldid).qpath_res(&qpath, hir_id);
                match res {
                    Res::Def(def_kind, def_id) => match def_kind {
                        DefKind::Struct => {
                            let TyKind::Adt(tys) =
                                self.adt_ty(def_id, def_id, FIRST_VARIANT).kind()
                            else {
                                todo!()
                            };

                            for field in fields {
                                self.hir_map.insert(
                                    field.pat.hir_id,
                                    tys.get(&field.ident.name).cloned().unwrap(),
                                );
                            }
                        }
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

                            let TyKind::Adt(tys) = self.adt_ty(def_id, id, idx.into()).kind else {
                                todo!()
                            };

                            for field in fields {
                                self.hir_map.insert(
                                    field.pat.hir_id,
                                    tys.get(&field.ident.name).cloned().unwrap(),
                                );
                            }
                        }
                        DefKind::Ctor(_, _) => todo!(),
                        _ => todo!(),
                    },

                    Res::Err => {}

                    _ => {
                        debug(format!("Unsupported res {:#?}", res));
                        todo!()
                    }
                }
            }

            PatKind::Box(pat) => self.process_pattern(pat.kind, ty, pat.hir_id),
            PatKind::Deref(pat) => self.process_pattern(pat.kind, ty, pat.hir_id),
            PatKind::Err(_) | PatKind::Lit(_) => {}

            PatKind::TupleStruct(qpath, pats, _) => {
                match self.tcx.typeck(ldid).qpath_res(&qpath, hir_id) {
                    Res::Def(def_kind, def_id) => match def_kind {
                        // TODO: Basically rewrite all of this logic. It's incorrect!
                        DefKind::Ctor(_, _) => {
                            debug("Looking at ctor");

                            // Get the DefId of the variant
                            let def_id_variant = self.tcx.parent(def_id);

                            // Get the DefId of the parent
                            let def_id = self.tcx.parent(def_id_variant);

                            let ty = self.adt_ty(def_id, def_id, FIRST_VARIANT);

                            match ty.kind {
                                TyKind::Adt(map) => {
                                    let fields =
                                        map.iter().sorted_by_key(|(key, _)| key.to_string());

                                    for (pat, (_, fty)) in pats.iter().zip(fields) {
                                        self.process_pattern(pat.kind, fty.clone(), pat.hir_id);
                                    }
                                }

                                _ => todo!(),
                            }
                        }

                        DefKind::Struct => {
                            let TyKind::Adt(tys) =
                                self.adt_ty(def_id, def_id, FIRST_VARIANT).kind()
                            else {
                                todo!()
                            };

                            for (i, pat) in pats.iter().enumerate() {
                                let key = Symbol::intern(&i.to_string());
                                let ty = self.context.influence(tys[&key].clone());
                                self.process_pattern(pat_kind, ty, pat.hir_id);
                            }
                        }

                        _ => todo!(),
                    },

                    _ => todo!(),
                }
            }

            PatKind::Tuple(pats, _) => {
                match ty.kind {
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
                }
            }

            PatKind::Ref(pat, _) => self.process_pattern(pat.kind, ty, pat.hir_id),

            // Nothing to process
            PatKind::Path(_) => {}
            PatKind::Wild => {}
            PatKind::Never => {}

            _ => {
                debug(format!("Unsupported pattern kind {:#?}", pat_kind));
                todo!()
            }
        }
    }

    pub fn check_item(&mut self, item: &Item) -> Result {
        debug(format!("Checking item {:?}", item.kind));
        let did = item.owner_id.to_def_id();

        if self.tcx.get_attr(did, Symbol::intern("ignore")).is_some() {
            return Ok(());
        }

        match item.kind {
            ItemKind::Fn(signature, _, body_id) => self.check_item_fn(&signature, body_id),
            ItemKind::Impl(impl_) => self.check_item_impl(impl_),
            ItemKind::Struct(var_data, _) => {
                debug("checking struct ...");
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

                if let Ok(ty) = parser.parse_ity() {
                    match ty.kind {
                        ATyKind::Fn(ref arg_tys, _) => {
                            // Make sure the number of arguments matches
                            let actual = arg_tys.len();

                            if actual == expected {
                                // Note that we currently don't check whether types have the correct shape
                                self.def_map
                                    .insert(def_id, self.context.influence(ty.into()));
                            } else {
                                self.def_map.insert(def_id, default);

                                let msg = format!("Function must have {expected} arguments, but its Coenobita signature has {actual}");
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

        debug("Preapring to check item fn body...");
        let TyKind::Fn(args, expected) = self.fn_ty(def_id).kind else {
            panic!()
        };

        let body = self.tcx.hir().body(body_id);

        for (param, arg) in body.params.iter().zip(args) {
            self.hir_map.insert(param.pat.hir_id, arg);
        }

        let expr = body.value;
        let actual =
            self.check_expr(&body.value, &Expectation::ExpectHasType(*expected.clone()))?;

        debug("Done checking item fn body");

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
        let default = self.context.influence(Ty::ty_adt(field_defs.len()));

        let fields = match self.tcx.get_attrs_by_path(def_id, &self.attr).next() {
            Some(attr) => {
                // The `coenobita::tag` is guaranteed to be a normal attribute
                let AttrKind::Normal(normal) = &attr.kind else {
                    unreachable!()
                };

                debug("preparing to parse struct attr");

                let psess = create_psess(&self.tcx);
                let mut parser = create_parser(&psess, normal.item.args.inner_tokens());

                let mut map = HashMap::new();

                if let Ok(ty) = parser.parse_ity() {
                    match ty.kind {
                        ATyKind::Struct(ref elements) => {
                            debug(format!("We parsed a struct with elements {:#?}", elements));
                            for (name, ty) in elements {
                                map.insert(name.name, ty.clone().into());
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

                            if let Ok(ty) = parser.parse_ity() {
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

        debug(format!("the map for struct is {:#?}", fields));

        // Since this type is a struct definition, its flow pair will never be used and is only here for consistency
        let mut ty = self.context.introduce();
        ty.kind = TyKind::Adt(fields);

        self.def_map.insert(def_id, ty);

        Ok(())
    }

    pub fn check_external_item_fn(&mut self, def_id: DefId) -> Result {
        let expected = self
            .tcx
            .fn_sig(def_id)
            .skip_binder()
            .inputs()
            .iter()
            .count();

        let default = self.context.influence(Ty::ty_fn(expected));

        match self.tcx.get_attrs_by_path(def_id, &self.attr).next() {
            Some(attr) => {
                // The `coenobita::tag` is guaranteed to be a normal attribute
                let AttrKind::Normal(normal) = &attr.kind else {
                    unreachable!()
                };

                let psess = create_psess(&self.tcx);
                let mut parser = create_parser(&psess, normal.item.args.inner_tokens());

                if let Ok(ty) = parser.parse_ity() {
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
    /// pattern to its type. If an integrity annotation is provided, it's used as an expectation.
    pub fn check_let_stmt(&mut self, local: &LetStmt) -> Result {
        let mut processed = false;

        for attr in self.tcx.hir().attrs(local.hir_id) {
            if attr.path_matches(&self.attr) {
                let AttrKind::Normal(normal) = &attr.kind else {
                    unreachable!()
                };

                let psess = create_psess(&self.tcx);
                let mut parser = create_parser(&psess, normal.item.args.inner_tokens());

                if let Ok(ty) = parser.parse_ity() {
                    let expected: Ty = ty.into();
                    // TODO: Use `process_pattern`
                    self.hir_map.insert(local.pat.hir_id, expected.clone());

                    if let Some(expr) = local.init {
                        self.check_expr(expr, &Expectation::ExpectHasType(expected.clone()))?;
                    }
                };

                processed = true;
            }
        }

        if !processed {
            if let Some(expr) = local.init {
                let actual = self.check_expr(expr, &Expectation::NoExpectation)?;
                self.hir_map.insert(local.pat.hir_id, actual);
            } else {
                // TODO: Think about what should happen here
                self.hir_map
                    .insert(local.pat.hir_id, self.context.universal());
            }
        }

        Ok(())
    }

    /// Serves as the type checking entrypoint for all expressions.
    pub fn check_expr(&mut self, expr: &Expr, expectation: &Expectation) -> Result<Ty> {
        let ty = match expr.kind {
            ExprKind::Lit(_) => self.context.introduce(),
            ExprKind::Break(_, _) => self.context.introduce(),
            ExprKind::Continue(_) => self.context.introduce(),

            ExprKind::Unary(_, expr) => self.check_expr(expr, expectation)?,
            ExprKind::DropTemps(expr) => self.check_expr(expr, expectation)?,
            ExprKind::Cast(expr, _) => self.check_expr(expr, expectation)?,

            // TODO: Think about the typing rules for dereferencing and array indexing
            ExprKind::AddrOf(_, _, expr) => self.check_expr(expr, expectation)?,
            ExprKind::Index(expr, _, _) => self.check_expr(expr, expectation)?,

            ExprKind::Assign(dest, expr, _) => self.check_expr_assign(dest, expr, expectation)?,
            ExprKind::AssignOp(_, dest, expr) => self.check_expr_assign(dest, expr, expectation)?,

            ExprKind::Block(block, _) => self.check_expr_block(block, expectation)?,
            ExprKind::Loop(block, _, _, _) => self.check_expr_block(block, expectation)?,

            ExprKind::Path(qpath) => self.check_expr_path(expr.hir_id, &qpath, expectation)?,
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
            ExprKind::Struct(qpath, fields, _) => {
                self.check_expr_struct(expr.hir_id, qpath, fields)?
            }

            _ => {
                debug(format!("Skipping expression of kind {:#?}", expr.kind));
                todo!()
            }
        };

        expectation.check(self.tcx, ty, expr.span)
    }

    /// Checks the type of a path expression.
    pub fn check_expr_path(
        &mut self,
        hir_id: HirId,
        qpath: &QPath,
        expectation: &Expectation,
    ) -> Result<Ty> {
        let local_def_id = hir_id.owner.to_def_id().as_local().unwrap();

        let ty = match self.tcx.typeck(local_def_id).qpath_res(qpath, hir_id) {
            Res::Local(hir_id) => self.local(hir_id),
            Res::Def(def_kind, def_id) => match def_kind {
                // TODO: Check that `AssocFn` is handled properly
                DefKind::Fn | DefKind::AssocFn => self.fn_ty(def_id),

                // TODO: Test this thoroughly
                DefKind::Ctor(ctor_of, ctor_kind) => match ctor_of {
                    CtorOf::Struct => {
                        debug(format!("checking CTOR of defid {:?}", def_id));
                        let def_id = self.tcx.parent(def_id);
                        self.adt_ty(def_id, def_id, FIRST_VARIANT)
                    }

                    CtorOf::Variant => {
                        debug(format!("checking CTOR variatn of defid {:?}", def_id));

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
                DefKind::AssocConst | DefKind::Const => self.context.introduce(),

                _ => todo!(),
            },

            _ => todo!(),
        };

        expectation.check(self.tcx, ty, qpath.span())
    }

    /// Checks the type of a call expression.
    pub fn check_expr_call(&mut self, func: &Expr, args: &[Expr], span: Span) -> Result<Ty> {
        let fty = self.check_expr(func, &Expectation::NoExpectation)?;

        match fty.kind() {
            TyKind::Fn(arg_tys, ret_ty) => {
                if args.len() != arg_tys.len() {
                    let msg = format!(
                        "expected {} argument(s), found {}",
                        arg_tys.len(),
                        args.len()
                    );

                    return Err(self.tcx.dcx().span_err(span, msg));
                }

                for (ty, expr) in arg_tys.into_iter().zip(args) {
                    let expectation = Expectation::ExpectHasType(ty);
                    self.check_expr(expr, &expectation)?;
                }

                Ok(*ret_ty)
            }

            TyKind::Adt(_) => {
                // We don't really know whether the underlying ADT has named or unnamed fields, so pretend we're good
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
                            let msg = format!(
                                "expected {} argument(s), found {}",
                                arg_tys.len(),
                                args.len()
                            );

                            return Err(self.tcx.dcx().span_err(span, msg));
                        }

                        let args = std::iter::once(receiver).chain(args.iter());
                        for (ty, expr) in arg_tys.into_iter().zip(args) {
                            let expectation = Expectation::ExpectHasType(ty);
                            self.check_expr(expr, &expectation)?;
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
        let ity = self.check_expr(guard, &Expectation::NoExpectation)?;
        self.context.enter(&ity);

        let mut rty = self.check_expr(then_expr, expectation)?;

        if let Some(else_expr) = else_expr {
            rty = rty.merge(self.check_expr(else_expr, expectation)?);
        }

        self.context.exit();
        Ok(rty)
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

                self.check_expr(&args[0], &Expectation::NoExpectation)?
            }

            _ => self.check_expr(guard, &Expectation::NoExpectation)?,
        };

        self.context.enter(&ity);

        let mut result = self.context.introduce();

        for arm in arms {
            self.process_pattern(arm.pat.kind, ity.clone(), arm.hir_id);
            result = result.merge(self.check_expr(arm.body, expectation)?)
        }

        self.context.exit();
        Ok(result)
    }

    /// Checks the type of a `let` expression. These typically appear as the guards of `if` expressions.
    pub fn check_expr_let(&mut self, let_expr: &LetExpr, expectation: &Expectation) -> Result<Ty> {
        let ty = self.check_expr(let_expr.init, expectation)?;
        self.process_pattern(let_expr.pat.kind, ty.clone(), let_expr.pat.hir_id);
        Ok(ty)
    }

    /// Checks the type of a binary expression.
    pub fn check_expr_binary(
        &mut self,
        lhs: &Expr,
        rhs: &Expr,
        expectation: &Expectation,
    ) -> Result<Ty> {
        let ty = self
            .check_expr(lhs, expectation)?
            .merge(self.check_expr(rhs, expectation)?);

        Ok(self.context.influence(ty))
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
                self.check_expr(expr, expectation)
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
    pub fn check_expr_assign(
        &mut self,
        dest: &Expr,
        expr: &Expr,
        expectation: &Expectation,
    ) -> Result<Ty> {
        let expected = self.check_expr(dest, expectation)?;
        let expectation = &Expectation::ExpectHasType(expected.clone());
        self.check_expr(expr, expectation)
    }

    /// Checks the type of a struct expression.
    pub fn check_expr_struct(
        &mut self,
        hir_id: HirId,
        qpath: &QPath,
        fields: &[ExprField],
    ) -> Result<Ty> {
        let local_def_id = hir_id.owner.to_def_id().as_local().unwrap();

        match self.tcx.typeck(local_def_id).qpath_res(qpath, hir_id) {
            Res::Def(def_kind, def_id) => match def_kind {
                DefKind::Struct => {
                    let sty = self.adt_ty(def_id, def_id, FIRST_VARIANT);

                    match sty.kind() {
                        TyKind::Adt(field_tys) => {
                            for field in fields {
                                let ty = field_tys[&field.ident.name].clone();
                                let expectation = &Expectation::ExpectHasType(ty);
                                self.check_expr(field.expr, expectation)?;
                            }

                            // We need to replace the flow pair of `ty` with the introduction pair
                            let mut ty = self.context.introduce();
                            ty.kind = TyKind::Adt(field_tys);

                            Ok(ty)
                        }

                        _ => {
                            let msg =
                                format!("'{}' is not a struct", qpath_string(self.tcx, qpath));
                            Err(self.tcx.dcx().span_err(qpath.span(), msg))
                        }
                    }
                }

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

                    let ty = self.adt_ty(def_id, id, idx.into());
                    let TyKind::Adt(tys) = &ty.kind else { todo!() };

                    for field in fields {
                        let ty = tys[&field.ident.name].clone();
                        let expectation = Expectation::ExpectHasType(ty);
                        self.check_expr(field.expr, &expectation)?;
                    }

                    Ok(ty)
                }

                _ => {
                    debug(format!("Unsupported def kind {:#?}", def_kind));
                    todo!()
                }
            },

            _ => todo!(),
        }
    }

    /// Checks the type of an array expression.
    pub fn check_expr_array(&mut self, exprs: &[Expr]) -> Result<Ty> {
        let mut result = self.context.introduce();
        let mut item = self.context.introduce();

        for expr in exprs {
            item = item.merge(self.check_expr(expr, &Expectation::NoExpectation)?);
        }

        result.kind = TyKind::Array(Box::new(item));

        Ok(result)
    }

    /// Checks the type of a tuple expression.
    pub fn check_expr_tup(&mut self, exprs: &[Expr]) -> Result<Ty> {
        let mut result = self.context.introduce();

        let mut items = vec![];

        for expr in exprs {
            items.push(self.check_expr(expr, &Expectation::NoExpectation)?);
        }

        result.kind = TyKind::Tuple(items);
        Ok(result)
    }

    /// Checks the type of a return expression.
    pub fn check_expr_ret(&mut self, expr: Option<&Expr>, expectation: &Expectation) -> Result<Ty> {
        match expr {
            Some(expr) => Ok(self.check_expr(expr, expectation)?),
            None => Ok(self.context.introduce()),
        }
    }

    /// Checks the type of a closure expression.
    pub fn check_expr_closure(
        &mut self,
        closure: &Closure,
        expectation: &Expectation,
    ) -> Result<Ty> {
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
                let ret = self.check_expr(&body.value, &expectation)?;

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

                let ret = self.check_expr(&body.value, &Expectation::NoExpectation)?;

                let mut ty = self.context.introduce();
                ty.kind = TyKind::Fn(args, Box::new(ret));

                Ok(ty)
            }
        }
    }

    /// Checks the type of a projection.
    pub fn check_expr_field(&mut self, object: &Expr, field: Ident) -> Result<Ty> {
        match self.check_expr(object, &Expectation::NoExpectation)?.kind {
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
                self.check_let_stmt(local);
                Ok(self.context.introduce())
            }
            StmtKind::Expr(expr) => self.check_expr(expr, expectation),
            StmtKind::Semi(expr) => self.check_expr(expr, expectation),
            StmtKind::Item(item_id) => {
                self.check_item(self.tcx.hir().item(item_id))?;
                Ok(self.context.introduce())
            }
        }
    }
}

fn qpath_string<'tcx>(tcx: TyCtxt<'tcx>, qpath: &QPath<'_>) -> String {
    match qpath {
        QPath::Resolved(_, path) => {
            // If the path is resolved, we can get the DefId and print it
            if let Res::Def(_, def_id) = path.res {
                let path_str = tcx.def_path_str(def_id);
                format!("Resolved QPath: {}", path_str)
            } else {
                todo!()
            }
        }
        QPath::TypeRelative(ty, segment) => {
            // If it's a type-relative path, we print it in a type-relative way
            format!("TypeRelative QPath: {:?}::{}", ty, segment.ident)
        }
        QPath::LangItem(lang_item, _) => {
            // Language items can be printed directly by their known name
            format!("LangItem QPath: {:?}", lang_item)
        }
    }
}
