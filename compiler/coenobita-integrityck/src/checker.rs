use std::collections::HashMap;

use coenobita_ast::ast::TyKind as ATyKind;
use coenobita_log::debug;
use coenobita_middle::flow::FlowPair;
use coenobita_middle::map::Map;
use coenobita_middle::ty::{Ty as _Ty, TyKind};

use coenobita_parse::{create_parser, create_psess};
use rustc_ast::AttrKind;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::def_id::DefId;
use rustc_hir::{
    Arm, Block, BodyId, Closure, Expr, ExprField, ExprKind, FnSig, HirId, Impl, ImplItem,
    ImplItemKind, Item, ItemKind, LetExpr, LetStmt, LoopSource, MatchSource, PatKind, PathSegment,
    QPath, Stmt, StmtKind,
};
use rustc_middle::query::queries::def_kind;
use rustc_middle::ty::layout::HasTyCtxt;
use rustc_middle::ty::{self, FieldDef, TyCtxt};
use rustc_span::symbol::{kw, Ident};
use rustc_span::{ErrorGuaranteed, Symbol};

use crate::context::Context;

pub struct Checker<'cnbt, 'tcx> {
    crate_name: &'cnbt str,
    attr: Vec<Symbol>,
    tcx: TyCtxt<'tcx>,
    def_map: Map<DefId, Ty>,
    hir_map: Map<HirId, Ty>,
    context: Context<'cnbt>,
}

type Ty = _Ty<FlowPair>;
type Result<T = ()> = std::result::Result<T, ErrorGuaranteed>;

impl<'cnbt, 'tcx> Checker<'cnbt, 'tcx> {
    pub fn new(crate_name: &'cnbt str, tcx: TyCtxt<'tcx>, context: Context<'cnbt>) -> Self {
        Checker {
            crate_name,
            context,
            attr: vec![Symbol::intern("cnbt"), Symbol::intern("tag")],
            tcx,
            def_map: Map::new(),
            hir_map: Map::new(),
        }
    }

    pub fn function_ty(&mut self, def_id: DefId) -> Ty {
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

    pub fn adt_ty(&mut self, def_id: DefId) -> Ty {
        debug(format!("Trying to get type of adt (struct) {:#?}", def_id));

        match self.def_map.get(&def_id) {
            Some(ty) => ty.clone(),
            None => {
                // We haven't processed the definition of this function yet
                let field_defs = &self
                    .tcx
                    .adt_def(def_id)
                    .variants()
                    .iter()
                    .next()
                    .unwrap()
                    .fields
                    .raw;

                let _ = self.check_item_struct(def_id, field_defs);
                self.def_map.get(&def_id).unwrap().clone()
            }
        }
    }

    pub fn local_ty(&mut self, hir_id: HirId) -> Result<Ty> {
        match self.hir_map.get(&hir_id) {
            Some(ty) => Ok(ty.clone()),
            None => {
                // We haven't processed this variable yet, which means it has no annotation
                let default = self.context.universal();
                self.hir_map.insert(hir_id, default.clone());
                Ok(default)
            }
        }
    }

    pub fn process_pat(&mut self, pat_kind: PatKind, ty: Ty, hir_id: HirId) {
        let ldid = hir_id.owner.to_def_id().as_local().unwrap();

        match pat_kind {
            PatKind::Binding(_, hir_id, _, _) => {
                self.hir_map.insert(hir_id, ty.clone());
            }

            PatKind::Struct(qpath, fields, _) => {
                debug("Checking patkind struct...");

                let res = self.tcx.typeck(ldid).qpath_res(&qpath, hir_id);
                match res {
                    Res::Def(def_kind, def_id) => {
                        let TyKind::Adt(tys) = self.adt_ty(def_id).kind() else {
                            todo!()
                        };

                        for field in fields {
                            self.hir_map.insert(
                                field.pat.hir_id,
                                tys.get(&field.ident.name).cloned().unwrap(),
                            );
                        }
                    }

                    Res::Err => {}

                    _ => {
                        debug(format!("Unsupported res {:#?}", res));
                        todo!()
                    }
                }
            }

            PatKind::Box(pat) => self.process_pat(pat.kind, ty, pat.hir_id),
            PatKind::Deref(pat) => self.process_pat(pat.kind, ty, pat.hir_id),
            PatKind::Err(_) | PatKind::Lit(_) => {}

            PatKind::TupleStruct(qpath, pats, _) => {
                match self.tcx.typeck(ldid).qpath_res(&qpath, hir_id) {
                    Res::Def(def_kind, def_id) => {
                        debug("Checknig a res def...");

                        match def_kind {
                            DefKind::Ctor(ctor_of, ctor_kind) => {
                                debug(format!(
                                    "Checking constructor of {:?}, kind {:?}",
                                    ctor_of, ctor_kind
                                ));
                                todo!()
                            }

                            DefKind::Struct => {
                                let TyKind::Adt(tys) = self.adt_ty(def_id).kind() else {
                                    todo!()
                                };

                                for (i, pat) in pats.iter().enumerate() {
                                    let key = Symbol::intern(&i.to_string());
                                    let ty = self.context.influence(tys[&key].clone());
                                    self.process_pat(pat_kind, ty, pat.hir_id);
                                }
                            }

                            _ => todo!(),
                        }
                    }

                    _ => todo!(),
                }
            }

            _ => todo!(),
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
        let TyKind::Fn(args, expected) = self.function_ty(def_id).kind else {
            panic!()
        };

        let body = self.tcx.hir().body(body_id);

        for (param, arg) in body.params.iter().zip(args) {
            self.hir_map.insert(param.pat.hir_id, arg);
        }

        let expr = body.value;
        let actual = self.check_expr(&body.value)?;

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

        // Since this type is a struct definition, its flow pair will never be used and is only here for consistency
        let mut ty = self.context.universal();
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

    pub fn check_let_stmt(&mut self, local: &LetStmt) {
        debug("Checking a let stmt!");

        for attr in self.tcx.hir().attrs(local.hir_id) {
            if attr.path_matches(&self.attr) {
                // SAFETY - The `coenobita::tag` is guaranteed to be a normal attribute
                let AttrKind::Normal(normal) = &attr.kind else {
                    unreachable!()
                };

                let psess = create_psess(&self.tcx);
                let mut parser = create_parser(&psess, normal.item.args.inner_tokens());

                if let Ok(ty) = parser.parse_ity() {
                    let expected: Ty = ty.into();
                    self.hir_map.insert(local.pat.hir_id, expected.clone());

                    if let Some(expr) = local.init {
                        if let Ok(actual) = self.check_expr(expr) {
                            if !actual.satisfies(&expected) {
                                let msg = format!("Expected {expected}, found {actual}");
                                self.tcx.dcx().span_err(expr.span, msg);
                            }
                        }
                    }
                };
            }
        }
    }

    pub fn check_expr(&mut self, expr: &Expr) -> Result<Ty> {
        debug(format!("> expr kind is {:?}", expr.kind));

        match expr.kind {
            ExprKind::Lit(_) => Ok(self.context.introduce()),
            ExprKind::Path(qpath) => self.check_expr_path(expr.hir_id, &qpath),
            ExprKind::Call(func, args) => self.check_expr_call(func, args),
            ExprKind::MethodCall(_, receiver, args, _) => {
                self.check_method_call(expr.hir_id, receiver, args)
            }
            ExprKind::If(guard, then_expr, else_expr) => {
                self.check_expr_if(guard, then_expr, else_expr)
            }
            ExprKind::Match(guard, arms, source) => self.check_expr_match(guard, arms, source),
            ExprKind::Let(let_expr) => self.check_expr_let(let_expr),
            ExprKind::Binary(_, lhs, rhs) => self.check_expr_binary(lhs, rhs),
            ExprKind::Unary(_, expr) => self.check_expr(expr),
            ExprKind::Block(block, _) => self.check_expr_block(block),
            ExprKind::Assign(dest, expr, _) => self.check_expr_assign(dest, expr),
            ExprKind::AssignOp(_, dest, expr) => self.check_expr_assign(dest, expr),
            ExprKind::AddrOf(_, _, expr) => self.check_expr(expr),
            ExprKind::Array(exprs) => self.check_expr_array(exprs),
            ExprKind::Tup(exprs) => self.check_expr_tup(exprs),
            ExprKind::Loop(block, _, _, _) => {
                // self.check_expr_loop( block, loop_source)
                self.check_expr_block(block)
            }
            ExprKind::Struct(qpath, fields, _) => {
                // TODO: Account for `..base` (the last field of the tuple above)
                self.check_expr_struct(expr.hir_id, qpath, fields)
            }

            // This kind of expression is typically generated by the compiler
            ExprKind::DropTemps(expr) => self.check_expr(expr),

            // Nothing needs to happen here but `Ty` must be returned, so we pretend `break` has a type
            ExprKind::Break(_, _) => Ok(self.context.introduce()),

            ExprKind::Ret(expr) => self.check_expr_ret(expr),
            ExprKind::Cast(expr, _) => self.check_expr(expr),
            ExprKind::Closure(closure) => self.check_expr_closure(closure),
            ExprKind::Index(expr, _, _) => {
                // TODO: Decide the semantics for indexing an array
                self.check_expr(expr)
            }
            ExprKind::Field(obj, ident) => self.check_expr_field(obj, ident),

            _ => {
                debug(format!("Skipping expr of kind {:#?}", expr.kind));
                todo!()
            }
        }
    }

    pub fn check_expr_path(&mut self, hir_id: HirId, qpath: &QPath) -> Result<Ty> {
        let local_def_id = hir_id.owner.to_def_id().as_local().unwrap();

        match self.tcx.typeck(local_def_id).qpath_res(qpath, hir_id) {
            Res::Local(hir_id) => self.local_ty(hir_id),
            Res::Def(def_kind, def_id) => match def_kind {
                DefKind::Fn => Ok(self.function_ty(def_id)),

                DefKind::AssocFn => {
                    // TODO: Associated functions are not actually tracked yet
                    Ok(self.function_ty(def_id))
                }

                _ => {
                    debug(format!("Unsupported path expr def kind {:?}", def_kind));
                    debug(format!("path is {:?}", qpath));

                    todo!()
                }
            },
            _ => todo!(),
        }
    }

    pub fn check_expr_call(&mut self, func: &Expr, args: &[Expr]) -> Result<Ty> {
        let fty = self.check_expr(func)?;

        match fty.kind() {
            TyKind::Fn(arg_tys, ret_ty) => {
                if args.len() != arg_tys.len() {
                    todo!()
                }

                for (expected, expr) in arg_tys.iter().zip(args) {
                    let actual = self.check_expr(expr)?;

                    if !actual.satisfies(expected) {
                        let msg = format!("Expected {expected}, found {actual}");
                        self.tcx.dcx().span_err(expr.span, msg);
                    }
                }

                Ok(*ret_ty)
            }

            _ => todo!(),
        }
    }

    pub fn check_method_call(
        &mut self,

        hir_id: HirId,
        receiver: &Expr,
        args: &[Expr],
    ) -> Result<Ty> {
        let (def_kind, def_id) = self
            .tcx
            .typeck(hir_id.owner.def_id)
            .type_dependent_def(hir_id)
            .unwrap();

        match def_kind {
            DefKind::AssocFn => {
                debug("Getting the ty of an assoc fn");
                let fty = self.function_ty(def_id);
                debug(format!("It is {:?}", fty));

                match fty.kind() {
                    TyKind::Fn(arg_tys, ret_ty) => {
                        // We subtract one to account for the receiver
                        if args.len() != arg_tys.len() - 1 {
                            todo!()
                        }

                        let args = std::iter::once(receiver).chain(args.iter());

                        for (expected, expr) in arg_tys.iter().zip(args) {
                            let actual = self.check_expr(expr)?;

                            if !actual.satisfies(expected) {
                                let msg = format!("Expected {expected}, found {actual}");
                                self.tcx.dcx().span_err(expr.span, msg);
                            }
                        }

                        Ok(*ret_ty)
                    }

                    _ => todo!(),
                }
            }
            DefKind::Fn => {
                unreachable!()
            }
            _ => todo!(),
        }
    }

    pub fn check_expr_if(
        &mut self,

        guard: &Expr,
        then_expr: &Expr,
        else_expr: Option<&Expr>,
    ) -> Result<Ty> {
        let ity = self.check_expr(guard)?;
        self.context.enter(&ity);

        let mut rty = self.check_expr(then_expr)?;

        if let Some(else_expr) = else_expr {
            rty = rty.merge(self.check_expr(else_expr)?);
        }

        self.context.exit();
        Ok(rty)
    }

    pub fn check_expr_match(
        &mut self,

        guard: &Expr,
        arms: &[Arm],
        source: MatchSource,
    ) -> Result<Ty> {
        debug(format!(
            "|| Checking match expr: {:#?}\n||match source: {:?}",
            guard, source
        ));

        let ity = match source {
            MatchSource::ForLoopDesugar => {
                // The guard expression is a function call, and we want the argument
                let ExprKind::Call(_, args) = guard.kind else {
                    unreachable!()
                };

                let t = self.check_expr(&args[0])?;
                debug(format!("For loop ctx is {:?}", t));
                t
            }

            _ => self.check_expr(guard)?,
        };

        self.context.enter(&ity);

        let mut result = self.context.introduce();

        for arm in arms {
            debug(format!("Pat kind for match arm is {:#?}", arm.pat.kind));

            self.process_pat(arm.pat.kind, ity.clone(), arm.hir_id);

            result = result.merge(self.check_expr(arm.body)?)
        }

        self.context.exit();
        Ok(result)
    }

    pub fn check_expr_let(&mut self, let_expr: &LetExpr) -> Result<Ty> {
        let ty = self.check_expr(let_expr.init)?;

        self.process_pat(let_expr.pat.kind, ty.clone(), let_expr.pat.hir_id);
        Ok(ty)
    }

    pub fn check_expr_binary(&mut self, lhs: &Expr, rhs: &Expr) -> Result<Ty> {
        let ty = self.check_expr(lhs)?.merge(self.check_expr(rhs)?);

        Ok(self.context.influence(ty))
    }

    pub fn check_expr_block(&mut self, block: &Block) -> Result<Ty> {
        for stmt in block.stmts {
            debug("Checking a statement...");
            self.check_stmt(stmt)?;
        }

        match block.expr {
            Some(expr) => {
                debug("The last stmt is an expr");
                // This expression occurs at the very end without a semicolon
                self.check_expr(expr)
            }

            None if block.stmts.len() > 0 => {
                debug("The last stmt is NOT an expr");
                self.check_stmt(&block.stmts[block.stmts.len() - 1])
            }

            _ => Ok(self.context.introduce()),
        }
    }

    pub fn check_expr_assign(&mut self, dest: &Expr, expr: &Expr) -> Result<Ty> {
        let expected = self.check_expr(dest)?;
        let actual = self.check_expr(expr)?;

        if actual.satisfies(&expected) {
            Ok(expected)
        } else {
            let msg = format!("Expected {expected}, found {actual}");
            Err(self.tcx.dcx().span_err(expr.span, msg))
        }
    }

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
                    let sty = self.adt_ty(def_id);

                    match sty.kind() {
                        TyKind::Adt(field_tys) => {
                            for field in fields {
                                let expected = &field_tys[&field.ident.name];
                                let actual = self.check_expr(field.expr)?;

                                if !actual.satisfies(expected) {
                                    let msg = format!("Expected {expected}, found {actual}");
                                    return Err(self.tcx.dcx().span_err(field.expr.span, msg));
                                }
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

                _ => todo!(),
            },

            _ => todo!(),
        }
    }

    pub fn check_stmt(&mut self, stmt: &Stmt) -> Result<Ty> {
        match stmt.kind {
            StmtKind::Expr(expr) => {
                let _ = self.check_expr(expr);
            }
            StmtKind::Semi(expr) => {
                let _ = self.check_expr(expr);
            }
            StmtKind::Let(local) => self.check_let_stmt(local),
            StmtKind::Item(item_id) => {
                let _ = self.check_item(self.tcx.hir().item(item_id));
            }
        };

        Ok(self.context.universal())
    }

    pub fn check_expr_array(&mut self, exprs: &[Expr]) -> Result<Ty> {
        let mut result = self.context.introduce();
        let mut item = self.context.introduce();

        for expr in exprs {
            item = item.merge(self.check_expr(expr)?);
        }

        result.kind = TyKind::Array(Box::new(item));

        Ok(result)
    }

    pub fn check_expr_tup(&mut self, exprs: &[Expr]) -> Result<Ty> {
        let mut result = self.context.introduce();

        let mut items = vec![];

        for expr in exprs {
            items.push(self.check_expr(expr)?);
        }

        result.kind = TyKind::Tuple(items);
        Ok(result)
    }

    pub fn check_expr_ret(&mut self, expr: Option<&Expr>) -> Result<Ty> {
        match expr {
            Some(expr) => Ok(self.check_expr(expr)?),
            None => Ok(self.context.introduce()),
        }
    }

    // TODO: Implement bidirectional type checking
    pub fn check_expr_closure(&mut self, closure: &Closure) -> Result<Ty> {
        let arg_count = closure.fn_decl.inputs.len();

        debug("TODO: Implement closure logic via bidirection type checking");

        let mut ty = self.context.introduce();
        ty.kind = TyKind::Fn(
            (0..arg_count).map(|_| self.context.introduce()).collect(),
            Box::new(self.context.introduce()),
        );

        Ok(ty)
    }

    pub fn check_expr_field(&mut self, obj: &Expr, field: Ident) -> Result<Ty> {
        match self.check_expr(obj)?.kind {
            TyKind::Adt(field_map) => {
                if !field_map.contains_key(&field.name) {
                    let msg = format!("object has no field '{}'", field.name);
                    Err(self.tcx.dcx().span_err(field.span, msg))
                } else {
                    Ok(field_map[&field.name].clone())
                }
            }

            TyKind::Tuple(fields) => {
                let index = field.name.as_str().parse::<usize>().unwrap();

                if index >= fields.len() {
                    let msg = format!("field access out of bounds");
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
