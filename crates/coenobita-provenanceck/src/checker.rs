use std::collections::HashMap;

use coenobita_ast::ast::TyKind as ATyKind;
use coenobita_log::debug;
use coenobita_middle::map::Map;
use coenobita_middle::provenance::{Provenance, ProvenancePair};
use coenobita_middle::ty::{Ty as _Ty, TyKind};
use coenobita_parse::{create_parser, create_psess};

use rustc_ast::AttrKind;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::def_id::DefId;
use rustc_hir::{
    Arm, Block, BodyId, Expr, ExprField, ExprKind, FnSig, HirId, Item, ItemKind, LetStmt, QPath,
    Stmt, StmtKind,
};
use rustc_middle::ty::{self, FieldDef, TyCtxt};
use rustc_span::{ErrorGuaranteed, Symbol};

type Ty = _Ty<ProvenancePair>;

pub struct Checker<'cnbt, 'tcx> {
    crate_name: &'cnbt str,
    attr: Vec<Symbol>,
    tcx: TyCtxt<'tcx>,
    def_map: Map<DefId, Ty>,
    hir_map: Map<HirId, Ty>,
}

type Result<T = ()> = std::result::Result<T, ErrorGuaranteed>;

impl<'cnbt, 'tcx> Checker<'cnbt, 'tcx> {
    pub fn new(crate_name: &'cnbt str, tcx: TyCtxt<'tcx>) -> Self {
        Checker {
            crate_name,
            attr: vec![Symbol::intern("cnbt"), Symbol::intern("provenance")],
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
                let _ = self.new_check_item_fn(def_id);
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
                let default = self.universal();
                self.hir_map.insert(hir_id, default.clone());
                Ok(default)
            }
        }
    }

    pub fn universal(&self) -> Ty {
        Ty::new(
            ProvenancePair(Provenance::Universal, Provenance::Universal),
            TyKind::Abs,
        )
    }

    pub fn introduce(&self) -> Ty {
        Ty::new(
            ProvenancePair(
                Provenance::Specific(self.crate_name.to_owned()),
                Provenance::Specific(self.crate_name.to_owned()),
            ),
            TyKind::Abs,
        )
    }

    pub fn check_item(&mut self, item: &Item) -> Result {
        debug(format!("Checking item {:?}", item.kind));
        let did = item.owner_id.to_def_id();

        if self.tcx.get_attr(did, Symbol::intern("ignore")).is_some() {
            return Ok(());
        }

        match item.kind {
            ItemKind::Fn(signature, _, body_id) => self.check_item_fn(&signature, body_id),
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

    pub fn check_item_fn(&mut self, signature: &FnSig, body_id: BodyId) -> Result {
        let def_id = body_id.hir_id.owner.to_def_id();
        let expected = signature.decl.inputs.len();
        let default = Ty::ty_fn(expected);

        match self.tcx.get_attrs_by_path(def_id, &self.attr).next() {
            Some(attr) => {
                // The `coenobita::tag` is guaranteed to be a normal attribute
                let AttrKind::Normal(normal) = &attr.kind else {
                    unreachable!()
                };

                let psess = create_psess(&self.tcx);
                let mut parser = create_parser(&psess, normal.item.args.inner_tokens());

                if let Ok(ty) = parser.parse_pty() {
                    match ty.kind {
                        ATyKind::Fn(ref arg_tys, _) => {
                            // Make sure the number of arguments matches
                            let actual = arg_tys.len();

                            if actual == expected {
                                // Note that we currently don't check whether types have the correct shape
                                self.def_map.insert(def_id, ty.into());
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
        let expr = self.tcx.hir().body(body_id).value;
        let actual = self.check_expr(expr)?;
        debug("Done checking item fn body");

        let TyKind::Fn(_, expected) = self.function_ty(def_id).kind else {
            panic!()
        };

        if !actual.satisfies(&expected) {
            let msg = format!("Expected {expected}, found {actual}");
            self.tcx.dcx().span_err(expr.span, msg);
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

                    if let Ok(ty) = parser.parse_pty() {
                        fields.insert(field.name, ty.into());
                    };
                }

                _ => {
                    fields.insert(field.name, self.universal());
                }
            };
        }

        // Since this type is a struct definition, its flow pair will never be used and is only here for consistency
        let mut ty = self.universal();
        ty.kind = TyKind::Adt(fields);

        self.def_map.insert(def_id, ty);

        Ok(())
    }

    // TODO: Change the name of this function
    pub fn new_check_item_fn(&mut self, def_id: DefId) -> Result {
        let expected = self
            .tcx
            .fn_sig(def_id)
            .skip_binder()
            .inputs()
            .iter()
            .count();

        let default = Ty::ty_fn(expected);

        match self.tcx.get_attrs_by_path(def_id, &self.attr).next() {
            Some(attr) => {
                // The `coenobita::tag` is guaranteed to be a normal attribute
                let AttrKind::Normal(normal) = &attr.kind else {
                    unreachable!()
                };

                let psess = create_psess(&self.tcx);
                let mut parser = create_parser(&psess, normal.item.args.inner_tokens());

                if let Ok(ty) = parser.parse_pty() {
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
        for attr in self.tcx.hir().attrs(local.hir_id) {
            if attr.path_matches(&self.attr) {
                let AttrKind::Normal(normal) = &attr.kind else {
                    unreachable!()
                };

                let psess = create_psess(&self.tcx);
                let mut parser = create_parser(&psess, normal.item.args.inner_tokens());

                if let Ok(ty) = parser.parse_pty() {
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
            ExprKind::Lit(_) => Ok(self.introduce()),
            ExprKind::Path(qpath) => self.check_expr_path(expr.hir_id, &qpath),
            ExprKind::Call(func, args) => self.check_expr_call(func, args),
            ExprKind::If(guard, then_expr, else_expr) => {
                self.check_expr_if(guard, then_expr, else_expr)
            }
            ExprKind::Match(guard, arms, _) => self.check_expr_match(guard, arms),
            ExprKind::Binary(_, lhs, rhs) => self.check_expr_binary(lhs, rhs),
            ExprKind::Unary(_, expr) => self.check_expr(expr),
            ExprKind::Block(block, _) => self.check_expr_block(block),
            ExprKind::Assign(dest, expr, _) => self.check_expr_assign(dest, expr),
            ExprKind::AssignOp(_, dest, expr) => self.check_expr_assign(dest, expr),
            ExprKind::AddrOf(_, _, expr) => self.check_expr(expr),
            ExprKind::Struct(qpath, fields, _) => {
                // TODO: Account for `..base` (the last field of the tuple above)
                self.check_expr_struct(expr.hir_id, qpath, fields)
            }

            // This kind of expression is typically generated by the compiler
            ExprKind::DropTemps(expr) => self.check_expr(expr),

            // Loops with conditions, like `for i in 0..10 { ... }` are desugared into unconditional loops with match statements inside
            ExprKind::Loop(block, _, _, _) => self.check_expr_block(block),

            // Nothing needs to happen here but `Ty` must be returned, so we pretend `break` has a type
            ExprKind::Break(_, _) => Ok(self.introduce()),

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

    pub fn check_expr_if(
        &mut self,
        guard: &Expr,
        then_expr: &Expr,
        else_expr: Option<&Expr>,
    ) -> Result<Ty> {
        self.check_expr(guard)?;

        let tty = self.check_expr(then_expr)?;
        let ety = match else_expr {
            Some(expr) => self.check_expr(expr)?,
            None => tty.clone(),
        };

        if !ety.satisfies(&tty) {
            let l1 = "`if` and `else` have incompatible types";
            let msg = format!("{l1}\nexpected {tty}, found {ety}");
            Err(self.tcx.dcx().span_err(else_expr.unwrap().span, msg))
        } else {
            Ok(tty)
        }
    }

    pub fn check_expr_match(&mut self, guard: &Expr, arms: &[Arm]) -> Result<Ty> {
        self.check_expr(guard)?;

        if arms.is_empty() {
            todo!()
        }

        let mut arms = arms.iter();
        let result = self.check_expr(arms.next().unwrap().body)?;

        loop {
            match arms.next() {
                Some(arm) => {
                    let branch = self.check_expr(arm.body)?;

                    if !branch.satisfies(&result) {
                        let l1 = "arms have incompatible types";
                        let msg = format!("{l1}\nexpected {result}, found {branch}");
                        return Err(self.tcx.dcx().span_err(arm.body.span, msg));
                    }
                }

                None => break,
            }
        }

        Ok(result)
    }

    pub fn check_expr_binary(&mut self, lhs: &Expr, rhs: &Expr) -> Result<Ty> {
        self.check_expr(lhs)?;
        self.check_expr(rhs)?;

        Ok(self.introduce())
    }

    pub fn check_expr_block(&mut self, block: &Block) -> Result<Ty> {
        for stmt in block.stmts {
            self.check_stmt(stmt)?;
        }

        match block.expr {
            Some(expr) => {
                // This expression occurs at the very end without a semicolon
                self.check_expr(expr)
            }

            None if block.stmts.len() > 0 => self.check_stmt(&block.stmts[block.stmts.len() - 1]),

            _ => Ok(self.introduce()),
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
                            let mut ty = self.introduce();
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

        Ok(self.universal())
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
