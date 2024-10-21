use coenobita_ast::ast::TyKind as ATyKind;
use coenobita_middle::flow::FlowPair;
use coenobita_middle::map::Map;
use coenobita_middle::ty::{Ty, TyKind};

use coenobita_log::debug;

use coenobita_parse::CoenobitaParser;
use rustc_ast::ptr::P;
use rustc_ast::tokenstream::TokenStream;
use rustc_ast::{AttrKind, NormalAttr};
use rustc_driver::DEFAULT_LOCALE_RESOURCES;
use rustc_errors::fallback_fluent_bundle;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::def_id::DefId;
use rustc_hir::{
    Arm, Block, BodyId, Expr, ExprKind, FnSig, HirId, Item, ItemKind, LetStmt, QPath, Stmt,
    StmtKind,
};
use rustc_middle::ty::TyCtxt;
use rustc_parse::parser::Parser;
use rustc_session::parse::ParseSess;
use rustc_span::{ErrorGuaranteed, Symbol};

use crate::context::Context;
use crate::utilities::emitter;

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
            attr: vec![Symbol::intern("cnbt"), Symbol::intern("tag")],
            tcx,
            def_map: Map::new(),
            hir_map: Map::new(),
        }
    }

    pub fn function_ty(&self, def_id: DefId) -> Ty {
        debug(format!("Trying to get type of function {:#?}", def_id));

        match self.def_map.get(&def_id) {
            Some(ty) => ty.clone(),
            None => todo!(),
        }
    }

    pub fn local_ty(&self, hir_id: HirId) -> Result<Ty> {
        match self.hir_map.get(&hir_id) {
            Some(ty) => Ok(ty.clone()),
            None => todo!(),
        }
    }

    pub fn check_item(&mut self, context: &mut Context, item: &Item) -> Result {
        debug(format!("Checking item {:?}", item.kind));
        match item.kind {
            ItemKind::Fn(signature, _, body_id) => self.check_item_fn(context, &signature, body_id),
            _ => Ok(()),
        }
    }

    pub fn check_item_fn(
        &mut self,
        context: &mut Context,
        signature: &FnSig,
        body_id: BodyId,
    ) -> Result {
        let def_id = body_id.hir_id.owner.to_def_id();
        let expected = signature.decl.inputs.len();
        let default = context.influence(Ty::ty_fn(expected));

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
                                self.def_map.insert(def_id, context.influence(ty.into()));
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

        let expr = self.tcx.hir().body(body_id).value;
        let actual = self.check_expr(context, expr)?;

        let TyKind::Fn(_, expected) = self.function_ty(def_id).kind else {
            panic!()
        };

        if !actual.satisfies(&expected) {
            let msg = format!("Expected {expected}, found {actual}");
            self.tcx.dcx().span_err(expr.span, msg);
        }

        Ok(())
    }

    pub fn check_let_stmt(&mut self, context: &mut Context, local: &LetStmt) {
        for attr in self.tcx.hir().attrs(local.hir_id) {
            if attr.path_matches(&self.attr) {
                // SAFETY - The `coenobita::tag` is guaranteed to be a normal attribute
                let normal: &P<NormalAttr> = unsafe { std::mem::transmute(attr) };

                let psess = create_psess(&self.tcx);
                let mut parser = create_parser(&psess, normal.item.args.inner_tokens());

                if let Ok(ty) = parser.parse_ty() {
                    let expected: Ty = ty.into();
                    self.hir_map.insert(local.pat.hir_id, expected.clone());

                    if let Some(expr) = local.init {
                        if let Ok(actual) = self.check_expr(context, expr) {
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

    pub fn check_expr(&mut self, context: &mut Context, expr: &Expr) -> Result<Ty> {
        match expr.kind {
            ExprKind::Lit(_) => Ok(context.default()),
            ExprKind::Path(qpath) => self.check_expr_path(context, expr.hir_id, &qpath),
            ExprKind::Call(func, args) => self.check_expr_call(context, func, args),
            ExprKind::If(guard, then_expr, else_expr) => {
                self.check_expr_if(context, guard, then_expr, else_expr)
            }
            ExprKind::Match(guard, arms, _) => self.check_expr_match(context, guard, arms),
            ExprKind::Binary(_, lhs, rhs) => self.check_expr_binary(context, lhs, rhs),
            ExprKind::Block(block, _) => self.check_expr_block(context, block),
            _ => {
                debug(format!("Skipping expr of kind {:#?}", expr.kind));
                todo!()
            }
        }
    }

    pub fn check_expr_path(
        &self,
        context: &mut Context,
        hir_id: HirId,
        qpath: &QPath,
    ) -> Result<Ty> {
        let local_def_id = hir_id.owner.to_def_id().as_local().unwrap();

        match self.tcx.typeck(local_def_id).qpath_res(qpath, hir_id) {
            Res::Local(hir_id) => self.local_ty(hir_id),
            Res::Def(def_kind, def_id) => match def_kind {
                DefKind::Fn => Ok(self.function_ty(def_id)),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    pub fn check_expr_call(
        &mut self,
        context: &mut Context,
        func: &Expr,
        args: &[Expr],
    ) -> Result<Ty> {
        let fty = self.check_expr(context, func)?;

        match fty.kind() {
            TyKind::Fn(arg_tys, ret_ty) => {
                if args.len() != arg_tys.len() {
                    todo!()
                }

                for (expected, expr) in arg_tys.iter().zip(args) {
                    let actual = self.check_expr(context, expr)?;

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
        context: &mut Context,
        guard: &Expr,
        then_expr: &Expr,
        else_expr: Option<&Expr>,
    ) -> Result<Ty> {
        let ity = self.check_expr(context, guard)?;
        context.enter(&ity);

        let tty = self.check_expr(context, then_expr)?;
        let mut result = context.default().merge(tty);

        if let Some(else_expr) = else_expr {
            result = result.merge(self.check_expr(context, else_expr)?);
        }

        context.exit();
        Ok(result)
    }

    pub fn check_expr_match(
        &mut self,
        context: &mut Context,
        guard: &Expr,
        arms: &[Arm],
    ) -> Result<Ty> {
        let ity = self.check_expr(context, guard)?;
        context.enter(&ity);

        let mut result = context.default();

        for arm in arms {
            result = result.merge(self.check_expr(context, arm.body)?)
        }

        context.exit();
        Ok(result)
    }

    pub fn check_expr_binary(
        &mut self,
        context: &mut Context,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Result<Ty> {
        let explicit = self
            .check_expr(context, lhs)?
            .merge(self.check_expr(context, rhs)?);

        Ok(context.influence(explicit))
    }

    pub fn check_expr_block(&mut self, context: &mut Context, block: &Block) -> Result<Ty> {
        for stmt in block.stmts {
            self.check_stmt(context, stmt)?;
        }

        match block.expr {
            Some(expr) => {
                // This expression occurs at the very end without a semicolon
                self.check_expr(context, expr)
            }

            None => self.check_stmt(context, &block.stmts[block.stmts.len() - 1]),
        }
    }

    pub fn check_stmt(&mut self, context: &mut Context, stmt: &Stmt) -> Result<Ty> {
        match stmt.kind {
            StmtKind::Expr(expr) => {
                self.check_expr(context, expr);
            }
            StmtKind::Semi(expr) => {
                self.check_expr(context, expr);
            }
            StmtKind::Let(local) => self.check_let_stmt(context, local),
            StmtKind::Item(item_id) => {
                self.check_item(context, self.tcx.hir().item(item_id));
            }
        };

        Ok(context.influence(Ty::default()))
    }
}

fn create_psess(tcx: &TyCtxt) -> ParseSess {
    let opts = tcx.sess.opts.clone();
    let source_map = tcx.sess.psess.clone_source_map();
    let fallback_bundle = fallback_fluent_bundle(DEFAULT_LOCALE_RESOURCES.to_vec(), false);
    let emitter = emitter(opts, source_map.clone(), fallback_bundle);
    let dcx = rustc_errors::DiagCtxt::new(emitter);

    ParseSess::with_dcx(dcx, source_map)
}

fn create_parser<'cnbt>(psess: &'cnbt ParseSess, stream: TokenStream) -> CoenobitaParser<'cnbt> {
    // Set up the rustc parser and the custom CnbtParser
    let rustc_parser = Parser::new(&psess, stream, None);
    let parser = CoenobitaParser::new(rustc_parser);

    parser
}
