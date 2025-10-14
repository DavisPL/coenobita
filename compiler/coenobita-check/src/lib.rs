#![feature(rustc_private)]

mod cx;

use coenobita_middle::ty::{PassError, Type, TypeKind};
use coenobita_parse::PassTarget;
use rustc_abi::{VariantIdx, FIRST_VARIANT};

use std::collections::{BTreeSet, HashMap, VecDeque};
use std::fmt::Display;

use std::cmp::Eq;
use std::hash::Hash;

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

use rustc_hir::def::{CtorOf, DefKind, Res};
use rustc_hir::{
    def_id::DefId, Arm, AttrArgs, AttrKind, Attribute, Block, BodyId, Closure, Expr, ExprField, ExprKind,
    FnSig, HirId, Impl, ImplItem, ImplItemKind, Item, ItemKind, LangItem, LetExpr, LetStmt, MatchSource,
    PatField, PatKind, QPath, Stmt, StmtKind,
};
use rustc_middle::ty::{self, FieldDef, GenericArg, TypeckResults, TypingEnv};

use log::{debug, warn};

pub type Result<T = ()> = std::result::Result<T, ErrorGuaranteed>;
type Bound = (String, Set);

use coenobita_parse::{create_parser, create_psess, parse::Parse, Pass, Take};

use crate::cx::InfCtx;

struct Ctx<K, V> {
    scopes: Vec<HashMap<K, V>>,
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
        self.scopes.iter().rev().find_map(|cx| cx.get(k))
    }

    pub fn set(&mut self, k: K, v: V) {
        match self.scopes.last_mut() {
            Some(cx) => cx.insert(k, v),
            None => None,
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

    items: HashMap<DefId, Type>,

    vctx: Ctx<HirId, Type>,

    icx: InfCtx,

    crate_name: String,
    // items: HashMap<DefId, Type>,
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

            items: HashMap::new(),

            vctx: Ctx::new(),

            icx: InfCtx::new(crate_name.clone()),

            crate_name,
        }
    }

    fn check_item_fn(&mut self, _: &FnSig, body_id: BodyId) -> Result {
        let def_id = body_id.hir_id.owner.to_def_id();
        let local_def_id = def_id.as_local().unwrap();

        // Enter a new scope in the set context
        self.scx.enter();

        // Collect all set variables introduced by this function
        for attr in self.tcx.get_attrs_by_path(def_id, &self.take_attr) {
            let Take { right, bound, left } = self.parse_attr_take(attr)?;

            // Make sure this variable hasn't been introduced yet
            if let Some(bound) = self.scx.get(&left.0) {
                return Err(self.tcx.dcx().span_err(
                    left.1,
                    format!("variable already introduced with upper bound {bound}"),
                ));
            }

            // Make sure all variables in the bound exist
            self.ensure_variables_exist(&bound, &right)?;

            // Add upper bound to set variable context
            self.scx.set(left.0, bound);
        }

        let mut params = Vec::new();

        let body = self.tcx.hir_body(body_id);

        for param in body.params {
            let raw = self.tcx.typeck(local_def_id).node_type(param.hir_id);
            let ty = self.get_constr_for_ty(&raw);

            params.push(ty.clone());

            self.process_pattern(param.pat.kind, ty, param.pat.hir_id);
        }

        let mut rty = self.top_type();

        // Collect all set applications
        for attr in self.tcx.get_attrs_by_path(def_id, &self.pass_attr) {
            let pass = self.parse_pass(attr)?;

            match pass.left.0 {
                PassTarget::Argument(index) => self.pass(&mut params[index], pass)?,
                PassTarget::Return => self.pass(&mut rty, pass)?,
                PassTarget::Infer => warn!("Set pass with inferred target has no effect on function items")
            }
        }

        self.check_expr(&body.value, Some(&rty), false)?;

        // Exit this scope
        self.scx.exit();

        Ok(())
    }

    // ======== TYPES ======== //

    /// Given the `DefId` of a function, return its type.
    fn fn_ty(&mut self, def_id: DefId) -> Type {
        todo!()
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
            TyKind::Adt(adt_def, substs) => {
                // Structs, enums, unions (Vec, HashMap, your types, etc.)
                let type_def_id = adt_def.did();
                println!("ADT DefId: {:?}", type_def_id);

                return self
                    .get_type_constr(type_def_id)
                    .unwrap_or(&Type::opaque())
                    .clone();
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

    fn pass(&self, ty: &mut Type, pass: Pass) -> Result {
        let Pass { left, set, right } = pass;
        let span = left.1;

        self.ensure_variables_exist(&set, &right)?;

        if let Err(e) = ty.pass(&self.scx, set.clone()) {
            match e {
                PassError::BoundMismatch(bound) => {
                    return Err(self
                        .tcx
                        .dcx()
                        .span_err(span, format!("Provided set {set} is not a subset of {bound}")))
                },

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
            let mut ty = Type::opaque();

            for attr in self.tcx.get_attrs_by_path(field.did, &self.pass_attr) {
                let pass = self.parse_pass(attr)?;
                self.pass(&mut ty, pass)?;
            }

            fields.insert(field.name.to_string(), ty);
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
                match self.tcx.type_of(alias_to).skip_binder().kind() {
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
    fn check_expr_call(&mut self, fun: &Expr, args: &[Expr], _span: Span) -> Result<Type> {
        let owner_id = fun.hir_id.owner;
        let owner_def_kind = self.tcx.def_kind(owner_id);

        if owner_def_kind.is_fn_like() {
            let local_def_id = owner_id.to_def_id().as_local().unwrap();

            if let ExprKind::Path(qpath) = fun.kind {
                let typeck_results = self.tcx.typeck(local_def_id);

                if let Res::Def(def_kind, def_id) = typeck_results.qpath_res(&qpath, fun.hir_id) {
                    // TODO: Check intrinsic constraints
                    // self.check_intrinsic_constraints(def_id, def_kind, args, typeck_results)?;
                }
            }
        }

        let fty = self.check_expr(fun, None, false)?;

        let mut ty = match fty.clone().kind {
            TypeKind::Fn(arg_tys, ret_ty) => {
                if args.len() != arg_tys.len() {
                    warn!("arg ct doesnt match up");
                }

                for (ty, expr) in arg_tys.into_iter().zip(args) {
                    self.check_expr(expr, Some(&ty), false)?;
                }

                *ret_ty
            }

            TypeKind::Rec(elements) => {
                for (i, arg) in args.iter().enumerate() {
                    let ty = elements[&i.to_string()].clone();
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
                        for (ty, expr) in arg_tys.into_iter().zip(args) {
                            self.check_expr(expr, Some(&ty), false)?;
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
                for field in fields {
                    let ty = tys[&field.ident.to_string()].clone();
                    self.check_expr(field.expr, Some(&ty), false)?;
                }
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

        result.kind = TypeKind::Array(Box::new(item));

        Ok(result)
    }

    /// Checks the type of a tuple expression.
    fn check_expr_tup(&mut self, exprs: &[Expr]) -> Result<Type> {
        let mut result = self.bottom_type();

        let mut items = vec![];

        for expr in exprs {
            items.push(self.check_expr(expr, None, false)?);
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
        todo!()
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
                    Ok(field_map[&field.to_string()].clone())
                }
            }

            TypeKind::Tuple(fields) => {
                let index = field.name.as_str().parse::<usize>().unwrap();

                if index >= fields.len() {
                    let msg = format!("unknown field '{}'", field.name);
                    self.tcx.dcx().span_err(field.span, msg);
                    Ok(self.top_type())
                } else {
                    Ok(fields[index].clone())
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
        let mut processed = false;

        // TODO: We need to use a type constructor that matches the left type
        let mut ty = self.top_type();

        for attr in self.tcx.hir().attrs(local.hir_id) {
            if attr.path_matches(&self.pass_attr) {
                let Pass { left, set, right } = self.parse_pass(attr)?;

                self.ensure_variables_exist(&set, &right)?;

                if let Err(e) = ty.pass(&self.scx, set.clone()) {
                    match e {
                        PassError::BoundMismatch(bound) => {
                            return Err(self
                                .tcx
                                .dcx()
                                .span_err(left.1, format!("set {set} is not a subset of {bound}")))
                        },

                        PassError::Unexpected => {
                            return Err(self
                                .tcx
                                .dcx()
                                .span_err(left.1, format!("unexpected pass")))
                        }
                    }
                }
            }
        }

        if let Some(expr) = local.init {
            self.check_expr(expr, Some(&ty), false)?;
            self.process_pattern(local.pat.kind, ty, local.pat.hir_id);
        } else {
            self.process_pattern(local.pat.kind, self.top_type(), local.pat.hir_id);
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

    fn parse_pass(&self, attr: &Attribute) -> Result<Pass> {
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
            PatKind::Binding(_, hir_id, ident, _) => {
                // TODO: Figure out the HirId <-> String mapping
                self.str_to_hir.insert(ident.to_string(), hir_id);
                self.hir_to_ty.insert(hir_id, ty.clone());

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
        let mut child = child.clone();

        child.intrinsic[2] = child.intrinsic[2].clone().union(parent.intrinsic[2].clone());

        child
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
