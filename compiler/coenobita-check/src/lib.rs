#![feature(rustc_private)]

extern crate rustc_ast;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

use std::collections::HashMap;

use coenobita_middle::ty::{Ty, TyKind};
use rustc_hir::{def_id::DefId, HirId, PatField, PatKind, QPath};
use rustc_middle::ty::{FieldDef, TyCtxt};
use rustc_span::{symbol::Ident, ErrorGuaranteed, Symbol};
use rustc_target::abi::VariantIdx;

use log::warn;

/// Describes the behaviors that a context must have. You can supplement your static analysis
/// with custom context behavior by initializing a `Checker` with something implementing `Context`.
pub trait Context<P> {
    /// Push a type onto the context stack, influencing all results until `pop` is called.
    fn push(&mut self, ty: &Ty<P>);

    /// Pop the last type off the context stack unless it's the last one remaining.
    fn pop(&mut self);

    /// Influence a type using the current context.
    fn influence(&self, property: Ty<P>) -> Ty<P>;

    /// Return the ⊥ type in this context.
    fn bottom(&self) -> Ty<P>;

    /// Return the ⊤ type in this context.
    fn top(&self) -> Ty<P>;

    /// Return an iterator over shared type references.
    fn stack<'a>(&'a self) -> impl Iterator<Item = &'a Ty<P>>
    where
        P: 'a;
}

// /// A checking structure generic in (1) the property being checked and
// /// (2) the context used to influence properties during checking.
// pub struct Checker<'tcx, T, C: Context<T>> {
//     /// Contains the attribute we are targeting as a sequence of symbols.
//     attr: Vec<Symbol>,

//     /// Contains type checking results for the entire crate.
//     tcx: TyCtxt<'tcx>,

//     /// Maps item `DefId`s to their properties.
//     items: HashMap<DefId, T>,

//     /// Maps local `HirId`s to their properties.
//     locals: HashMap<HirId, T>,

//     /// Can be used to influence properties during checking.
//     context: C,
// }

#[derive(Debug, Clone)]
pub enum Expectation<P> {
    /// We don't know what type this expression should have.
    NoExpectation,

    /// This expression should satisfy the given property.
    ExpectHasType(Ty<P>),
}

pub type Result<T = ()> = std::result::Result<T, ErrorGuaranteed>;

pub trait Check<'tcx, P: Clone, C: Context<P>> {
    /// Return a mutable reference to the global typing context.
    fn tcx(&mut self) -> &mut TyCtxt<'tcx>;

    // ======== GETTERS ======== //

    /// Return a shared reference to the item map.
    fn get_items(&self) -> &HashMap<DefId, Ty<P>>;

    /// Return a shared reference to the local map.
    fn get_locals(&self) -> &HashMap<HirId, Ty<P>>;

    /// Return a mutable reference to the local map.
    fn get_locals_mut(&mut self) -> &mut HashMap<HirId, Ty<P>>;

    /// Return a mutable reference to the context.
    fn get_context_mut(&mut self) -> &mut C;

    // ======== PROPERTIES ======== //

    /// Given the `DefId` of a function, return its property.
    fn fn_property(&mut self, def_id: DefId) -> Ty<P> {
        if let Some(ty) = self.get_items().get(&def_id) {
            return ty.clone();
        }

        // We haven't processed the definition of this function yet
        let _ = self.check_item_fn_nonlocal(def_id);
        self.get_items().get(&def_id).unwrap().clone()
    }

    /// Given the parent and variant `DefId`s of an ADT (as well as its index), return its property.
    fn adt_property(&mut self, def_id_parent: DefId, def_id_variant: DefId, index: VariantIdx) -> Ty<P> {
        if let Some(ty) = self.get_items().get(&def_id_variant) {
            return ty.clone();
        }

        // We haven't processed the definition of this function yet
        let field_defs = &self
            .tcx()
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
    fn local_property(&mut self, hir_id: HirId) -> Ty<P> {
        match self.get_locals().get(&hir_id) {
            Some(ty) => ty.clone(),
            None => {
                warn!("Silently failed to get type of local variable");
                self.get_context_mut().bottom()
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
                        let ty = self.get_context_mut().influence(map[&field.ident.name].clone());
                        self.process_pattern(field.pat.kind, ty, field.pat.hir_id);
                    }
                }

                TyKind::Opaque | TyKind::Infer => {
                    for field in fields {
                        let ty = self.get_context_mut().top();
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
        self.get_context_mut().push(&ty);

        match pat_kind {
            PatKind::Binding(_, hir_id, _, _) => {
                self.get_locals_mut().insert(hir_id, ty.clone());
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
                    let top = self.get_context_mut().top();

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
            PatKind::Range(_, _, _) | PatKind::Path(_) | PatKind::Wild | PatKind::Never => {}
        };

        self.get_context_mut().pop();
    }

    // ======== NONLOCAL ITEMS ======== //
    fn check_item_fn_nonlocal(&mut self, def_id: DefId) -> Result;

    // ======== LOCAL ITEMS ======== //
    fn check_item_struct(&mut self, def_id: DefId, field_defs: &[FieldDef]) -> Result;

    // ======== EXPRESSIONS ======== //
    fn check_expr_path(
        &mut self,
        hir_id: HirId,
        qpath: &QPath,
        expectation: &Expectation<P>,
    ) -> Result<Ty<P>>;
}
