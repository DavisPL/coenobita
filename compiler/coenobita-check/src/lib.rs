#![feature(rustc_private)]

use std::collections::{HashMap, VecDeque};

use rustc_hir::{def_id::DefId, AttrArgs, AttrKind, Attribute, BodyId, FnSig, HirId, Item, ItemKind, PatField, PatKind, QPath};
use rustc_middle::ty::{Ty, TyCtxt, TyKind};
use rustc_span::{ErrorGuaranteed, Span, Symbol};

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

use log::{debug, warn};

pub type Result<T = ()> = std::result::Result<T, ErrorGuaranteed>;
type Bound = (String, Set);

use coenobita_parse::{create_parser, create_psess, parse::Parse, Pass, Take};

#[derive(Clone, Debug)]
struct Type {
    pub binder: VecDeque<(String, Set)>,
    pub intrinsic: [Set; 3],
    pub intrinsic_idx: usize
}

impl Type {
    /// Replace variable `var` with `val` in every type this one contains, recursively, as long as `val` satisfies the bound on the outermost binder
    /// Return error if variable passed when binder is empty
    fn pass(&mut self, scx: &SetCtx, val: Set) -> std::result::Result<(), PassError> {
        match self.binder.pop_front() {
            Some((var, bound)) => {
                if !val.subset(scx, &bound) {
                    return Err(PassError::BoundMismatch(bound.clone()));
                }

                // TODO: Recursively replace set variables with the value we've been given

                Ok(())
            },

            None => {
                if self.intrinsic_idx == 3 {
                    return Err(PassError::Unexpected)
                }

                self.intrinsic[self.intrinsic_idx] = val;
                self.intrinsic_idx += 1;
                Ok(())
            }
        }
    }
}

pub struct Checker<'tcx> {
	tcx: TyCtxt<'tcx>,
	take_attr: Vec<Symbol>,
    pass_attr: Vec<Symbol>,

    scx: SetCtx,

    binders: HashMap<DefId, Vec<Bound>>,

    str_to_hir: HashMap<String, HirId>,
    hir_to_ty: HashMap<HirId, Type>

    // ty_constructors: HashMap<Ty<'tcx>, >
}

#[derive(Debug)]
enum PassError {
    /// The provided set is not a subset of the known upper bound.
    BoundMismatch(Set),

    /// No set was expected.
    Unexpected
}

impl<'tcx> Checker<'tcx> {
	pub fn new(crate_name: String, tcx: TyCtxt<'tcx>) -> Self {
		Checker {
            tcx,
            take_attr: vec![Symbol::intern("coenobita"), Symbol::intern("take")],
            pass_attr: vec![Symbol::intern("coenobita"), Symbol::intern("pass")],
            scx: SetCtx::new(),
            binders: HashMap::new(),

            str_to_hir: HashMap::new(),
            hir_to_ty: HashMap::new()
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

            let type_constr = self.get_type_constr(&param_ty);

            debug!("- {:#?} : {param_ty}", param.pat.simple_ident());

            self.process_pattern(param.pat.kind, type_constr);
        }

        self.display_str_to_hir();
        self.display_hir_to_ty();
        
        let mut ret_ty = Type {binder: VecDeque::from([]), intrinsic: [Set::Universe, Set::Universe, Set::Universe], intrinsic_idx: 0 };

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

    fn get_type_constr(&mut self, ty: &Ty<'tcx>) -> Type {
        // TODO: Actually get the constructor
        match ty.kind() {
            TyKind::Adt(adt_def, substs) => {
                // Structs, enums, unions (Vec, HashMap, your types, etc.)
                let type_def_id = adt_def.did();
                println!("ADT DefId: {:?}", type_def_id);
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

        Type {
            binder: VecDeque::from([]),
            intrinsic: [Set::Universe, Set::Universe, Set::Universe],
            intrinsic_idx: 0
        }
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
            _ => Ok(())
        }
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

    fn process_pattern(&mut self, pat_kind: PatKind, ty: Type) {
        match pat_kind {
            PatKind::Binding(_, hir_id, ident, _) => {
                self.str_to_hir.insert(ident.to_string(), hir_id);
                self.hir_to_ty.insert(hir_id, ty.clone());
            }

            _ => {}
        };
    }
}