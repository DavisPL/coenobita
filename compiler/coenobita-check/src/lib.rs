#![feature(rustc_private)]

use std::collections::HashMap;

use rustc_hir::Item;
use rustc_middle::ty::TyCtxt;
use rustc_span::{ErrorGuaranteed, Symbol};

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

enum AttrKind {
	Fn
}

pub struct Checker<'tcx> {
	tcx: TyCtxt<'tcx>,
	attr: Vec<Symbol>
}

impl<'tcx> Checker<'tcx> {
	pub fn new(crate_name: String, tcx: TyCtxt<'tcx>) -> Self {
		Checker { tcx, attr: vec![Symbol::intern("cnbt")] }
	}

	pub fn check_item(&mut self, item: &Item) -> Result {
		let def_id = item.owner_id.to_def_id();

		debug!("Getting attributes for item {:?}", def_id);

		for attr in self.tcx.get_attrs_by_path(def_id, &self.attr) {
			debug!("{:#?}", attr);
		}

		Ok(())
	}
}