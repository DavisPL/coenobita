use coenobita_check::{Check, Checker};
use coenobita_middle::{flow::FlowPair, provenance::ProvenancePair};
use rustc_driver::{Callbacks, Compilation};
use rustc_hir::{intravisit::Visitor, Item};
use rustc_interface::{interface::Compiler, Queries};
use rustc_middle::{hir::nested_filter::OnlyBodies, ty::TyCtxt};

pub struct CoenobitaCallbacks {
    crate_name: String,
    crate_type: String,
}

impl CoenobitaCallbacks {
    pub fn new(crate_name: String, crate_type: String) -> Self {
        CoenobitaCallbacks {
            crate_name,
            crate_type,
        }
    }
}

impl Callbacks for CoenobitaCallbacks {
    fn after_analysis<'tcx>(&mut self, _compiler: &Compiler, queries: &'tcx Queries<'tcx>) -> Compilation {
        queries.global_ctxt().unwrap().enter(|tcx| {
            let is_proc_macro = tcx
                .sess
                .opts
                .crate_types
                .contains(&rustc_session::config::CrateType::ProcMacro);

            if is_proc_macro {
                // It is impossible to make some proc macro crates capability safe, so skip them
                return;
            }

            if self.crate_name == "trybuild"
                || self.crate_name == "syn"
                || self.crate_name == "quote"
                || self.crate_name == "proc_macro2"
            {
                // Skip trybuild for now
                return;
            }

            let hir_map = tcx.hir();
            let mut visitor = CoenobitaVisitor::new(&self.crate_name, &self.crate_type, tcx);

            hir_map.visit_all_item_likes_in_crate(&mut visitor);
        });

        Compilation::Continue
    }
}

struct CoenobitaVisitor<'cnbt, 'tcx> {
    crate_name: &'cnbt str,
    tcx: TyCtxt<'tcx>,
    ichecker: Checker<'tcx, FlowPair>,
    pchecker: Checker<'tcx, ProvenancePair>,
}

impl<'c, 'tcx> CoenobitaVisitor<'c, 'tcx> {
    pub fn new(crate_name: &'c str, crate_type: &'c str, tcx: TyCtxt<'tcx>) -> Self {
        CoenobitaVisitor {
            crate_name,
            tcx,
            ichecker: Checker::new(crate_name.to_owned(), tcx),
            pchecker: Checker::new(crate_name.to_string(), tcx),
        }
    }
}

impl<'c, 'tcx> Visitor<'tcx> for CoenobitaVisitor<'c, 'tcx> {
    type NestedFilter = OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_item(&mut self, item: &'tcx Item<'tcx>) -> Self::Result {
        // Check integrity
        let _ = self.ichecker.check_item(item);

        // Check provenance
        let _ = self.pchecker.check_item(item);
    }
}
