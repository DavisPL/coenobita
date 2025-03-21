use coenobita_check::{Check, Checker};
use coenobita_middle::{flow::FlowPair, provenance::ProvenancePair};
use rustc_driver::{Callbacks, Compilation};
use rustc_hir::{intravisit::Visitor, Item};
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

const SKIP: [&str; 4] = ["trybuild", "syn", "quote", "proc_macro2"];

impl Callbacks for CoenobitaCallbacks {
    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &rustc_interface::interface::Compiler,
        tcx: TyCtxt<'tcx>,
    ) -> Compilation {
        let is_proc_macro = tcx
            .sess
            .opts
            .crate_types
            .contains(&rustc_session::config::CrateType::ProcMacro);

        if !is_proc_macro && !SKIP.contains(&self.crate_name.as_str()) {
            let mut visitor = CoenobitaVisitor::new(&self.crate_name, &self.crate_type, tcx);
            tcx.hir_visit_all_item_likes_in_crate(&mut visitor);
        }

        Compilation::Continue
    }
}

#[allow(unused)]
struct CoenobitaVisitor<'cnbt, 'tcx> {
    crate_name: &'cnbt str,
    tcx: TyCtxt<'tcx>,
    ichecker: Checker<'tcx, FlowPair>,
    pchecker: Checker<'tcx, ProvenancePair>,
}

impl<'c, 'tcx> CoenobitaVisitor<'c, 'tcx> {
    #[allow(unused)]
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

    fn maybe_tcx(&mut self) -> Self::MaybeTyCtxt {
        self.tcx
    }

    fn visit_item(&mut self, item: &'tcx Item<'tcx>) -> Self::Result {
        // Check integrity
        // let _ = self.ichecker.check_item(item);

        // Check provenance
        let _ = self.pchecker.check_item(item);
    }
}
