use rustc_driver::{Callbacks, Compilation};
use rustc_hir::{intravisit::Visitor, Item};
use rustc_interface::{interface::Compiler, Queries};
use rustc_middle::{hir::nested_filter::OnlyBodies, ty::TyCtxt};

use coenobita_integrityck::{checker::Checker as IChecker, context::Context as IContext};
use coenobita_provck::{checker::Checker as PChecker, context::Context as PContext};

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
    ichecker: IChecker<'cnbt, 'tcx>,
    pchecker: PChecker<'cnbt, 'tcx>,
}

impl<'c, 'tcx> CoenobitaVisitor<'c, 'tcx> {
    pub fn new(crate_name: &'c str, crate_type: &'c str, tcx: TyCtxt<'tcx>) -> Self {
        let icontext = IContext::new(crate_name, crate_type);
        let pcontext = PContext::new(crate_name, crate_type);

        CoenobitaVisitor {
            crate_name,
            tcx,
            ichecker: IChecker::new(tcx, icontext),
            pchecker: PChecker::new(tcx, pcontext),
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
