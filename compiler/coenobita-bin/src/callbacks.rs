use rustc_driver::{Callbacks, Compilation};
use rustc_hir::{intravisit::Visitor, Item};
use rustc_interface::{interface::Compiler, Queries};
use rustc_middle::{hir::nested_filter::OnlyBodies, ty::TyCtxt};

use coenobita_integrityck::{checker::Checker as IChecker, context::Context};
use coenobita_provck::checker::Checker as PChecker;

pub struct CoenobitaCallbacks {
    crate_name: String,
}

impl CoenobitaCallbacks {
    pub fn new(crate_name: String) -> Self {
        CoenobitaCallbacks {
            crate_name: crate_name,
        }
    }
}

impl Callbacks for CoenobitaCallbacks {
    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        queries.global_ctxt().unwrap().enter(|tcx| {
            let hir_map = tcx.hir();
            let mut visitor = CoenobitaVisitor::new(&self.crate_name, tcx);

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
    pub fn new(crate_name: &'c str, tcx: TyCtxt<'tcx>) -> Self {
        let context = Context::new(crate_name);

        CoenobitaVisitor {
            crate_name,
            tcx,
            ichecker: IChecker::new(crate_name, tcx, context),
            pchecker: PChecker::new(crate_name, tcx),
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
        // let _ = self.pchecker.check_item(item);
    }
}
