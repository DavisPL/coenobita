use rustc_driver::{Callbacks, Compilation};
use rustc_hir::{intravisit::Visitor, Item};
use rustc_interface::{interface::Compiler, Queries};
use rustc_middle::{hir::nested_filter::OnlyBodies, ty::TyCtxt};

use coenobita_typeck::{checker::Checker, context::Context};

pub struct CoenobitaCallbacks<'a> {
    crate_name: &'a str,
}

impl<'a> CoenobitaCallbacks<'a> {
    pub fn new(crate_name: &'a str) -> Self {
        CoenobitaCallbacks {
            crate_name: crate_name,
        }
    }
}

impl Callbacks for CoenobitaCallbacks<'_> {
    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        queries.global_ctxt().unwrap().enter(|tcx| {
            let hir_map = tcx.hir();
            let mut visitor = CoenobitaVisitor::new(self.crate_name.to_owned(), tcx);

            hir_map.visit_all_item_likes_in_crate(&mut visitor);
        });

        Compilation::Continue
    }
}

struct CoenobitaVisitor<'tcx> {
    crate_name: String,
    tcx: TyCtxt<'tcx>,
    checker: Checker<'tcx>,
}

impl<'tcx> CoenobitaVisitor<'tcx> {
    pub fn new(crate_name: String, tcx: TyCtxt<'tcx>) -> Self {
        CoenobitaVisitor {
            crate_name: crate_name.clone(),
            tcx,
            checker: Checker::new(crate_name, tcx),
        }
    }
}

impl<'tcx> Visitor<'tcx> for CoenobitaVisitor<'tcx> {
    type NestedFilter = OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_item(&mut self, item: &'tcx Item<'tcx>) -> Self::Result {
        let mut context = Context::new(self.crate_name.clone());
        self.checker.check_item(&mut context, item);
    }
}
