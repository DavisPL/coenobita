use std::collections::HashSet;

use coenobita_middle::{
    flow::{FlowPair, FlowSet},
    ty::{Ty, TyKind},
};

pub struct Context<'cnbt> {
    crate_name: &'cnbt str,
    levels: Vec<FlowSet>,
}

impl<'cnbt> Context<'cnbt> {
    pub fn new(crate_name: &'cnbt str) -> Self {
        Context {
            crate_name,
            levels: vec![FlowSet::Specific(HashSet::from([crate_name.to_owned()]))],
        }
    }

    pub fn universal(&self) -> Ty {
        Ty::new(
            FlowPair(FlowSet::Universal, FlowSet::Universal),
            TyKind::Infer,
        )
    }

    pub fn introduce(&self) -> Ty {
        let explicit = self.origin();
        let implicit = self
            .levels
            .iter()
            .fold(self.levels[0].clone(), |acc, elem| acc.union(elem));

        Ty::new(FlowPair(explicit, implicit), TyKind::Infer)
    }

    pub fn influence(&self, ty: Ty) -> Ty {
        let implicit = self
            .levels
            .iter()
            .fold(self.levels[0].clone(), |acc, elem| acc.union(elem));

        let fpair = FlowPair(
            ty.fpair.explicit().clone(),
            ty.fpair.implicit().clone().union(&implicit),
        );

        Ty::new(fpair, ty.kind)
    }

    pub fn origin(&self) -> FlowSet {
        FlowSet::Specific(HashSet::from([self.crate_name.to_owned()]))
    }

    pub fn enter(&mut self, ty: &Ty) {
        let fset = ty.fpair.explicit().clone().union(ty.fpair.implicit());
        self.levels.push(fset);
    }

    pub fn exit(&mut self) {
        if self.levels.len() > 1 {
            self.levels.pop();
        }
    }
}
