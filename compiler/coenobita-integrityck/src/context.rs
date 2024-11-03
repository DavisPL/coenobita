use std::collections::HashSet;

use coenobita_log::debug;
use coenobita_middle::flow::{FlowPair, FlowSet};
use coenobita_middle::ty::{Ty as _Ty, TyKind};

type Ty = _Ty<FlowPair>;

pub struct Context<'cnbt> {
    crate_name: &'cnbt str,
    crate_type: &'cnbt str,
    levels: Vec<FlowSet>,
}

impl<'cnbt> Context<'cnbt> {
    pub fn new(crate_name: &'cnbt str, crate_type: &'cnbt str) -> Self {
        Context {
            crate_name,
            crate_type,
            levels: vec![],
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
            .fold(self.origin(), |acc, elem| acc.union(elem));

        let ty = Ty::new(FlowPair(explicit, implicit), TyKind::Infer);
        debug(format!("introducing new ty {:#?}", ty));
        ty
    }

    /// Adds implicit contributors to the given type. This is only necessary when "creating"
    /// new values that aren't introduced (like the results of binary expressions).
    pub fn influence(&self, ty: Ty) -> Ty {
        if self.levels.is_empty() {
            ty
        } else {
            let implicit = self
                .levels
                .iter()
                .fold(self.levels[0].clone(), |acc, elem| acc.union(elem));

            let fpair = FlowPair(
                ty.property.explicit().clone(),
                ty.property.implicit().clone().union(&implicit),
            );

            Ty::new(fpair, ty.kind)
        }
    }

    pub fn origin(&self) -> FlowSet {
        let name = if self.crate_type == "bin" {
            "bin".to_owned()
        } else {
            self.crate_name.to_owned()
        };

        FlowSet::Specific(HashSet::from([name]))
    }

    pub fn enter(&mut self, ty: &Ty) {
        let fset = ty.property.explicit().clone().union(ty.property.implicit());
        self.levels.push(fset);
    }

    pub fn exit(&mut self) {
        self.levels.pop();
    }
}
