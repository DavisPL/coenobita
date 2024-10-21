use std::collections::HashSet;

use coenobita_middle::{
    flow::{FlowPair, FlowSet},
    ty::Ty,
};

pub struct Context<'cnbt> {
    crate_name: &'cnbt str,
    levels: Vec<HashSet<String>>,
}

impl<'cnbt> Context<'cnbt> {
    pub fn new(crate_name: &'cnbt str) -> Self {
        Context {
            crate_name,
            levels: vec![HashSet::from([crate_name.to_owned()])],
        }
    }

    pub fn default(&self) -> Ty {
        self.influence(Ty::default().with_explicit(self.origin()))
    }

    pub fn influence(&self, ty: Ty) -> Ty {
        let mut influencers = self.levels[0].clone();

        for influencer in self.levels.iter().skip(1) {
            influencers.extend(influencer.clone());
        }

        let implicit = FlowSet::Specific(influencers);
        let fpair = FlowPair::new(ty.fpair.explicit().clone(), implicit);

        Ty::new(fpair, ty.kind)
    }

    pub fn origin(&self) -> FlowSet {
        FlowSet::Specific(HashSet::from([self.crate_name.to_owned()]))
    }

    pub fn enter(&mut self, ty: &Ty) {
        self.levels.push(HashSet::from_iter(ty.origins()));
    }

    pub fn exit(&mut self) {
        if self.levels.len() > 1 {
            self.levels.pop();
        }
    }
}
