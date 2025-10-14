use std::collections::BTreeSet;

use coenobita_middle::set::{Set, SetCtx};
use coenobita_middle::ty::Type;
use log::debug;

pub struct InfCtx {
    scopes: Vec<Set>,
}

impl InfCtx {
    pub fn new(origin: String) -> Self {
        let set = Set::Concrete(BTreeSet::from([origin]));

        InfCtx { scopes: vec![set] }
    }

    pub fn enter(&mut self, set: &Set) {
        self.scopes.push(set.clone());
    }

    pub fn exit(&mut self) {
        self.scopes.pop();
    }

    pub fn influence(&self, mut ty: Type) -> Type {
        for set in &self.scopes {
            ty.intrinsic[2] = ty.intrinsic[2].clone().union(set.clone());
        }

        ty
    }
}
