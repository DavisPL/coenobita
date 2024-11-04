use std::collections::HashSet;

use coenobita_middle::provenance::{Provenance, ProvenancePair};
use coenobita_middle::ty::TyKind;

use crate::shared::Ty;

pub struct Context<'cnbt> {
    crate_name: &'cnbt str,
    crate_type: &'cnbt str,
    levels: Vec<Provenance>,
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
            ProvenancePair(Provenance::Universal, Provenance::Universal),
            TyKind::Infer,
        )
    }

    pub fn introduce(&self) -> Ty {
        Ty::new(ProvenancePair(self.origin(), self.origin()), TyKind::Infer)
    }

    pub fn influence(&self, ty: Ty) -> Ty {
        let mut ty = ty;
        ty.property.1 = self.origin();
        ty
    }

    pub fn origin(&self) -> Provenance {
        let name = if self.crate_type == "bin" {
            "bin".to_owned()
        } else {
            self.crate_name.to_owned()
        };

        Provenance::Specific(name)
    }
}
