use std::{collections::HashSet, fmt::Display};

use itertools::Itertools;

use crate::property::Property;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ProvenancePair(pub Provenance, pub Provenance);

impl ProvenancePair {
    pub fn first(&self) -> &Provenance {
        &self.0
    }

    pub fn last(&self) -> &Provenance {
        &self.1
    }
}

impl Display for ProvenancePair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({},{})", self.first(), self.last())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Provenance {
    Specific(HashSet<String>),
    Universal,
}

impl Default for Provenance {
    fn default() -> Self {
        Self::Universal
    }
}

impl Display for Provenance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Specific(origins) => {
                let origins = origins.iter().sorted().join(",");
                write!(f, "{origins}")
            }

            Self::Universal => write!(f, "*"),
        }
    }
}

impl Property for ProvenancePair {
    fn satisfies(&self, other: &Self) -> bool {
        let first = match (self.first(), other.first()) {
            (_, Provenance::Universal) => true,
            (Provenance::Specific(o1), Provenance::Specific(o2)) => o1 == o2,
            _ => false,
        };

        let last = match (self.last(), other.last()) {
            (_, Provenance::Universal) => true,
            (Provenance::Specific(o1), Provenance::Specific(o2)) => o1 == o2,
            _ => false,
        };

        first && last
    }

    fn merge(&self, other: Self) -> Self {
        todo!()
    }
}
