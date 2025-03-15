use std::{collections::HashSet, fmt::Display, path::Path};

use rustc_span::Symbol;
use serde::{Deserialize, Serialize};

use crate::{origin::OriginSet, property::Property};

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub struct ProvenancePair {
    authors: OriginSet,
    suppliers: OriginSet,
}

impl ProvenancePair {
    pub fn new(authors: OriginSet, suppliers: OriginSet) -> Self {
        ProvenancePair { authors, suppliers }
    }

    pub fn first(&self) -> &OriginSet {
        &self.authors
    }

    pub fn last(&self) -> &OriginSet {
        &self.suppliers
    }
}

impl Display for ProvenancePair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({},{})", self.first(), self.last())
    }
}

impl Property for ProvenancePair {
    fn satisfies(&self, other: &Self) -> bool {
        let first = match (self.first(), other.first()) {
            (_, OriginSet::Universal) => true,
            (OriginSet::Specific(o1), OriginSet::Specific(o2)) => o1 == o2,
            _ => false,
        };

        let last = match (self.last(), other.last()) {
            (_, OriginSet::Universal) => true,
            (OriginSet::Specific(o1), OriginSet::Specific(o2)) => o1 == o2,
            _ => false,
        };

        first && last
    }

    fn influence(&self, other: Self) -> Self {
        other
    }

    fn merge(&self, other: Self) -> Self {
        Self {
            authors: self.first().union(other.first()),
            suppliers: self.last().union(other.last()),
        }
    }

    fn bottom(origin: String) -> Self {
        let mut set = HashSet::new();
        set.insert(origin);

        Self {
            authors: OriginSet::Specific(set.clone()),
            suppliers: OriginSet::Specific(set),
        }
    }

    fn top() -> Self {
        Self {
            authors: OriginSet::Universal,
            suppliers: OriginSet::Universal,
        }
    }

    fn attr() -> Vec<Symbol> {
        vec![Symbol::intern("cnbt"), Symbol::intern("observation")]
    }

    fn intrinsics_path() -> std::path::PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .join("intrinsics")
            .join("provenance")
    }
}
