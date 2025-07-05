use std::collections::HashSet;
use std::fmt;
use std::path::Path;

use rustc_span::Symbol;
use serde::{Deserialize, Serialize};

use crate::origin::OriginSet;
use crate::property::Property;

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct FlowPair {
    pub(crate) explicit: OriginSet,
    pub(crate) implicit: OriginSet,
}

impl FlowPair {
    pub fn new(explicit: OriginSet, implicit: OriginSet) -> Self {
        FlowPair { explicit, implicit }
    }

    pub fn explicit(&self) -> &OriginSet {
        &self.explicit
    }

    pub fn implicit(&self) -> &OriginSet {
        &self.implicit
    }
}

impl fmt::Display for FlowPair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.explicit(), self.implicit())
    }
}

impl Property for FlowPair {
    fn satisfies(&self, other: &Self) -> bool {
        let explicit = self.explicit().is_subset(other.explicit());
        let implicit = self.implicit().is_subset(other.implicit());

        explicit && implicit
    }

    fn merge(&self, other: Self) -> Self {
        let explicit = self.explicit().union(other.explicit());
        let implicit = self.implicit().union(other.implicit());

        Self { explicit, implicit }
    }

    fn influence(&self, other: Self) -> Self {
        let implicit = self.implicit().union(other.implicit());

        Self {
            explicit: other.explicit,
            implicit,
        }
    }

    fn bottom(origin: String) -> Self {
        let mut set = HashSet::new();
        set.insert(origin);

        Self {
            explicit: OriginSet::Specific(set.clone()),
            implicit: OriginSet::Specific(set),
        }
    }

    fn top() -> Self {
        Self {
            explicit: OriginSet::Universal,
            implicit: OriginSet::Universal,
        }
    }

    fn attr() -> Vec<Symbol> {
        vec![Symbol::intern("cnbt"), Symbol::intern("integrity")]
    }

    fn intrinsics_path() -> std::path::PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .join("intrinsics")
            .join("annotations")
            .join("integrity")
            .with_extension("json")
    }
}
