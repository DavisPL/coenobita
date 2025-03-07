use std::collections::HashSet;
use std::fmt;

use itertools::Itertools;

use crate::property::Property;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct FlowPair {
    pub(crate) explicit: FlowSet,
    pub(crate) implicit: FlowSet,
}

impl FlowPair {
    pub fn new(explicit: FlowSet, implicit: FlowSet) -> Self {
        FlowPair { explicit, implicit }
    }

    pub fn explicit(&self) -> &FlowSet {
        &self.explicit
    }

    pub fn implicit(&self) -> &FlowSet {
        &self.implicit
    }
}

impl fmt::Display for FlowPair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.explicit(), self.implicit())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum FlowSet {
    Specific(HashSet<String>),
    Universal,
}

impl FlowSet {
    pub fn union(&self, other: &FlowSet) -> FlowSet {
        match self {
            FlowSet::Universal => FlowSet::Universal,
            FlowSet::Specific(s1) => match other {
                FlowSet::Universal => FlowSet::Universal,
                FlowSet::Specific(s2) => FlowSet::Specific(s1.union(&s2).cloned().collect()),
            },
        }
    }

    pub fn is_subset(&self, other: &FlowSet) -> bool {
        match self {
            FlowSet::Universal => match other {
                FlowSet::Universal => true,
                _ => false,
            },
            FlowSet::Specific(s1) => match other {
                FlowSet::Specific(s2) => s1.is_subset(&s2),
                _ => true,
            },
        }
    }
}

impl Default for FlowSet {
    fn default() -> Self {
        Self::Universal
    }
}

impl fmt::Display for FlowSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Specific(origins) => {
                let origins = origins.iter().map(|ident| ident.to_string()).sorted().join(",");
                write!(f, "{{{origins}}}")
            }

            Self::Universal => {
                write!(f, "{{*}}")
            }
        }
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

    fn bottom() -> Self {
        unimplemented!()
    }

    fn top() -> Self {
        unimplemented!()
    }
}
