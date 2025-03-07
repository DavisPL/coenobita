use std::collections::HashSet;
use std::fmt;

use itertools::Itertools;
use rustc_span::Symbol;

use log::debug;

use crate::origin::OriginSet;
use crate::property::Property;

#[derive(Clone, Debug, Default, PartialEq)]
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

// #[derive(Clone, Debug, PartialEq)]
// pub enum OriginSet {
//     Specific(HashSet<String>),
//     Universal,
// }

// impl OriginSet {
//     pub fn union(&self, other: &OriginSet) -> OriginSet {
//         match self {
//             OriginSet::Universal => OriginSet::Universal,
//             OriginSet::Specific(s1) => match other {
//                 OriginSet::Universal => OriginSet::Universal,
//                 OriginSet::Specific(s2) => OriginSet::Specific(s1.union(&s2).cloned().collect()),
//             },
//         }
//     }

//     pub fn is_subset(&self, other: &OriginSet) -> bool {
//         match self {
//             OriginSet::Universal => match other {
//                 OriginSet::Universal => true,
//                 _ => false,
//             },
//             OriginSet::Specific(s1) => match other {
//                 OriginSet::Specific(s2) => s1.is_subset(&s2),
//                 _ => true,
//             },
//         }
//     }
// }

// impl Default for OriginSet {
//     fn default() -> Self {
//         Self::Universal
//     }
// }

// impl fmt::Display for OriginSet {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Self::Specific(origins) => {
//                 let origins = origins.iter().map(|ident| ident.to_string()).sorted().join(",");
//                 write!(f, "{{{origins}}}")
//             }

//             Self::Universal => {
//                 write!(f, "{{*}}")
//             }
//         }
//     }
// }

impl Property for FlowPair {
    fn satisfies(&self, other: &Self) -> bool {
        let explicit = self.explicit().is_subset(other.explicit());
        let implicit = self.implicit().is_subset(other.implicit());

        debug!(
            "is {} a subset of {}? {}",
            self.explicit(),
            other.explicit(),
            explicit
        );
        debug!(
            "is {} a subset of {}? {}",
            self.implicit(),
            other.implicit(),
            implicit
        );

        explicit && implicit
    }

    fn merge(&self, other: Self) -> Self {
        let explicit = self.explicit().union(other.explicit());
        let implicit = self.implicit().union(other.implicit());

        Self { explicit, implicit }
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
        vec![Symbol::intern("cnbt"), Symbol::intern("flow")]
    }
}
