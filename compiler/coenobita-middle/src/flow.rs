use std::{collections::HashSet, fmt::Display};

use coenobita_ast::flow;
use itertools::Itertools;

#[derive(Clone, Debug, PartialEq)]
pub struct FlowPair(pub FlowSet, pub FlowSet);

impl FlowPair {
    pub fn new(explicit: FlowSet, implicit: FlowSet) -> Self {
        FlowPair(explicit, implicit)
    }

    pub fn origins(&self) -> Vec<String> {
        todo!()
    }

    pub fn explicit(&self) -> &FlowSet {
        &self.0
    }

    pub fn implicit(&self) -> &FlowSet {
        &self.1
    }
}

impl From<flow::FlowPair> for FlowPair {
    fn from(value: flow::FlowPair) -> Self {
        FlowPair(value.explicit.into(), value.implicit.into())
    }
}

impl Display for FlowPair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.explicit(), self.implicit())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum FlowSet {
    Universal,
    Specific(HashSet<String>),
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

impl From<flow::FlowSet> for FlowSet {
    fn from(value: flow::FlowSet) -> Self {
        match value {
            flow::FlowSet::Universal(_) => Self::Universal,
            flow::FlowSet::Specific(origins, _) => {
                Self::Specific(origins.iter().map(|ident| ident.to_string()).collect())
            }
        }
    }
}

impl Display for FlowSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Specific(origins) => {
                let origins = origins
                    .iter()
                    .map(|ident| ident.to_string())
                    .sorted()
                    .join(",");
                write!(f, "{{{origins}}}")
            }

            Self::Universal => {
                write!(f, "{{*}}")
            }
        }
    }
}
