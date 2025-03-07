use itertools::Itertools;
use std::{collections::HashSet, fmt};

#[derive(Debug, Clone, PartialEq)]
pub enum OriginSet {
    Specific(HashSet<String>),
    Universal,
}

impl OriginSet {
    pub fn union(&self, other: &Self) -> Self {
        match self {
            Self::Universal => Self::Universal,
            Self::Specific(s1) => match other {
                Self::Universal => Self::Universal,
                Self::Specific(s2) => Self::Specific(s1.union(&s2).cloned().collect()),
            },
        }
    }

    pub fn is_subset(&self, other: &OriginSet) -> bool {
        match self {
            OriginSet::Universal => match other {
                OriginSet::Universal => true,
                _ => false,
            },
            OriginSet::Specific(s1) => match other {
                OriginSet::Specific(s2) => s1.is_subset(&s2),
                _ => true,
            },
        }
    }
}

impl Default for OriginSet {
    fn default() -> Self {
        Self::Universal
    }
}

impl fmt::Display for OriginSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Specific(origins) => {
                let origins = origins.iter().sorted().join(",");
                write!(f, "{{{origins}}}")
            }

            Self::Universal => write!(f, "{{*}}"),
        }
    }
}
