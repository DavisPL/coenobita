use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::fmt::Debug;

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub enum Set {
    /// A set variable (`A`, `B`, `C`).
    Variable(String),

    /// A concrete origin set (`{a,b,c}`).
    Concrete(Option<HashSet<String>>),

    /// A union of sets (`A ∪ {a,b,c}`).
    Union(SetUnion),
}

impl Debug for Set {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Set::Variable(value) => write!(f, "{:?}", value),
            Set::Union(value) => write!(f, "{:?}", value),
            Set::Concrete(value) => {
                if let Some(origins) = value {
                    write!(f, "{{{}}}", origins.iter().sorted().join(","))
                } else {
                    write!(f, "{{*}}")
                }
            }
        }
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct SetUnion {
    /// The variables in this union.
    pub variables: HashSet<String>,

    /// The union of all the concrete sets in this union.
    pub concrete: HashSet<String>
}

impl SetUnion {
    pub fn new(sets: Vec<&Set>) -> Self {
        todo!()
    }
}

impl Debug for SetUnion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let variables = self.variables.iter().sorted().join(" U ");
        let concrete = self.concrete.iter().sorted().join(",");
        write!(f, "{} U {{{}}}", variables, concrete)
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct SetBind {
    /// Set variable being bound.
    left: String,

    /// The upper bound of the set variable.
    right: Set,
}

impl Debug for SetBind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ⊆ {:?}", self.left, self.right)
    }
}
