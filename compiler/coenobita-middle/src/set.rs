use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::fmt::{Debug, Display};
use std::hash::Hash;

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Set {
    /// A set variable (`A`, `B`, `C`).
    Variable(String),

    /// A concrete origin set (`{a,b,c}`).
    Concrete(Option<HashSet<String>>),

    /// A union of sets (`A ∪ {a,b,c}`).
    Union(SetUnion),
}

// impl Set {
//     pub fn contains(&self, other: &Set) -> bool {

//     }
// }

impl Hash for Set {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

impl Display for Set {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Set::Variable(value) => write!(f, "{}", value),
            Set::Union(value) => write!(f, "{}", value),
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

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct SetUnion {
    /// The variables in this union.
    pub variables: HashSet<String>,

    /// The union of all the concrete sets in this union.
    pub concrete: Option<HashSet<String>>,
}

impl SetUnion {
    pub fn new(sets: Vec<&Set>) -> Self {
        let mut variables = HashSet::new();
        let mut concrete = HashSet::new();
        let mut top = false;

        for set in sets {
            match set {
                Set::Variable(value) => {
                    variables.insert(value.clone());
                }
                Set::Concrete(value) => match value {
                    Some(origins) => concrete.extend(origins.iter().cloned()),
                    None => {
                        top = true;
                        break;
                    }
                },
                Set::Union(un) => {
                    variables.extend(un.variables.iter().cloned());

                    match &un.concrete {
                        None => {
                            top = true;
                            break;
                        }
                        Some(origins) => concrete.extend(origins.iter().cloned()),
                    }
                }
            }
        }

        SetUnion {
            variables,
            concrete: if top { None } else { Some(concrete) },
        }
    }
}

impl Display for SetUnion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let concrete = match &self.concrete {
            Some(origins) => origins.iter().sorted().join(","),
            None => "*".to_string(),
        };

        if self.variables.len() > 0 {
            let variables = self.variables.iter().sorted().join(" U ");
            write!(f, "{} U {{{}}}", variables, concrete)
        } else {
            write!(f, "{{{}}}", concrete)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct SetBind {
    /// Set variable being bound.
    pub left: String,

    /// The upper bound of the set variable.
    pub right: Set,
}

impl SetBind {
    pub fn new(left: String, right: Set) -> Self {
        Self { left, right }
    }
}

impl Display for SetBind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ⊆ {}", self.left, self.right)
    }
}
