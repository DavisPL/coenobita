use itertools::Itertools;
use std::{collections::{BTreeSet, HashMap}, fmt::Display};

#[macro_export]
macro_rules! set {
    // Empty set
    () => {
        ::std::collections::BTreeSet::new()
    };

    // Set with elements
    ($($x:expr),+ $(,)?) => {
        {
            let mut temp_set = ::std::collections::BTreeSet::new();
            $(
                temp_set.insert($x.into());
            )+
            temp_set
        }
    };
}

struct SetCtx {
	bindings: HashMap<String, Set>
}

impl SetCtx {
    fn new() -> Self {
        SetCtx {
			bindings: HashMap::new()
		}
    }

    fn bound(&self, var: &str) -> Option<&Set> {
        self.bindings.get(var)
    }

	fn bind(&mut self, var: String, set: Set) {
		self.bindings.insert(var, set);
	}
}

#[derive(Clone, Ord, Eq, PartialEq, Hash, PartialOrd)]
enum Set {
    Variable(String),
    Concrete(BTreeSet<String>),
    Union(BTreeSet<Set>),
}

impl Set {
    fn subset(&self, ctx: &SetCtx, other: &Set) -> bool {
        match (self, other) {
            (Set::Concrete(s1), Set::Concrete(s2)) => s1.is_subset(s2),

            // We can never determine whether a concrete set is a subset of a variable because variables do not have lower bounds
            (Set::Concrete(_), Set::Variable(_)) => false,

            // We can only prove that a concrete set is a subset of a union if it's a subset of the union's concrete component
            (Set::Concrete(e1), Set::Union(s2)) => {
				let mut e2 = BTreeSet::new();

				for set in s2 {
					if let Set::Concrete(elements) = set {
						e2.extend(elements.clone());
					}
				}

				e1.is_subset(&e2)
			}

            // If we are comparing a variable to itself, we can apply REFL to conclude that the subset relation holds
            (Set::Variable(v1), Set::Variable(v2)) if v1 == v2 => true,

			// If set variable `self` is an explicit member of the union, then it is definitely a subset
			(Set::Variable(_), Set::Union(s1)) if s1.contains(self) => true,

            // In all other cases, we try to determine whether the variable's upper bound is a subset of 'other'
            (Set::Variable(v), w2) => match ctx.bound(v) {
                Some(w1) => w1.subset(ctx, w2),
                None => false,
            },

            // <UNION> ⊆ <SET>
            (Set::Union(s1), _) => {
                // Every component must be a subset of the right side
                for set in s1 {
                    if !set.subset(ctx, &other) {
                        return false;
                    }
                }

                true
            }
        }
    }

    fn union(self, other: Set) -> Set {
        match (self, other) {
            // {a,b} ∪ {b,c}
            (Set::Concrete(s1), Set::Concrete(s2)) => {
                Set::Concrete(s1.union(&s2).cloned().collect())
            }

            // <UNION> ∪ {a,b,c} | {a,b,c} ∪ <UNION>
            (Set::Union(s1), Set::Concrete(mut e1)) | (Set::Concrete(mut e1), Set::Union(s1)) => {
                let mut s2 = BTreeSet::new();

                for set in s1 {
                    if let Set::Concrete(e2) = set {
                        e1.extend(e2.into_iter());
                    } else {
                        s2.insert(set);
                    }
                }

                Set::Union(s2)
            }

            // <UNION> ∪ X | X ∪ <UNION>
            (Set::Union(mut s), Set::Variable(v)) | (Set::Variable(v), Set::Union(mut s)) => {
                s.insert(Set::Variable(v));
                Set::Union(s)
            }

            // <UNION> ∪ <UNION>
            (Set::Union(s1), Set::Union(s2)) => {
                // We know for sure that each set contains one or more set variables, and possibly one concrete set

                let mut concrete = BTreeSet::new();
                let mut s3 = BTreeSet::new();

                for set in s1 {
                    if let Set::Concrete(elements) = set {
                        concrete.extend(elements.into_iter());
                    } else {
                        s3.insert(set);
                    }
                }

                for set in s2 {
                    if let Set::Concrete(elements) = set {
                        concrete.extend(elements.into_iter());
                    } else {
                        s3.insert(set);
                    }
                }

                if !concrete.is_empty() {
                    s3.insert(Set::Concrete(concrete));
                }

                Set::Union(s3)
            }

            // {a,b,c} ∪ X
            (Set::Concrete(e), Set::Variable(v)) => {
                Set::Union(set![Set::Concrete(e), Set::Variable(v)])
            }

            // X ∪ {a,b,c}
            (Set::Variable(v), Set::Concrete(e)) => {
                Set::Union(set![Set::Variable(v), Set::Concrete(e)])
            }

            // X ∪ Y
            (Set::Variable(v1), Set::Variable(v2)) => {
                Set::Union(set![Set::Variable(v1), Set::Variable(v2)])
            }
        }
    }
}

impl Display for Set {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Set::Concrete(e) => {
                if e.is_empty() {
                    write!(f, "∅")
                } else {
                    write!(f, "{{{}}}", e.iter().sorted().join(","))
                }
            }

            Set::Union(s) => {
                let sets = s.iter().map(|set| format!("{set}")).join(" ∪ ");
                write!(f, "{sets}")
            }

            Set::Variable(v) => write!(f, "{v}"),
        }
    }
}