use itertools::Itertools;
use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
};

use crate::set::{Set, SetCtx};

#[derive(Debug)]
pub enum PassError {
    /// The provided set is not a subset of the known upper bound.
    BoundMismatch(Set),

    /// No set was expected.
    Unexpected,
}

#[derive(Clone, Debug)]
pub enum TypeKind {
    Opaque,
    Fn(Vec<Type>, Box<Type>),
    Rec(HashMap<String, Type>),
    Array(Box<Type>),
    Tuple(Vec<Type>),
}

#[derive(Clone, Debug)]
pub struct SetVar {
    pub ident: String,
    pub value: Option<Set>,
    pub bound: Set,
}

impl SetVar {
    pub fn new(ident: String, bound: Set) -> Self {
        Self {
            ident,
            value: None,
            bound,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub binder: Vec<SetVar>,
    pub var_to_idx: HashMap<String, usize>,
    pub binder_idx: usize,
    pub intrinsic: [Set; 3],
    pub intrinsic_idx: usize,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeKind::Rec(fields) => {
                write!(
                    f,
                    "{{ {} }}",
                    fields.iter().map(|(k, v)| format!("{k} : {v}")).join(", ")
                )
            }

            TypeKind::Fn(params, rty) => {
                let params = params.iter().join(", ");

                if self.binder.len() > 0 {
                    let set_vars = self
                        .binder
                        .iter()
                        .map(|sv| format!("{} ⊆ {}", sv.ident, sv.bound))
                        .join(", ");

                    write!(f, "fn[{set_vars}]({params}) → {rty}")
                } else {
                    write!(f, "fn({params}) → {rty}")
                }
            }

            // TODO: Implement proper printing for every kind
            _ => write!(
                f,
                "{} {} {}",
                self.intrinsic[0], self.intrinsic[1], self.intrinsic[2]
            ),
        }
    }
}

impl TypeKind {
    pub fn replace(&mut self, var: &str, set: &Set) {
        match self {
            TypeKind::Array(ty) => {
                ty.replace(var, set);
            }

            TypeKind::Fn(params, rty) => {
                for param in params {
                    param.replace(var, set);
                }

                rty.replace(var, set);
            }

            TypeKind::Rec(fields) => {
                for ty in fields.values_mut() {
                    ty.replace(var, set);
                }
            }

            TypeKind::Tuple(elements) => {
                for e in elements {
                    e.replace(var, set);
                }
            }

            _ => {}
        }
    }
}

impl Type {
    pub fn opaque() -> Self {
        Self {
            kind: TypeKind::Opaque,
            binder: vec![],
            binder_idx: 0,
            var_to_idx: HashMap::new(),
            intrinsic: [Set::Universe, Set::Universe, Set::Universe],
            intrinsic_idx: 0,
        }
    }

    pub fn replace(&mut self, var: &str, set: &Set) {
        for i in 0..3 {
            self.intrinsic[i] = self.intrinsic[i].replace(var, set)
        }
    }

    pub fn fun(n: usize) -> Self {
        let args = (0..n).map(|_| Type::opaque()).collect();

        Self {
            kind: TypeKind::Fn(args, Box::new(Type::opaque())),
            binder: vec![],
            binder_idx: 0,
            var_to_idx: HashMap::new(),
            intrinsic: [Set::Universe, Set::Universe, Set::Universe],
            intrinsic_idx: 0,
        }
    }

    pub fn record(fields: HashMap<String, Type>) -> Self {
        Self {
            kind: TypeKind::Rec(fields),
            binder: vec![],
            binder_idx: 0,
            var_to_idx: HashMap::new(),
            intrinsic: [Set::Universe, Set::Universe, Set::Universe],
            intrinsic_idx: 0,
        }
    }

    pub fn satisfies(&self, scx: &SetCtx, other: &Type) -> bool {
        let kind = match (&self.kind, &other.kind) {
            (TypeKind::Opaque, TypeKind::Opaque) => true,
            (TypeKind::Fn(ps1, r1), TypeKind::Fn(ps2, r2)) => {
                for (p1, p2) in ps1.iter().zip(ps2.iter()) {
                    if !p1.satisfies(scx, p2) {
                        return false;
                    }
                }

                r2.satisfies(scx, &r1)
            }
            _ => false,
        };

        if !kind {
            return false;
        }

        for (s1, s2) in self.intrinsic.iter().zip(other.intrinsic.iter()) {
            if !s1.subset(scx, s2) {
                return false;
            }
        }

        true
    }

    /// Replace variable `var` with `val` in every type this one contains, recursively, as long as `val` satisfies the bound on the outermost binder
    /// Return error if variable passed when binder is empty
    pub fn pass(&mut self, scx: &SetCtx, val: Set) -> std::result::Result<(), PassError> {
        if self.binder_idx < self.binder.len() {
            let slot = self.binder[self.binder_idx].clone();

            if !val.subset(scx, &slot.bound) {
                return Err(PassError::BoundMismatch(slot.bound.clone()));
            }

            self.binder[self.binder_idx].value = Some(val.clone());
            self.binder_idx += 1;

            self.kind.replace(&slot.ident, &val);

            return Ok(());
        }

        if self.intrinsic_idx == 3 {
            return Err(PassError::Unexpected);
        }

        self.intrinsic[self.intrinsic_idx] = val;
        self.intrinsic_idx += 1;
        Ok(())
    }
}
