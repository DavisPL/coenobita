use std::{collections::HashMap, fmt::Display};

use crate::flow::FlowPair;
use crate::origin::OriginSet;
use crate::property::Property;
use itertools::Itertools;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Ty<P: Property> {
    pub property: P,

    pub kind: TyKind<P>,
}

impl<'a, P: Property> Ty<P> {
    pub fn new(property: P, kind: TyKind<P>) -> Self {
        Ty { property, kind }
    }

    pub fn kind(&self) -> TyKind<P> {
        self.kind.clone()
    }

    pub fn ty_fn(n: usize) -> Self {
        let property = P::default();
        let default_ty = Ty::new(property.clone(), TyKind::Infer);

        let args = vec![default_ty.clone(); n];
        let kind = TyKind::Fn(None, args, Box::new(default_ty));

        Ty { property, kind }
    }

    pub fn satisfies(&self, other: &Ty<P>) -> bool {
        let s = self.property.satisfies(&other.property);

        s && match (self.kind(), other.kind()) {
            // TODO: `Abs` should really be `Infer`
            (_, TyKind::Opaque) => true,
            (_, TyKind::Infer) => true,
            (TyKind::Adt(f1), TyKind::Adt(f2)) => {
                if f1.len() != f2.len() {
                    false
                } else {
                    for (field, ty) in f1 {
                        if !f2.contains_key(&field) || !ty.satisfies(&f2[&field]) {
                            return false;
                        }
                    }

                    true
                }
            }
            (TyKind::Array(t1), TyKind::Array(t2)) => t1.satisfies(&t2),
            (TyKind::Fn(_, as1, r1), TyKind::Fn(_, as2, r2)) => {
                // TODO: Subtyping should be contravariant for argument types!
                if as1.len() != as2.len() {
                    return false;
                } else {
                    for (a1, a2) in as1.iter().zip(as2) {
                        if !a1.satisfies(&a2) {
                            return false;
                        }
                    }
                }

                r1.satisfies(&r2)
            }
            (TyKind::Tuple(items1), TyKind::Tuple(items2)) => {
                for (i1, i2) in items1.iter().zip(items2) {
                    if !i1.satisfies(&i2) {
                        return false;
                    }
                }

                true
            }
            _ => false,
        }
    }

    pub fn merge(&self, other: Self) -> Self {
        let property = self.property.merge(other.property);
        Self {
            property,
            kind: self.kind(),
        }
    }

    pub fn influence(&self, mut other: Self) -> Self {
        other.property = self.property.influence(other.property);
        other
    }
}

impl Ty<FlowPair> {
    pub fn with_explicit(mut self, explicit: OriginSet) -> Self {
        self.property.explicit = explicit;
        self
    }

    pub fn ty_adt(n: usize) -> Ty<FlowPair> {
        let default_flow_pair = FlowPair::new(OriginSet::Universal, OriginSet::Universal);
        let default_ty = Ty::new(default_flow_pair.clone(), TyKind::Infer);

        let args = vec![default_ty.clone(); n]; // Create `n` copies of `default_ty`
        let mut map = HashMap::new();

        for (i, arg) in (0..n).zip(args) {
            map.insert(i.to_string(), arg);
        }

        Ty {
            property: default_flow_pair,
            kind: TyKind::Adt(map),
        }
    }
}

impl<P: Property> Display for Ty<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.property, self.kind)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TyKind<P: Property> {
    Opaque,
    Fn(Option<Box<Ty<P>>>, Vec<Ty<P>>, Box<Ty<P>>),
    Tuple(Vec<Ty<P>>),
    Array(Box<Ty<P>>),
    Adt(HashMap<String, Ty<P>>),
    Infer,
}

impl<P: Property> Display for TyKind<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fn(tup_ty, arg_tys, ret_ty) => {
                let args = arg_tys.iter().map(|ty| ty.to_string()).sorted().join(",");
                let tup_ty = tup_ty.clone().map_or(String::from(""), |ty| ty.to_string());

                write!(f, " fn {tup_ty}({args}) -> {}", ret_ty)
            }

            Self::Tuple(item_tys) => {
                let items = item_tys
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, " ({})", items)
            }

            Self::Array(item_ty) => write!(f, " [{}]", item_ty),

            Self::Adt(field_tys) => {
                let fields = field_tys
                    .iter()
                    .sorted_by_key(|&(key, _)| key)
                    .map(|(key, value)| format!("{key}:{value}"))
                    .collect::<Vec<_>>()
                    .join(",");

                write!(f, " struct {{{fields}}}")
            }

            Self::Opaque => write!(f, ""),
            Self::Infer => write!(f, ""),
        }
    }
}
