use std::{collections::HashMap, fmt::Display};

use crate::flow::{FlowPair, FlowSet};
use crate::provenance::{Provenance, ProvenancePair};
use coenobita_ast::ast::{self, Ty as ATy, TyKind as ATyKind};
use coenobita_ast::flow::FlowPair as AFlowPair;
use coenobita_ast::provenance::ProvenancePair as AProvenancePair;
use itertools::Itertools;
use rustc_span::Symbol;

#[derive(Debug, Clone, PartialEq)]
pub struct Ty<T> {
    pub property: T,

    pub kind: TyKind<T>,
}

impl<T: Clone> Ty<T> {
    pub fn new(property: T, kind: TyKind<T>) -> Self {
        Ty { property, kind }
    }

    pub fn kind(&self) -> TyKind<T> {
        self.kind.clone()
    }
}

impl Ty<FlowPair> {
    pub fn with_explicit(mut self, explicit: FlowSet) -> Self {
        self.property.0 = explicit;
        self
    }

    pub fn merge(self, other: Ty<FlowPair>) -> Ty<FlowPair> {
        let explicit = self.property.explicit().union(other.property.explicit());
        let implicit = self.property.implicit().union(other.property.implicit());

        // We assume both types have the same shape
        Ty::new(FlowPair(explicit, implicit), self.kind().clone())
    }

    pub fn ty_fn(n: usize) -> Ty<FlowPair> {
        let default_flow_pair = FlowPair::new(FlowSet::Universal, FlowSet::Universal);
        let default_ty = Ty::new(default_flow_pair.clone(), TyKind::Infer);

        let args = vec![default_ty.clone(); n]; // Create `n` copies of `default_ty`

        Ty {
            property: default_flow_pair,
            kind: TyKind::Fn(args, Box::new(default_ty)),
        }
    }

    pub fn ty_adt(n: usize) -> Ty<FlowPair> {
        let default_flow_pair = FlowPair::new(FlowSet::Universal, FlowSet::Universal);
        let default_ty = Ty::new(default_flow_pair.clone(), TyKind::Infer);

        let args = vec![default_ty.clone(); n]; // Create `n` copies of `default_ty`
        let mut map = HashMap::new();

        for (i, arg) in (0..n).zip(args) {
            map.insert(Symbol::intern(&i.to_string()), arg);
        }

        Ty {
            property: default_flow_pair,
            kind: TyKind::Adt(map),
        }
    }

    pub fn satisfies(&self, other: &Ty<FlowPair>) -> bool {
        let explicit = self
            .property
            .explicit()
            .is_subset(other.property.explicit());
        let implicit = self
            .property
            .implicit()
            .is_subset(other.property.implicit());

        explicit
            && implicit
            && match (self.kind(), other.kind()) {
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
                (TyKind::Fn(as1, r1), TyKind::Fn(as2, r2)) => {
                    // Subtyping is contravariant for argument types
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

    pub fn origins(&self) -> Vec<String> {
        self.property.origins()
    }
}

impl Ty<ProvenancePair> {
    pub fn ty_fn(n: usize) -> Ty<ProvenancePair> {
        let defalt_provenance_pair = ProvenancePair(Provenance::Universal, Provenance::Universal);
        let default_ty = Ty::new(defalt_provenance_pair.clone(), TyKind::Infer);

        let args = vec![default_ty.clone(); n]; // Create `n` copies of `default_ty`

        Ty {
            property: defalt_provenance_pair,
            kind: TyKind::Fn(args, Box::new(default_ty)),
        }
    }

    pub fn satisfies(&self, other: &Ty<ProvenancePair>) -> bool {
        // TODO: Take type shape (kind) into account
        let first = match (self.property.first(), other.property.first()) {
            (_, Provenance::Universal) => true,
            (Provenance::Specific(o1), Provenance::Specific(o2)) => o1 == o2,
            _ => false,
        };

        let last = match (self.property.last(), other.property.last()) {
            (_, Provenance::Universal) => true,
            (Provenance::Specific(o1), Provenance::Specific(o2)) => o1 == o2,
            _ => false,
        };

        // first && last && match (self.kind(), other.kind()) {
        //     (TyKind::Abs, TyKind::Abs) => true,
        //     (TyKind::Adt(f1), TyKind::Adt(f2)) => {
        //         f1 == f2
        //     }
        //     (TyKind::Arr(t1), TyKind::Arr(t2))
        // }

        first && last
    }
}

impl<T: Display> Display for Ty<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.property, self.kind)
    }
}

impl From<ATy<AFlowPair>> for Ty<FlowPair> {
    fn from(value: ATy<AFlowPair>) -> Self {
        Ty {
            property: value.property.into(),
            kind: value.kind.into(),
        }
    }
}

impl From<ATy<AProvenancePair>> for Ty<ProvenancePair> {
    fn from(value: ATy<AProvenancePair>) -> Self {
        Ty {
            property: value.property.into(),
            kind: value.kind.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyKind<T> {
    Opaque,
    Fn(Vec<Ty<T>>, Box<Ty<T>>),
    Tuple(Vec<Ty<T>>),
    Array(Box<Ty<T>>),
    Adt(HashMap<Symbol, Ty<T>>),
    Infer,
}

impl<T: Display> Display for TyKind<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fn(arg_tys, ret_ty) => {
                let args = arg_tys.iter().map(|ty| ty.to_string()).sorted().join(",");

                write!(f, " fn({}) -> {}", args, ret_ty)
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

impl From<ATyKind<AFlowPair>> for TyKind<FlowPair> {
    fn from(value: ATyKind<AFlowPair>) -> Self {
        match value {
            ATyKind::Fn(args, ret) => {
                let args = args.into_iter().map(|ty| ty.into()).collect();
                let ret = Box::new(Ty::from(*ret));
                Self::Fn(args, ret)
            }

            ATyKind::Struct(fields) => {
                let mut map = HashMap::new();

                for (ident, ty) in fields {
                    map.insert(ident.name, ty.into());
                }

                Self::Adt(map)
            }

            ATyKind::StructTuple(elements) => {
                let mut map = HashMap::new();

                for (i, ty) in elements.into_iter().enumerate() {
                    map.insert(Symbol::intern(&i.to_string()), ty.into());
                }

                Self::Adt(map)
            }

            ATyKind::Tuple(elements) => {
                let elements = elements.into_iter().map(|ty| ty.into()).collect();
                Self::Tuple(elements)
            }

            ATyKind::Array(element) => {
                let element = Box::new(Ty::from(*element));
                Self::Array(element)
            }

            ATyKind::Opaque => Self::Opaque,
        }
    }
}

impl From<ATyKind<AProvenancePair>> for TyKind<ProvenancePair> {
    fn from(value: ATyKind<AProvenancePair>) -> Self {
        match value {
            ATyKind::Fn(arg_tys, ret_ty) => Self::Fn(
                arg_tys.into_iter().map(|ty| ty.into()).collect(),
                Box::new(Ty::from(*ret_ty)),
            ),
            ATyKind::Tuple(item_tys) => {
                Self::Tuple(item_tys.into_iter().map(|ty| ty.into()).collect())
            }
            ATyKind::Array(item_ty) => Self::Array(Box::new(Ty::from(*item_ty))),
            ATyKind::Opaque => Self::Opaque,
            _ => todo!(),
        }
    }
}
