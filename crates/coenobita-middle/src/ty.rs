use std::{collections::HashMap, fmt::Display};

use crate::flow::{FlowPair, FlowSet};
use coenobita_ast::ast::{self, Ty as ATy, TyKind as ATyKind};
use coenobita_ast::flow::FlowPair as AFlowPair;
use itertools::Itertools;
use rustc_span::Symbol;

#[derive(Debug, Clone)]
pub struct Ty<T> {
    pub property: T,

    pub kind: TyKind<T>,
}

impl Ty<FlowPair> {
    pub fn new(fpair: FlowPair, kind: TyKind<FlowPair>) -> Self {
        Ty {
            property: fpair,
            kind,
        }
    }

    pub fn with_explicit(mut self, explicit: FlowSet) -> Self {
        self.property.0 = explicit;
        self
    }

    pub fn merge(self, other: Ty<FlowPair>) -> Ty<FlowPair> {
        let explicit = self.property.explicit().union(other.property.explicit());
        let implicit = self.property.implicit().union(other.property.implicit());

        // We assume both types have the same shape
        Ty::new(FlowPair(explicit, implicit), self.kind())
    }

    pub fn kind(self) -> TyKind<FlowPair> {
        self.kind
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

    pub fn satisfies(&self, other: &Ty<FlowPair>) -> bool {
        // TODO: Take type shape (kind) into account
        self.property
            .explicit()
            .is_subset(other.property.explicit())
            && self
                .property
                .implicit()
                .is_subset(other.property.implicit())
    }

    pub fn origins(&self) -> Vec<String> {
        self.property.origins()
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

#[derive(Debug, Clone)]
pub enum TyKind<T> {
    Abs,
    Fn(Vec<Ty<T>>, Box<Ty<T>>),
    Tup(Vec<Ty<T>>),
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

            Self::Tup(item_tys) => {
                let items = item_tys
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, " ({})", items)
            }

            Self::Adt(field_tys) => {
                let fields = field_tys
                    .iter()
                    .sorted_by_key(|&(key, _)| key)
                    .map(|(key, value)| format!("{key}:{value}"))
                    .collect::<Vec<_>>()
                    .join(",");

                write!(f, " struct {{{fields}}}")
            }

            Self::Abs => write!(f, ""),
            Self::Infer => write!(f, ""),
        }
    }
}

impl From<ATyKind<AFlowPair>> for TyKind<FlowPair> {
    fn from(value: ATyKind<AFlowPair>) -> Self {
        match value {
            ast::TyKind::Abstract => Self::Abs,
            ast::TyKind::Fn(arg_tys, ret_ty) => Self::Fn(
                arg_tys.into_iter().map(|ty| ty.into()).collect(),
                Box::new(Ty::from(*ret_ty)),
            ),
            ast::TyKind::Tup(item_tys) => {
                Self::Tup(item_tys.into_iter().map(|ty| ty.into()).collect())
            }
        }
    }
}
