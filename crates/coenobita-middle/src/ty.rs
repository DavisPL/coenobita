use std::{collections::HashMap, fmt::Display};

use crate::flow::{FlowPair, FlowSet};
use coenobita_ast::ast::{self, Ty as ATy, TyKind as ATyKind};
use coenobita_ast::flow::FlowPair as AFlowPair;
use itertools::Itertools;
use rustc_span::Symbol;

#[derive(Debug, Clone)]
pub struct Ty {
    pub fpair: FlowPair,

    pub kind: TyKind,
}

impl Ty {
    pub fn new(fpair: FlowPair, kind: TyKind) -> Self {
        Ty { fpair, kind }
    }

    pub fn with_explicit(mut self, explicit: FlowSet) -> Self {
        self.fpair.0 = explicit;
        self
    }

    pub fn merge(self, other: Ty) -> Ty {
        let explicit = self.fpair.explicit().union(other.fpair.explicit());
        let implicit = self.fpair.implicit().union(other.fpair.implicit());

        // We assume both types have the same shape
        Ty::new(FlowPair(explicit, implicit), self.kind())
    }

    pub fn kind(self) -> TyKind {
        self.kind
    }

    pub fn ty_fn(n: usize) -> Ty {
        let default_flow_pair = FlowPair::new(FlowSet::Universal, FlowSet::Universal);
        let default_ty = Ty::new(default_flow_pair.clone(), TyKind::Infer);

        let args = vec![default_ty.clone(); n]; // Create `n` copies of `default_ty`

        Ty {
            fpair: default_flow_pair,
            kind: TyKind::Fn(args, Box::new(default_ty)),
        }
    }

    pub fn satisfies(&self, other: &Ty) -> bool {
        // TODO: Take type shape (kind) into account
        self.fpair.explicit().is_subset(other.fpair.explicit())
            && self.fpair.implicit().is_subset(other.fpair.implicit())
    }

    pub fn origins(&self) -> Vec<String> {
        self.fpair.origins()
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.fpair, self.kind)
    }
}

impl From<ATy<AFlowPair>> for Ty {
    fn from(value: ATy<AFlowPair>) -> Self {
        Ty {
            fpair: value.property.into(),
            kind: value.kind.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TyKind {
    Abs,
    Fn(Vec<Ty>, Box<Ty>),
    Tup(Vec<Ty>),
    Adt(HashMap<Symbol, Ty>),
    Infer,
}

impl Display for TyKind {
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

impl From<ATyKind<AFlowPair>> for TyKind {
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
