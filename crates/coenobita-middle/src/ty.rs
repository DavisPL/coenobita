use std::fmt::Display;

use crate::flow::{FlowPair, FlowSet};
use coenobita_ast::ast::{self, Ty as ATy, TyKind as ATyKind};

#[derive(Debug, Clone)]
pub struct Ty {
    pub fpair: FlowPair,

    pub kind: TyKind,
}

impl Ty {
    pub fn new(fpair: FlowPair, kind: TyKind) -> Self {
        Ty { fpair, kind }
    }

    pub fn default() -> Self {
        Ty {
            fpair: FlowPair::default(),
            kind: TyKind::Abs,
        }
    }

    pub fn with_explicit(mut self, explicit: FlowSet) -> Self {
        self.fpair.0 = explicit;
        self
    }

    pub fn merge(self, other: Ty) -> Ty {
        todo!()
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

impl From<ATy> for Ty {
    fn from(value: ATy) -> Self {
        Ty {
            fpair: value.fpair.into(),
            kind: value.kind.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TyKind {
    Abs,
    Fn(Vec<Ty>, Box<Ty>),
    Tup(Vec<Ty>),
    Infer,
}

impl Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fn(arg_tys, ret_ty) => {
                let args = arg_tys
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
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

            Self::Abs => write!(f, ""),
            _ => todo!(),
        }
    }
}

impl From<ATyKind> for TyKind {
    fn from(value: ATyKind) -> Self {
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
