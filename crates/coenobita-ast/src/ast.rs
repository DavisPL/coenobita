use std::fmt::Display;

use rustc_span::Span;

use crate::flow::FlowPair;

#[derive(Clone)]
pub struct Ty {
    pub fpair: FlowPair,

    pub kind: TyKind,

    pub span: Span,
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.fpair, self.kind)
    }
}

#[derive(Clone)]
pub enum TyKind {
    Fn(Vec<Ty>, Box<Ty>),
    Tup(Vec<Ty>),
    Abstract,
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

            Self::Abstract => write!(f, ""),
        }
    }
}
