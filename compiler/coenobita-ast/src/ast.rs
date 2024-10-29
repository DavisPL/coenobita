use std::fmt::Display;

use itertools::Itertools;
use rustc_span::Span;

#[derive(Clone)]
pub struct Ty<T> {
    pub property: T,

    pub kind: TyKind<T>,

    pub span: Span,
}

impl<T: Display> Display for Ty<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.property, self.kind)
    }
}

#[derive(Clone)]
pub enum TyKind<T> {
    Fn(Vec<Ty<T>>, Box<Ty<T>>),
    Tup(Vec<Ty<T>>),
    Arr(Box<Ty<T>>),
    Abstract,
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

            Self::Arr(item_ty) => write!(f, " [{}]", item_ty),

            Self::Abstract => write!(f, ""),
        }
    }
}
