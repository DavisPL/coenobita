#![feature(rustc_private)]

extern crate rustc_span;

use itertools::Itertools;
use std::fmt;

use coenobita_middle::property::Property;
use coenobita_middle::ty::{Ty, TyKind};

use rustc_span::symbol::Ident;
use rustc_span::Span;

#[derive(Clone, Debug)]
pub struct TyAST<P: Property> {
    pub inner: Ty<P>,

    pub span: Span,
}

impl<P: Property> fmt::Display for TyAST<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

#[derive(Clone, Debug)]
pub enum TyKindAST<P: Property> {
    /// Type with shape `fn(S, T) -> U`.
    Fn(Vec<TyAST<P>>, Box<TyAST<P>>),

    /// Type with shape `struct { x: T, y: U }`.
    Struct(Vec<(Ident, TyAST<P>)>),

    /// Type with shape `struct (T, U)`.
    StructTuple(Vec<TyAST<P>>),

    /// Type with shape `(S, T, U)`.
    Tuple(Vec<TyAST<P>>),

    /// Type with shape `[T]`.
    Array(Box<TyAST<P>>),

    /// Type with unknown (opaque) shape.
    Opaque,
}

impl<P: Property> Into<Ty<P>> for TyAST<P> {
    fn into(self) -> Ty<P> {
        self.inner
    }
}

impl<P: Property> Into<TyKind<P>> for TyKindAST<P> {
    fn into(self) -> TyKind<P> {
        unimplemented!()
    }
}

impl<P: Property> fmt::Display for TyKindAST<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fn(args, ret) => {
                let args = args.iter().map(|ty| ty.to_string()).sorted().join(",");
                write!(f, " fn({args}) -> {ret}")
            }

            Self::Struct(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name, ty)| format!("{name}:{ty}"))
                    .collect::<Vec<_>>()
                    .join(",");

                write!(f, " struct {{ {fields} }}")
            }

            Self::StructTuple(elements) => {
                let elements = elements
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(",");

                write!(f, " struct ({elements})")
            }

            Self::Tuple(elements) => {
                let elements = elements
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(",");

                write!(f, " ({elements})")
            }

            Self::Array(element) => write!(f, " [{element}]"),

            Self::Opaque => write!(f, ""),
        }
    }
}
