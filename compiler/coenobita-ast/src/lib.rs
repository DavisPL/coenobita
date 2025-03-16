#![feature(rustc_private)]

extern crate rustc_span;

use itertools::Itertools;
use std::collections::HashMap;
use std::fmt;

use coenobita_middle::property::Property;
use coenobita_middle::ty::{Ty, TyKind};

use rustc_span::symbol::Ident;
use rustc_span::{Span, Symbol};

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
        match self {
            Self::Fn(args, ret) => TyKind::Fn(
                None,
                args.into_iter().map(|t| t.into()).collect(),
                Box::new(ret.inner.into()),
            ),
            Self::Tuple(fields) => TyKind::Tuple(fields.into_iter().map(|t| t.into()).collect()),
            Self::Array(ty) => TyKind::Array(Box::new(ty.inner)),
            Self::Struct(fields) => {
                let mut map = HashMap::new();

                for (ident, ty) in fields {
                    map.insert(ident.name.to_ident_string(), ty.into());
                }

                TyKind::Adt(map)
            }
            Self::StructTuple(elements) => {
                let mut map = HashMap::new();

                for (i, ty) in elements.into_iter().enumerate() {
                    map.insert(i.to_string(), ty.into());
                }

                TyKind::Adt(map)
            }
            Self::Opaque => TyKind::Opaque,
        }
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
