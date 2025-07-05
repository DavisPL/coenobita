#![feature(rustc_private)]

extern crate rustc_span;

use coenobita_middle::set::SetBind;
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt;

use coenobita_middle::property::Property;
use coenobita_middle::ty::{Type, TypeForm};

use rustc_span::symbol::Ident;
use rustc_span::{Span, Symbol};

#[derive(Clone, Debug)]
pub struct TyAST {
    pub inner: Type,
    pub span: Span,
}

impl fmt::Display for TyAST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

#[derive(Clone, Debug)]
pub enum TypeFormAST {
    /// Type with shape `fn(S, T) -> U`.
    Fn(Vec<SetBind>, Vec<TyAST>, Box<TyAST>),

    /// Type with shape `struct { x: T, y: U }`.
    Struct(Vec<(Ident, TyAST)>),

    /// Type with shape `struct (T, U)`.
    StructTuple(Vec<TyAST>),

    /// Type with shape `(S, T, U)`.
    Tuple(Vec<TyAST>),

    /// Type with shape `[T]`.
    Array(Box<TyAST>),

    /// Type with unknown (opaque) shape.
    Opaque,
}

impl Into<Type> for TyAST {
    fn into(self) -> Type {
        self.inner
    }
}

impl Into<TypeForm> for TypeFormAST {
    fn into(self) -> TypeForm {
        match self {
            Self::Fn(bounds, args, rty) => TypeForm::Fn(
                bounds,
                args.into_iter().map(|a| a.into()).collect(),
                Box::new(rty.inner.into()),
            ),
            Self::Tuple(fields) => TypeForm::Tuple(fields.into_iter().map(|a| a.into()).collect()),
            Self::Array(ty) => TypeForm::Array(Box::new(ty.inner.into())),
            Self::Struct(fields) => {
                let mut map = HashMap::new();

                for (ident, ty) in fields {
                    map.insert(ident.name.to_ident_string(), ty.into());
                }

                TypeForm::Adt(map)
            }
            Self::StructTuple(elements) => {
                let mut map = HashMap::new();

                for (i, ty) in elements.into_iter().enumerate() {
                    map.insert(i.to_string(), ty.into());
                }

                TypeForm::Adt(map)
            }
            Self::Opaque => TypeForm::Opaque,
        }
    }
}

impl fmt::Display for TypeFormAST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fn(_, args, ret) => {
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
