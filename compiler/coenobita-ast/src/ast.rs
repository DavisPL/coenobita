use std::fmt::Display;

use itertools::Itertools;
use rustc_span::{symbol::Ident, Span};

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
    /// Type with shape `fn(S, T) -> U`.
    Fn(Vec<Ty<T>>, Box<Ty<T>>),

    /// Type with shape `struct { x: T, y: U }`.
    Struct(Vec<(Ident, Ty<T>)>),

    /// Type with shape `struct (T, U)`.
    StructTuple(Vec<Ty<T>>),

    /// Type with shape `(S, T, U)`.
    Tuple(Vec<Ty<T>>),

    /// Type with shape `[T]`.
    Array(Box<Ty<T>>),

    /// Type with unknown (opaque) shape.
    Opaque,
}

impl<T: Display> Display for TyKind<T> {
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
