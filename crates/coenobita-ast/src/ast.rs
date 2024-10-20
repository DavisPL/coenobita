use rustc_span::Span;

use crate::flow::FlowPair;

pub struct Ty {
    pub fpair: FlowPair,

    pub kind: TyKind,

    pub span: Span,
}

pub enum TyKind {
    Fn(Vec<Ty>, Box<Ty>),
    Tup(Vec<Ty>),
    Abstract,
}
