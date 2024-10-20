use rustc_span::Span;

use crate::flow::FlowPair;

pub struct Ty {
    pub flow_pair: FlowPair,

    pub kind: TyKind,

    pub span: Span,
}

pub enum TyKind {
    Fn(Vec<Ty>, Box<Ty>),
    Tup(Vec<Ty>),
    Abstract,
}
