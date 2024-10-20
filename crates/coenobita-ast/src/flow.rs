use rustc_span::symbol::Ident;
use rustc_span::Span;

pub struct FlowPair {
    pub explicit: FlowSet,

    pub implicit: FlowSet,

    pub span: Span,
}

pub enum FlowSet {
    Specific(Vec<Ident>, Span),
    Universal(Span),
}
