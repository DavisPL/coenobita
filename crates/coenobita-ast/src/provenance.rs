use rustc_span::{symbol::Ident, Span};

pub struct ProvenancePair {
    pub first: Ident,

    pub last: Ident,

    pub span: Span,
}
