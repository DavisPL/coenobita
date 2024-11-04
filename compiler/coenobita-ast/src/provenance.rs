use std::fmt::Display;

use rustc_span::{symbol::Ident, Span};

#[derive(Clone, Debug)]
pub struct ProvenancePair {
    pub first: Provenance,

    pub last: Provenance,

    pub span: Span,
}

impl Display for ProvenancePair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.first, self.last)
    }
}

#[derive(Clone, Debug)]
pub enum Provenance {
    Specific(Ident, Span),
    Universal(Span),
}

impl Display for Provenance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Specific(origin, _) => write!(f, "{origin}"),
            Self::Universal(_) => write!(f, "*"),
        }
    }
}
