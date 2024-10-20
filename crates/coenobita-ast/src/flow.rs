use std::fmt::Display;

use rustc_span::symbol::Ident;
use rustc_span::Span;

pub struct FlowPair {
    pub explicit: FlowSet,

    pub implicit: FlowSet,

    pub span: Span,
}

impl Display for FlowPair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.explicit, self.implicit)
    }
}

pub enum FlowSet {
    Specific(Vec<Ident>, Span),
    Universal(Span),
}

impl Display for FlowSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Specific(origins, _) => {
                let origins = origins
                    .iter()
                    .map(|ident| ident.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "{{{origins}}}")
            }

            Self::Universal(_) => {
                write!(f, "{{*}}")
            }
        }
    }
}
