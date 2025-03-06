use coenobita_middle::{property::Property, ty::Ty};
use rustc_middle::ty::TyCtxt;
use rustc_span::Span;

use crate::shared::Result;

/// Bidirectional type checking requires us to propagate the type
/// we expect an expression to have as we move along.
#[derive(Debug, Clone)]
pub enum Expectation<P: Property> {
    /// We don't know what type this expression should have.
    NoExpectation,

    /// This expression should satisfy the given type.
    ExpectHasType(Ty<P>),
}

impl<P: Property> Expectation<P> {
    /// Checks whether the provided type `actual` matches the expectation.
    pub fn check(&self, tcx: TyCtxt, actual: Ty<P>, span: Span) -> Result<Ty<P>> {
        match self {
            Self::NoExpectation => Ok(actual),
            Self::ExpectHasType(expected) => {
                if actual.satisfies(expected) {
                    Ok(expected.clone())
                } else {
                    let msg = format!("expected {expected}, found {actual}");
                    Err(tcx.dcx().span_err(span, msg))
                }
            }
        }
    }
}
