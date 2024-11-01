use rustc_middle::ty::TyCtxt;
use rustc_span::Span;

use crate::shared::{Result, Ty};

/// Bidirectional type checking requires us to propagate the type
/// we expect an expression to have as we move along.
#[derive(Clone)]
pub enum Expectation {
    /// We don't know what type this expression should have.
    NoExpectation,

    /// This expression should satisfy the given type.
    ExpectHasType(Ty),
}

impl Expectation {
    /// Checks whether the provided type `actual` matches the expectation.
    pub fn check(&self, tcx: TyCtxt, actual: Ty, span: Span) -> Result<Ty> {
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
