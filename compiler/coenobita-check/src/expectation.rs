use coenobita_middle::{property::Property, ty::Type};
use rustc_middle::ty::TyCtxt;
use rustc_span::Span;

use crate::shared::Result;
use log::debug;

/// Bidirectional type checking requires us to propagate the type
/// we expect an expression to have as we move along.
#[derive(Debug, Clone)]
pub enum Expectation {
    /// We don't know what type this expression should have.
    NoExpectation,

    /// This expression should satisfy the given type.
    ExpectHasType(Type),
}

impl Expectation {
    /// Checks whether the provided type `actual` matches the expectation.
    pub fn check(&self, tcx: TyCtxt, actual: Type, span: Span) -> Result<Type> {
        //
        todo!()
    }
}
