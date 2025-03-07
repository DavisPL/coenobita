use std::{
    collections::HashSet,
    fmt::{self, Display},
};

use rustc_span::Symbol;

pub trait Property: Clone + Default + Display + fmt::Debug {
    fn satisfies(&self, other: &Self) -> bool;

    fn merge(&self, other: Self) -> Self;

    fn bottom(origin: String) -> Self;

    fn top() -> Self;

    fn attr() -> Vec<Symbol>;
}
