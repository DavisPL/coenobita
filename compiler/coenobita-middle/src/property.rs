use std::{
    fmt::{self, Display},
    path::PathBuf,
};

use rustc_span::Symbol;
use serde::Serialize;

pub trait Property: Clone + Display + Default + fmt::Debug + Serialize {
    fn satisfies(&self, other: &Self) -> bool;

    fn influence(&self, other: Self) -> Self;

    fn merge(&self, other: Self) -> Self;

    fn bottom(origin: String) -> Self;

    fn top() -> Self;

    fn attr() -> Vec<Symbol>;

    fn intrinsics_path() -> PathBuf;
}
