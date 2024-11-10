use crate::{path::PathBuf, transmute};

pub use std::env::{
    var,
    var_os,
VarError};

pub fn temp_dir() -> PathBuf {
    transmute!(std::env::temp_dir())
}
