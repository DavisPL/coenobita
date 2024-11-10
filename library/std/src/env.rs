use std::env;

use crate::{path::PathBuf, transmute};

#[inline(always)]
pub fn temp_dir() -> PathBuf {
    transmute!(env::temp_dir())
}
