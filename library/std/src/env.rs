use crate::{path::PathBuf, transmute};

pub fn temp_dir() -> PathBuf {
    transmute!(std::env::temp_dir())
}
