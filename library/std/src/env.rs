pub use std::env::{self, *};

use crate::io;
use crate::path::Path;
use crate::{path::PathBuf, transmute};

#[inline(always)]
pub fn temp_dir() -> PathBuf {
    transmute!(env::temp_dir())
}

#[inline(always)]
pub fn current_dir() -> io::Result<PathBuf> {
    transmute!(env::current_dir())
}

#[inline(always)]
pub fn set_current_dir<P: AsRef<Path>>(path: P) -> io::Result<()> {
    env::set_current_dir(&path.as_ref().inner)
}

#[inline(always)]
pub fn current_exe() -> io::Result<PathBuf> {
    transmute!(env::current_exe())
}