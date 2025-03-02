use std::{convert, ffi::OsStr};

use crate::path::{Path, PathBuf};

pub type JoinFn = fn(&OsStr, &OsStr) -> PathBuf;
pub type PushFn = fn(&mut Path, &OsStr);

pub trait From<T> {
    #[cnbt::provenance((*,*) fn((*, root)) -> (*,root))]
    fn from(value: T) -> Self;
}
