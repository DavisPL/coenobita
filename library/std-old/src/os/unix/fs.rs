pub use std::os::unix::fs::{DirEntryExt, MetadataExt};

use crate::cap::AsRef;
use crate::{fs, io, path::Path, transmute};

#[inline(always)]
pub fn symlink<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> io::Result<()> {
    transmute!(std::os::unix::fs::symlink(
        &AsRef::as_ref_cap(&original).inner,
        &AsRef::as_ref_cap(&link).inner
    ))
}

impl MetadataExt for fs::Metadata {
    #[inline(always)]
    fn dev(&self) -> u64 {
        self.inner.dev()
    }

    #[inline(always)]
    fn ino(&self) -> u64 {
        self.inner.ino()
    }

    #[inline(always)]
    fn mode(&self) -> u32 {
        self.inner.mode()
    }

    #[inline(always)]
    fn nlink(&self) -> u64 {
        self.inner.nlink()
    }

    #[inline(always)]
    fn uid(&self) -> u32 {
        self.inner.uid()
    }

    #[inline(always)]
    fn gid(&self) -> u32 {
        self.inner.gid()
    }

    #[inline(always)]
    fn rdev(&self) -> u64 {
        self.inner.rdev()
    }

    #[inline(always)]
    fn size(&self) -> u64 {
        self.inner.size()
    }

    #[inline(always)]
    fn atime(&self) -> i64 {
        self.inner.atime()
    }

    #[inline(always)]
    fn atime_nsec(&self) -> i64 {
        self.inner.atime_nsec()
    }

    #[inline(always)]
    fn mtime(&self) -> i64 {
        self.inner.mtime()
    }

    #[inline(always)]
    fn mtime_nsec(&self) -> i64 {
        self.inner.mtime_nsec()
    }

    #[inline(always)]
    fn ctime(&self) -> i64 {
        self.inner.ctime()
    }

    #[inline(always)]
    fn ctime_nsec(&self) -> i64 {
        self.inner.ctime_nsec()
    }

    #[inline(always)]
    fn blksize(&self) -> u64 {
        self.inner.blksize()
    }

    #[inline(always)]
    fn blocks(&self) -> u64 {
        self.inner.blocks()
    }
}
