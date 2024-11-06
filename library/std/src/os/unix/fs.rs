pub use std::os::unix::fs::{DirEntryExt, MetadataExt};

use crate::{fs, io, path::Path, transmute};

pub fn symlink<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> io::Result<()> {
    transmute!(std::os::unix::fs::symlink(
        &original.as_ref().inner,
        &link.as_ref().inner
    ))
}

impl MetadataExt for fs::Metadata {
    fn dev(&self) -> u64 {
        self.inner.dev()
    }
    fn ino(&self) -> u64 {
        self.inner.ino()
    }
    fn mode(&self) -> u32 {
        self.inner.mode()
    }
    fn nlink(&self) -> u64 {
        self.inner.nlink()
    }
    fn uid(&self) -> u32 {
        self.inner.uid()
    }
    fn gid(&self) -> u32 {
        self.inner.gid()
    }
    fn rdev(&self) -> u64 {
        self.inner.rdev()
    }
    fn size(&self) -> u64 {
        self.inner.size()
    }
    fn atime(&self) -> i64 {
        self.inner.atime()
    }
    fn atime_nsec(&self) -> i64 {
        self.inner.atime_nsec()
    }
    fn mtime(&self) -> i64 {
        self.inner.mtime()
    }
    fn mtime_nsec(&self) -> i64 {
        self.inner.mtime_nsec()
    }
    fn ctime(&self) -> i64 {
        self.inner.ctime()
    }
    fn ctime_nsec(&self) -> i64 {
        self.inner.ctime_nsec()
    }
    fn blksize(&self) -> u64 {
        self.inner.blksize()
    }
    fn blocks(&self) -> u64 {
        self.inner.blocks()
    }
}
