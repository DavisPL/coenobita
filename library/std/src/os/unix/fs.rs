use std::os::unix::fs;
pub use std::os::unix::fs::*;

use crate::fs::OpenOptions;
use crate::{io, transmute};
use crate::path::Path;

#[inline(always)]
pub fn symlink<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> io::Result<()> {
    fs::symlink(&original.as_ref().inner, &link.as_ref().inner)
}

impl OpenOptionsExt for OpenOptions {
    fn mode(&mut self, mode: u32) -> &mut OpenOptions {
        transmute!(self.inner.mode(mode))
    }

    fn custom_flags(&mut self, flags: i32) -> &mut OpenOptions {
        transmute!(self.inner.custom_flags(flags))
    }
}

impl DirBuilderExt for crate::fs::DirBuilder {
    fn mode(&mut self, mode: u32) -> &mut crate::fs::DirBuilder {
        transmute!(self.inner.mode(mode))
    }
}