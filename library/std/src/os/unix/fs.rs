use std::os::unix::fs;
pub use std::os::unix::fs::*;

use crate::io;
use crate::path::Path;

#[inline(always)]
pub fn symlink<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> io::Result<()> {
    fs::symlink(&original.as_ref().inner, &link.as_ref().inner)
}
