use std::path;

#[repr(transparent)]
pub struct PathBuf {
    pub(crate) inner: path::PathBuf,
}

#[repr(transparent)]
pub struct Path {
    pub(crate) inner: path::Path,
}

impl AsRef<Path> for PathBuf {
    fn as_ref(&self) -> &Path {
        unsafe { std::mem::transmute(self.inner.as_path()) }
    }
}
