use core::fmt;
use std::{borrow::Borrow, fmt::Debug, io, ops::Deref, path};

use crate::{ffi::OsStr, fs::Metadata};

pub struct Display<'a>(path::Display<'a>);

impl fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

#[derive(Clone)]
pub struct PathBuf {
    pub(crate) inner: path::PathBuf,
}

impl Debug for PathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl Borrow<Path> for PathBuf {
    fn borrow(&self) -> &Path {
        self.deref()
    }
}

impl Deref for PathBuf {
    type Target = Path;

    #[inline]
    fn deref(&self) -> &Path {
        Path::new(&self.inner)
    }
}

impl PathBuf {
    pub fn file_name(&self) -> Option<&OsStr> {
        self.inner.file_name()
    }
}

impl PartialEq for PathBuf {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl Eq for PathBuf {}

impl PartialOrd for PathBuf {
    #[inline]
    fn partial_cmp(&self, other: &PathBuf) -> Option<std::cmp::Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}

impl Ord for PathBuf {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.inner.cmp(&other.inner)
    }
}

impl<T: Into<path::PathBuf>> From<T> for PathBuf {
    fn from(value: T) -> Self {
        PathBuf {
            inner: value.into(),
        }
    }
}

pub struct Path {
    pub(crate) inner: path::Path,
}

impl Path {
    pub fn new<S: AsRef<OsStr> + ?Sized>(s: &S) -> &Path {
        unsafe { &*(s.as_ref() as *const OsStr as *const Path) }
    }

    #[inline]
    pub fn as_os_str(&self) -> &OsStr {
        self.inner.as_os_str()
    }

    pub fn to_path_buf(&self) -> PathBuf {
        PathBuf::from(self.inner.to_path_buf())
    }

    pub fn metadata(&self) -> io::Result<Metadata> {
        Ok(Metadata(self.inner.metadata()?))
    }

    pub fn display(&self) -> Display<'_> {
        Display(self.inner.display())
    }

    pub fn join<P: AsRef<Path>>(&self, path: P) -> PathBuf {
        PathBuf {
            inner: self.inner.join(&path.as_ref().inner),
        }
    }

    pub fn is_dir(&self) -> bool {
        self.inner.is_dir()
    }
}

impl Debug for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl AsRef<Path> for PathBuf {
    fn as_ref(&self) -> &Path {
        unsafe { std::mem::transmute(self.inner.as_path()) }
    }
}

impl AsRef<Path> for Path {
    fn as_ref(&self) -> &Path {
        &self
    }
}

impl PartialEq for Path {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl Eq for Path {}

impl PartialOrd for Path {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}

impl Ord for Path {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.inner.cmp(&other.inner)
    }
}
