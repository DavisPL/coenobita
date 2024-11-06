use crate::ops::Deref;
use std::borrow::Borrow;
use std::path::{self, Prefix};
use std::str::FromStr;
use std::{cmp, fmt};

use crate::borrow::Cow;
use crate::collections::TryReserveError;
use crate::ffi::{OsStr, OsString};
use crate::io;
use crate::{fs, transmute};

pub struct Components<'a> {
    inner: path::Components<'a>,
}

pub struct Iter<'a> {
    inner: path::Iter<'a>,
}

pub struct Ancestors<'a> {
    inner: path::Ancestors<'a>,
}

pub struct PathBuf {
    inner: path::PathBuf,
}

impl Eq for PathBuf {}

impl PartialEq for PathBuf {
    #[inline]
    fn eq(&self, other: &PathBuf) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl PartialOrd for PathBuf {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}

impl Ord for PathBuf {
    #[inline]
    fn cmp(&self, other: &PathBuf) -> cmp::Ordering {
        self.inner.cmp(&other.inner)
    }
}

impl<T: ?Sized + AsRef<OsStr>> From<&T> for PathBuf {
    #[inline]
    fn from(s: &T) -> PathBuf {
        PathBuf::from(s.as_ref().to_os_string())
    }
}

impl From<OsString> for PathBuf {
    #[inline]
    fn from(s: OsString) -> PathBuf {
        PathBuf { inner: s.into() }
    }
}

impl From<PathBuf> for OsString {
    #[inline]
    fn from(path_buf: PathBuf) -> OsString {
        path_buf.inner.into()
    }
}

impl From<String> for PathBuf {
    #[inline]
    fn from(s: String) -> PathBuf {
        PathBuf::from(OsString::from(s))
    }
}

impl FromStr for PathBuf {
    type Err = core::convert::Infallible;

    #[inline]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(PathBuf::from(s))
    }
}

impl PathBuf {
    #[inline]
    pub fn new() -> PathBuf {
        PathBuf {
            inner: path::PathBuf::new(),
        }
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> PathBuf {
        PathBuf {
            inner: path::PathBuf::with_capacity(capacity),
        }
    }

    #[inline]
    pub fn as_path(&self) -> &Path {
        self
    }

    #[inline]
    pub fn push<P: AsRef<Path>>(&mut self, path: P) {
        self.inner.push(&path.as_ref().inner);
    }

    #[inline]
    pub fn pop(&mut self) -> bool {
        self.inner.pop()
    }

    #[inline]
    pub fn set_file_name<S: AsRef<OsStr>>(&mut self, file_name: S) {
        self.inner.set_file_name(file_name.as_ref());
    }

    #[inline]
    pub fn set_extension<S: AsRef<OsStr>>(&mut self, extension: S) -> bool {
        self.inner.set_extension(extension.as_ref())
    }

    #[inline]
    pub fn as_mut_os_string(&mut self) -> &mut OsString {
        self.inner.as_mut_os_string()
    }

    #[inline]
    pub fn into_os_string(self) -> OsString {
        self.inner.into_os_string()
    }

    #[inline]
    pub fn into_boxed_path(self) -> Box<Path> {
        unsafe { std::mem::transmute(self.inner.into_boxed_path()) }
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }

    #[inline]
    pub fn clear(&mut self) {
        self.inner.clear()
    }

    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional)
    }

    #[inline]
    pub fn try_reserve(&mut self, additional: usize) -> Result<(), TryReserveError> {
        self.inner.try_reserve(additional)
    }

    #[inline]
    pub fn reserve_exact(&mut self, additional: usize) {
        self.inner.reserve_exact(additional)
    }

    #[inline]
    pub fn try_reserve_exact(&mut self, additional: usize) -> Result<(), TryReserveError> {
        self.inner.try_reserve_exact(additional)
    }

    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.inner.shrink_to_fit()
    }

    #[inline]
    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.inner.shrink_to(min_capacity)
    }
}

impl Clone for PathBuf {
    #[inline]
    fn clone(&self) -> Self {
        PathBuf {
            inner: self.inner.clone(),
        }
    }

    #[inline]
    fn clone_from(&mut self, source: &Self) {
        self.inner.clone_from(&source.inner)
    }
}

impl fmt::Debug for PathBuf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl Deref for PathBuf {
    type Target = Path;

    #[inline]
    fn deref(&self) -> &Path {
        Path::new(&self.inner)
    }
}

impl Borrow<Path> for PathBuf {
    #[inline]
    fn borrow(&self) -> &Path {
        self.deref()
    }
}


macro_rules! impl_cmp {
    (<$($life:lifetime),*> $lhs:ty, $rhs: ty) => {
        impl<$($life),*> PartialEq<$rhs> for $lhs {
            #[inline]
            fn eq(&self, other: &$rhs) -> bool {
                <Path as PartialEq>::eq(self, other)
            }
        }

        impl<$($life),*> PartialEq<$lhs> for $rhs {
            #[inline]
            fn eq(&self, other: &$lhs) -> bool {
                <Path as PartialEq>::eq(self, other)
            }
        }

        impl<$($life),*> PartialOrd<$rhs> for $lhs {
            #[inline]
            fn partial_cmp(&self, other: &$rhs) -> Option<cmp::Ordering> {
                <Path as PartialOrd>::partial_cmp(self, other)
            }
        }

        impl<$($life),*> PartialOrd<$lhs> for $rhs {
            #[inline]
            fn partial_cmp(&self, other: &$lhs) -> Option<cmp::Ordering> {
                <Path as PartialOrd>::partial_cmp(self, other)
            }
        }
    };
}

impl_cmp!(<> PathBuf, Path);
impl_cmp!(<'a> PathBuf, &'a Path);


#[repr(transparent)]
pub struct Path {
    pub(crate) inner: path::Path,
}

pub struct StripPrefixError(());

impl Path {
    #[inline]
    pub fn new<S: AsRef<OsStr> + ?Sized>(s: &S) -> &Path {
        unsafe { &*(s.as_ref() as *const OsStr as *const Path) }
    }

    #[inline]
    pub fn as_os_str(&self) -> &OsStr {
        self.inner.as_os_str()
    }

    #[inline]
    pub fn as_mut_os_str(&mut self) -> &mut OsStr {
        self.inner.as_mut_os_str()
    }

    #[inline]
    pub fn to_str(&self) -> Option<&str> {
        self.inner.to_str()
    }

    #[inline]
    pub fn to_string_lossy(&self) -> Cow<'_, str> {
        self.inner.to_string_lossy()
    }

    #[inline]
    pub fn to_path_buf(&self) -> PathBuf {
        unsafe { std::mem::transmute(self.inner.to_path_buf()) }
    }

    #[inline]
    pub fn is_absolute(&self) -> bool {
        self.inner.is_absolute()
    }

    #[inline]
    pub fn is_relative(&self) -> bool {
        !self.is_absolute()
    }

    #[inline]
    pub fn has_root(&self) -> bool {
        self.inner.has_root()
    }

    #[inline]
    pub fn parent(&self) -> Option<&Path> {
        unsafe { std::mem::transmute(self.inner.parent()) }
    }

    #[inline]
    pub fn ancestors(&self) -> Ancestors<'_> {
        unsafe { std::mem::transmute(self.inner.ancestors()) }
    }

    #[inline]
    pub fn file_name(&self) -> Option<&OsStr> {
        self.inner.file_name()
    }

    #[inline]
    pub fn strip_prefix<P>(&self, base: P) -> Result<&Path, StripPrefixError>
    where
        P: AsRef<Path>,
    {
        unsafe { std::mem::transmute(self.inner.strip_prefix(&base.as_ref().inner)) }
    }

    #[inline]
    pub fn starts_with<P: AsRef<Path>>(&self, base: P) -> bool {
        self.inner.starts_with(&base.as_ref().inner)
    }

    #[inline]
    pub fn ends_with<P: AsRef<Path>>(&self, child: P) -> bool {
        self.inner.ends_with(&child.as_ref().inner)
    }

    #[inline]
    pub fn file_stem(&self) -> Option<&OsStr> {
        self.inner.file_stem()
    }

    #[inline]
    pub fn extension(&self) -> Option<&OsStr> {
        self.inner.extension()
    }

    #[inline]
    pub fn join<P: AsRef<Path>>(&self, path: P) -> PathBuf {
        unsafe { std::mem::transmute(self.inner.join(&path.as_ref().inner)) }
    }

    #[inline]
    pub fn with_file_name<S: AsRef<OsStr>>(&self, file_name: S) -> PathBuf {
        unsafe { std::mem::transmute(self.inner.with_file_name(file_name)) }
    }

    #[inline]
    pub fn with_extension<S: AsRef<OsStr>>(&self, extension: S) -> PathBuf {
        unsafe { std::mem::transmute(self.inner.with_extension(extension)) }
    }

    #[inline]
    pub fn components(&self) -> Components<'_> {
        unsafe { std::mem::transmute(self.inner.components()) }
    }

    #[inline]
    pub fn iter(&self) -> Iter<'_> {
        unsafe { std::mem::transmute(self.inner.iter()) }
    }

    #[inline]
    pub fn display(&self) -> Display<'_> {
        unsafe { std::mem::transmute(self.inner.display()) }
    }

    #[inline]
    pub fn metadata(&self) -> io::Result<fs::Metadata> {
        unsafe { std::mem::transmute(self.inner.metadata()) }
    }

    #[inline]
    pub fn symlink_metadata(&self) -> io::Result<fs::Metadata> {
        unsafe { std::mem::transmute(self.inner.symlink_metadata()) }
    }

    #[inline]
    pub fn canonicalize(&self) -> io::Result<PathBuf> {
        unsafe { std::mem::transmute(self.inner.canonicalize()) }
    }

    #[inline]
    pub fn read_link(&self) -> io::Result<PathBuf> {
        unsafe { std::mem::transmute(self.inner.read_link()) }
    }

    #[inline]
    pub fn read_dir(&self) -> io::Result<fs::ReadDir> {
        unsafe { std::mem::transmute(self.inner.read_dir()) }
    }

    #[inline]
    pub fn exists(&self) -> bool {
        self.inner.exists()
    }

    #[inline]
    pub fn try_exists(&self) -> io::Result<bool> {
        transmute!(self.inner.try_exists())
    }

    #[inline]
    pub fn is_file(&self) -> bool {
        self.inner.is_file()
    }

    #[must_use]
    pub fn is_dir(&self) -> bool {
        self.inner.is_dir()
    }

    #[inline]
    pub fn is_symlink(&self) -> bool {
        self.inner.is_symlink()
    }

    #[inline]
    pub fn into_path_buf(self: Box<Path>) -> PathBuf {
        todo!()
    }
}

pub struct Display<'a> {
    inner: path::Display<'a>,
}

impl fmt::Debug for Display<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

impl AsRef<Path> for Path {
    #[inline]
    fn as_ref(&self) -> &Path {
        self
    }
}

impl AsRef<Path> for PathBuf {
    #[inline]
    fn as_ref(&self) -> &Path {
        self
    }
}

impl AsRef<Path> for str {
    #[inline]
    fn as_ref(&self) -> &Path {
        Path::new(self)
    }
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl AsRef<Path> for String {
    #[inline]
    fn as_ref(&self) -> &Path {
        Path::new(self)
    }
}

impl PartialEq for Path {
    #[inline]
    fn eq(&self, other: &Path) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl Eq for Path {}

impl PartialOrd for Path {
    #[inline]
    fn partial_cmp(&self, other: &Path) -> Option<cmp::Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}

impl Ord for Path {
    #[inline]
    fn cmp(&self, other: &Path) -> cmp::Ordering {
        self.inner.cmp(&other.inner)
    }
}
