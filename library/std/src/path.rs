use crate::ops::Deref;
use std::borrow::Borrow;
use std::path;
use std::str::FromStr;
use std::{cmp, fmt};

use crate::borrow::Cow;
use crate::collections::TryReserveError;
use crate::ffi::{OsStr, OsString};
use crate::io;
use crate::{fs, transmute};

use crate::cap::{AsRef, From};

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
    #[inline(always)]
    fn eq(&self, other: &PathBuf) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl PartialOrd for PathBuf {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}

impl Ord for PathBuf {
    #[inline(always)]
    fn cmp(&self, other: &PathBuf) -> cmp::Ordering {
        self.inner.cmp(&other.inner)
    }
}

impl<T: ?Sized + std::convert::AsRef<OsStr>> From<&T> for PathBuf {
    #[inline(always)]
    fn from(s: &T) -> PathBuf {
        PathBuf {
            inner: s.as_ref().into(),
        }
    }
}

impl From<OsString> for PathBuf {
    #[inline(always)]
    fn from(s: OsString) -> PathBuf {
        PathBuf { inner: s.into() }
    }
}

impl From<PathBuf> for OsString {
    #[inline(always)]
    fn from(path_buf: PathBuf) -> OsString {
        path_buf.inner.into()
    }
}

impl From<String> for PathBuf {
    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root)) -> (*,root))]
    fn from(s: String) -> PathBuf {
        PathBuf { inner: s.into() }
    }
}

// impl FromStr for PathBuf {
//     type Err = core::convert::Infallible;

//     #[inline(always)]
//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         Ok(PathBuf { inner: s.into() })
//     }
// }

impl PathBuf {
    #[inline(always)]
    pub fn new() -> PathBuf {
        transmute!(path::PathBuf::new())
    }

    #[inline(always)]
    pub fn with_capacity(capacity: usize) -> PathBuf {
        transmute!(path::PathBuf::with_capacity(capacity))
    }

    // No need for tag - doesn't create new path
    #[inline(always)]
    pub fn as_path(&self) -> &Path {
        self
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn push<P: AsRef<Path>>(&mut self, path: P) {
        self.inner.push(&<P as AsRef<Path>>::as_ref(&path).inner);
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root)) -> (*,root))]
    pub fn pop(&mut self) -> bool {
        self.inner.pop()
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn set_file_name<S: AsRef<OsStr>>(&mut self, file_name: S) {
        self.inner.set_file_name(&<S as AsRef<OsStr>>::as_ref(&file_name));
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn set_extension<S: AsRef<OsStr>>(&mut self, extension: S) -> bool {
        self.inner.set_extension(&<S as AsRef<OsStr>>::as_ref(&extension))
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root)) -> (*,root))]
    pub fn as_mut_os_string(&mut self) -> &mut OsString {
        self.inner.as_mut_os_string()
    }

    #[inline(always)]
    pub fn into_os_string(self) -> OsString {
        self.inner.into_os_string()
    }

    #[inline(always)]
    pub fn into_boxed_path(self) -> Box<Path> {
        unsafe { std::mem::transmute(self.inner.into_boxed_path()) }
    }

    #[inline(always)]
    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root)) -> (*,root))]
    pub fn clear(&mut self) {
        self.inner.clear()
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional)
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn try_reserve(&mut self, additional: usize) -> Result<(), TryReserveError> {
        self.inner.try_reserve(additional)
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn reserve_exact(&mut self, additional: usize) {
        self.inner.reserve_exact(additional)
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn try_reserve_exact(&mut self, additional: usize) -> Result<(), TryReserveError> {
        self.inner.try_reserve_exact(additional)
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root)) -> (*,root))]
    pub fn shrink_to_fit(&mut self) {
        self.inner.shrink_to_fit()
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.inner.shrink_to(min_capacity)
    }
}

impl Clone for PathBuf {
    #[inline(always)]
    fn clone(&self) -> Self {
        transmute!(self.inner.clone())
    }

    // No need for tag - nothing new being made here
    #[inline(always)]
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

    // No need for tag - doesn't create new path
    #[inline(always)]
    fn deref(&self) -> &Path {
        // TODO: See if this actually does what it should later
        unsafe { &*(std::convert::AsRef::as_ref(&self.inner) as *const OsStr as *const Path) }
    }
}

impl Borrow<Path> for PathBuf {
    // No need for tag - doesn't create new path
    #[inline(always)]
    fn borrow(&self) -> &Path {
        self.deref()
    }
}

macro_rules! impl_cmp {
    (<$($life:lifetime),*> $lhs:ty, $rhs: ty) => {
        impl<$($life),*> PartialEq<$rhs> for $lhs {
            #[inline(always)]
            fn eq(&self, other: &$rhs) -> bool {
                <Path as PartialEq>::eq(self, other)
            }
        }

        impl<$($life),*> PartialEq<$lhs> for $rhs {
            #[inline(always)]
            fn eq(&self, other: &$lhs) -> bool {
                <Path as PartialEq>::eq(self, other)
            }
        }

        impl<$($life),*> PartialOrd<$rhs> for $lhs {
            #[inline(always)]
            fn partial_cmp(&self, other: &$rhs) -> Option<cmp::Ordering> {
                <Path as PartialOrd>::partial_cmp(self, other)
            }
        }

        impl<$($life),*> PartialOrd<$lhs> for $rhs {
            #[inline(always)]
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
    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root)) -> (*,root))]
    pub fn new<S: AsRef<OsStr> + ?Sized>(s: &S) -> &Path {
        unsafe { &*(std::convert::AsRef::as_ref(&s) as *const OsStr as *const Path) }
    }

    #[inline(always)]
    pub fn as_os_str(&self) -> &OsStr {
        self.inner.as_os_str()
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root)) -> (*,root))]
    pub fn as_mut_os_str(&mut self) -> &mut OsStr {
        self.inner.as_mut_os_str()
    }

    #[inline(always)]
    pub fn to_str(&self) -> Option<&str> {
        self.inner.to_str()
    }

    #[inline(always)]
    pub fn to_string_lossy(&self) -> Cow<'_, str> {
        self.inner.to_string_lossy()
    }

    #[inline(always)]
    pub fn to_path_buf(&self) -> PathBuf {
        unsafe { std::mem::transmute(self.inner.to_path_buf()) }
    }

    #[inline(always)]
    pub fn is_absolute(&self) -> bool {
        self.inner.is_absolute()
    }

    #[inline(always)]
    pub fn is_relative(&self) -> bool {
        !self.is_absolute()
    }

    #[inline(always)]
    pub fn has_root(&self) -> bool {
        self.inner.has_root()
    }

    #[inline(always)]
    pub fn parent(&self) -> Option<&Path> {
        unsafe { std::mem::transmute(self.inner.parent()) }
    }

    #[inline(always)]
    pub fn ancestors(&self) -> Ancestors<'_> {
        unsafe { std::mem::transmute(self.inner.ancestors()) }
    }

    #[inline(always)]
    pub fn file_name(&self) -> Option<&OsStr> {
        self.inner.file_name()
    }

    #[inline(always)]
    pub fn strip_prefix<P>(&self, base: P) -> Result<&Path, StripPrefixError>
    where
        P: AsRef<Path>,
    {
        unsafe { std::mem::transmute(self.inner.strip_prefix(&std::convert::AsRef::as_ref(&base).inner)) }
    }

    #[inline(always)]
    pub fn starts_with<P: AsRef<Path>>(&self, base: P) -> bool {
        self.inner.starts_with(&std::convert::AsRef::as_ref(&base).inner)
    }

    #[inline(always)]
    pub fn ends_with<P: AsRef<Path>>(&self, child: P) -> bool {
        self.inner.ends_with(&std::convert::AsRef::as_ref(&child).inner)
    }

    #[inline(always)]
    pub fn file_stem(&self) -> Option<&OsStr> {
        self.inner.file_stem()
    }

    #[inline(always)]
    pub fn extension(&self) -> Option<&OsStr> {
        self.inner.extension()
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn join<P: AsRef<Path>>(&self, path: P) -> PathBuf {
        transmute!(self.inner.join(&<P as AsRef<Path>>::as_ref(&path).inner))
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn with_file_name<S: AsRef<OsStr>>(&self, file_name: S) -> PathBuf {
        unsafe { std::mem::transmute(self.inner.with_file_name(std::convert::AsRef::as_ref(&file_name))) }
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn with_extension<S: AsRef<OsStr>>(&self, extension: S) -> PathBuf {
        unsafe { std::mem::transmute(self.inner.with_extension(std::convert::AsRef::as_ref(&extension))) }
    }

    #[inline(always)]
    pub fn components(&self) -> Components<'_> {
        unsafe { std::mem::transmute(self.inner.components()) }
    }

    #[inline(always)]
    pub fn iter(&self) -> Iter<'_> {
        unsafe { std::mem::transmute(self.inner.iter()) }
    }

    #[inline(always)]
    pub fn display(&self) -> Display<'_> {
        unsafe { std::mem::transmute(self.inner.display()) }
    }

    #[inline(always)]
    pub fn metadata(&self) -> io::Result<fs::Metadata> {
        unsafe { std::mem::transmute(self.inner.metadata()) }
    }

    #[inline(always)]
    pub fn symlink_metadata(&self) -> io::Result<fs::Metadata> {
        unsafe { std::mem::transmute(self.inner.symlink_metadata()) }
    }

    #[inline(always)]
    pub fn canonicalize(&self) -> io::Result<PathBuf> {
        unsafe { std::mem::transmute(self.inner.canonicalize()) }
    }

    #[inline(always)]
    pub fn read_link(&self) -> io::Result<PathBuf> {
        unsafe { std::mem::transmute(self.inner.read_link()) }
    }

    #[inline(always)]
    pub fn read_dir(&self) -> io::Result<fs::ReadDir> {
        unsafe { std::mem::transmute(self.inner.read_dir()) }
    }

    #[inline(always)]
    pub fn exists(&self) -> bool {
        self.inner.exists()
    }

    #[inline(always)]
    pub fn try_exists(&self) -> io::Result<bool> {
        transmute!(self.inner.try_exists())
    }

    #[inline(always)]
    pub fn is_file(&self) -> bool {
        self.inner.is_file()
    }

    #[must_use]
    pub fn is_dir(&self) -> bool {
        self.inner.is_dir()
    }

    #[inline(always)]
    pub fn is_symlink(&self) -> bool {
        self.inner.is_symlink()
    }

    #[inline(always)]
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

impl std::convert::AsRef<Path> for Path {
    fn as_ref(&self) -> &Path {
        self
    }
}

impl std::convert::AsRef<Path> for PathBuf {
    fn as_ref(&self) -> &Path {
        self
    }
}

impl std::convert::AsRef<OsStr> for Path {
    #[inline(always)]
    fn as_ref(&self) -> &OsStr {
        <path::Path as std::convert::AsRef<OsStr>>::as_ref(&self.inner)
    }
}

impl std::convert::AsRef<OsStr> for PathBuf {
    #[inline(always)]
    fn as_ref(&self) -> &OsStr {
        <path::Path as std::convert::AsRef<OsStr>>::as_ref(&self.inner)
    }
}

impl std::convert::AsRef<Path> for OsStr {
    #[inline]
    fn as_ref(&self) -> &Path {
        transmute!(self)
    }
}

impl std::convert::AsRef<Path> for str {
    #[inline(always)]
    fn as_ref(&self) -> &Path {
        // TODO: Check if this works.
        transmute!(self)
    }
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl std::convert::AsRef<Path> for String {
    #[inline(always)]
    fn as_ref(&self) -> &Path {
        transmute!(self.as_str())
    }
}

impl PartialEq for Path {
    #[inline(always)]
    fn eq(&self, other: &Path) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl Eq for Path {}

impl PartialOrd for Path {
    #[inline(always)]
    fn partial_cmp(&self, other: &Path) -> Option<cmp::Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}

impl Ord for Path {
    #[inline(always)]
    fn cmp(&self, other: &Path) -> cmp::Ordering {
        self.inner.cmp(&other.inner)
    }
}
