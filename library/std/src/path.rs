use std::hash::{Hash, Hasher};
use std::ops::DerefMut;
pub use std::path::{self, *};

use crate::borrow::{Borrow, Cow};
use crate::cmp;
use crate::collections::TryReserveError;
use crate::convert;
use crate::ffi::{OsStr, OsString};
use crate::fmt;
use crate::fs;
use crate::io;
use crate::ops::Deref;
use crate::transmute;

pub struct PathBuf {
    inner: path::PathBuf,
}

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
        self.inner.push(&path.as_ref().inner);
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root)) -> (*,root))]
    pub fn pop(&mut self) -> bool {
        self.inner.pop()
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn set_file_name<S: AsRef<OsStr>>(&mut self, file_name: S) {
        self.inner.set_file_name(file_name.as_ref());
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn set_extension<S: AsRef<OsStr>>(&mut self, extension: S) -> bool {
        self.inner.set_extension(extension.as_ref())
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

impl crate::hash::Hash for PathBuf {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.inner.hash(h);
    }
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

impl Deref for PathBuf {
    type Target = Path;

    #[inline(always)]
    fn deref(&self) -> &Path {
        unsafe { &*(self.inner.as_ref() as *const OsStr as *const Path) }
    }
}

impl DerefMut for PathBuf {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Path {
        Path::from_inner_mut( self.inner.as_mut_os_str())
    }
}

impl Borrow<Path> for PathBuf {
    #[inline(always)]
    fn borrow(&self) -> &Path {
        self.deref()
    }
}

impl Clone for PathBuf {
    #[inline(always)]
    fn clone(&self) -> Self {
        transmute!(self.inner.clone())
    }

    #[inline(always)]
    fn clone_from(&mut self, source: &Self) {
        self.inner.clone_from(&source.inner)
    }
}

impl fmt::Debug for PathBuf {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

pub struct Path {
    pub(crate) inner: path::Path,
}

impl Path {
    #[cnbt::integrity({root}{root} fn({root}{root}) -> {*}{*})]
    #[cnbt::provenance((*,*) fn((*,root)) -> (*,root))]
    #[inline(always)]
    pub fn new<S: AsRef<OsStr> + ?Sized>(s: &S) -> &Path {
        unsafe { &*(s.as_ref() as *const OsStr as *const Path) }
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
        transmute!(self.inner.to_path_buf())
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
        transmute!(self.inner.parent())
    }

    #[inline(always)]
    pub fn ancestors(&self) -> Ancestors<'_> {
        transmute!(self.inner.ancestors())
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
        transmute!(self.inner.strip_prefix(&base.as_ref().inner))
    }

    #[inline(always)]
    pub fn starts_with<P: AsRef<Path>>(&self, base: P) -> bool {
        self.inner.starts_with(&base.as_ref().inner)
    }

    #[inline(always)]
    pub fn ends_with<P: AsRef<Path>>(&self, child: P) -> bool {
        self.inner.ends_with(&child.as_ref().inner)
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
        transmute!(self.inner.join(&path.as_ref().inner))
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn with_file_name<S: AsRef<OsStr>>(&self, file_name: S) -> PathBuf {
        transmute!(self.inner.with_file_name(file_name.as_ref()))
    }

    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*,root), (*,root)) -> (*,root))]
    pub fn with_extension<S: AsRef<OsStr>>(&self, extension: S) -> PathBuf {
        transmute!(self.inner.with_extension(extension.as_ref()))
    }

    #[inline(always)]
    pub fn components(&self) -> Components<'_> {
        transmute!(self.inner.components())
    }

    #[inline(always)]
    pub fn iter(&self) -> Iter<'_> {
        transmute!(self.inner.iter())
    }

    #[inline(always)]
    pub fn display(&self) -> Display<'_> {
        transmute!(self.inner.display())
    }

    #[inline(always)]
    pub fn metadata(&self) -> io::Result<fs::Metadata> {
        transmute!(self.inner.metadata())
    }

    #[inline(always)]
    pub fn symlink_metadata(&self) -> io::Result<fs::Metadata> {
        transmute!(self.inner.symlink_metadata())
    }

    #[inline(always)]
    pub fn canonicalize(&self) -> io::Result<PathBuf> {
        transmute!(self.inner.canonicalize())
    }

    #[inline(always)]
    pub fn read_link(&self) -> io::Result<PathBuf> {
        transmute!(self.inner.read_link())
    }

    #[inline(always)]
    pub fn read_dir(&self) -> io::Result<fs::ReadDir> {
        transmute!(self.inner.read_dir())
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

    #[inline(always)]
    pub fn is_dir(&self) -> bool {
        self.inner.is_dir()
    }

    #[inline(always)]
    pub fn is_symlink(&self) -> bool {
        self.inner.is_symlink()
    }

    #[inline(always)]
    pub fn into_path_buf(self: Box<Path>) -> PathBuf {
        self.to_path_buf()
    }

    #[inline(always)]
    fn from_inner_mut(inner: &mut OsStr) -> &mut Path {
        unsafe { &mut *(inner as *mut OsStr as *mut Path) }
    }
}

impl ToOwned for Path {
    type Owned = PathBuf;

    #[inline(always)]
    fn to_owned(&self) -> PathBuf {
        self.to_path_buf()
    }

    #[inline(always)]
    fn clone_into(&self, target: &mut PathBuf) {
        self.inner.clone_into(&mut target.inner);
    }
}

impl AsRef<Path> for Path {
    #[inline(always)]
    fn as_ref(&self) -> &Path {
        self
    }
}

impl AsRef<Path> for PathBuf {
    #[inline(always)]
    fn as_ref(&self) -> &Path {
        self
    }
}

impl AsRef<OsStr> for Path {
    #[inline(always)]
    fn as_ref(&self) -> &OsStr {
        self.inner.as_ref()
    }
}

impl AsRef<OsStr> for PathBuf {
    #[inline(always)]
    fn as_ref(&self) -> &OsStr {
        self.inner.as_ref()
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

impl fmt::Debug for Path {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
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

impl<T: ?Sized + AsRef<OsStr>> From<&T> for PathBuf {
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

impl From<Box<Path>> for PathBuf {
    #[inline(always)]
    fn from(boxed: Box<Path>) -> PathBuf {
        boxed.into_path_buf()
    }
}

impl From<PathBuf> for Box<Path> {
    #[inline(always)]
    fn from(p: PathBuf) -> Box<Path> {
        p.into_boxed_path()
    }
}
