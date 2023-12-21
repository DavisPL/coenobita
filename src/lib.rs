pub mod fs;
pub use macros::cap;

use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::io;
use std::iter::FusedIterator;
use std::marker::PhantomData;
use std::ops;
use std::path::{Components, Display, Iter, Path, PathBuf, StripPrefixError};

#[derive(Debug)]
pub struct Create;

#[derive(Debug)]
pub struct View;

#[derive(Debug)]
pub struct Read;

#[derive(Debug)]
pub struct Write;

#[derive(Debug)]
pub struct Append;

#[derive(Debug)]
pub struct Copy;

#[derive(Debug)]
pub struct Move;

#[derive(Debug)]
pub struct Delete;

// Provides capability-safe wrapper for PathBuf, applies to both directories and files
// Each generic type parameter represents different types of permissions

// A -> Direct permissions on the object only
// B -> Intransitive permissions on the object's descendants
// C -> Transitive permissions on the object's descendants

// Direct permissions refer to permissions that only apply to the "container" - if the directory is
// represented by an imaginary box, direct permissions only apply to the box and not the items inside

// Intransitive permissions refer to permissons that ONLY apply to the directory's direct
// children, which could be files or directories, without further inheritance

// Transitive permissions refer to permissions that are inherited by all of the parent directory's
// descendants in a cascading manner

// Files have no children so B and C aren't necessary, but programmers may wish to define them for directories

// Each A, B, C should be passed as a tuple of permissions in the following order OR the unit
// type () if they aren't granted - Create, View, Read, Write, Append, Copy, Move, Delete

#[derive(Debug)]
pub struct CapBuf<A, B, C> {
    path: PathBuf,
    phantom: PhantomData<(A, B, C)>,
}

#[derive(Debug)]
pub struct Cap<A, B, C> {
    phantom: PhantomData<(A, B, C)>,
    path: Path,
}

impl<A, B, C> ops::Deref for CapBuf<A, B, C> {
    type Target = Cap<A, B, C>;

    #[inline]
    fn deref(&self) -> &Cap<A, B, C> {
        Cap::new(&self.path)
    }
}

impl<A, B, C> AsRef<Cap<A, B, C>> for Cap<A, B, C> {
    #[inline]
    fn as_ref(&self) -> &Cap<A, B, C> {
        self
    }
}

impl<A, B, C> AsRef<Cap<A, B, C>> for CapBuf<A, B, C> {
    #[inline]
    fn as_ref(&self) -> &Cap<A, B, C> {
        self
    }
}

#[derive(Debug)]
pub struct Ancestors<'a, A, B, C> {
    next: Option<&'a Cap<A, B, C>>,
}

impl<'a, A, B, C> Iterator for Ancestors<'a, A, B, C> {
    type Item = &'a Cap<A, B, C>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next;
        self.next = next.and_then(Cap::<A, B, C>::parent);
        next
    }
}

// NOTE - Currently not used anywhere AFAIK, may be unnecessary
impl<A, B, C> FusedIterator for Ancestors<'_, A, B, C> {}

// These are all implemented for Path
impl<A, B, C> Cap<A, B, C> {
    pub fn new<S: AsRef<OsStr> + ?Sized>(s: &S) -> &Cap<A, B, C> {
        unsafe { &*(s.as_ref() as *const OsStr as *const Cap<A, B, C>) }
    }

    pub fn as_os_str(&self) -> &OsStr {
        self.path.as_os_str()
    }

    pub fn to_str(&self) -> Option<&str> {
        self.path.to_str()
    }

    pub fn to_string_lossy(&self) -> Cow<'_, str> {
        self.path.to_string_lossy()
    }

    // NOTE - This is equivalent to 'to_path_buf'
    pub fn to_cap_buf(&self) -> CapBuf<A, B, C> {
        CapBuf {
            // path: PathBuf::from(self.path.to_os_string()),
            path: self.path.to_path_buf(),
            phantom: PhantomData::<(A, B, C)>,
        }
    }

    pub fn is_absolute(&self) -> bool {
        self.path.is_absolute()
    }

    pub fn is_relative(&self) -> bool {
        self.path.is_relative()
    }

    pub fn has_root(&self) -> bool {
        self.path.has_root()
    }

    pub fn parent(&self) -> Option<&Cap<A, B, C>> {
        self.path.parent().map(|path| Cap::new(path))
    }

    pub fn ancestors(&self) -> Ancestors<'_, A, B, C> {
        Ancestors { next: Some(self) }
    }

    pub fn file_name(&self) -> Option<&OsStr> {
        self.path.file_name()
    }

    pub fn strip_prefix(&self, base: &Cap<A, B, C>) -> Result<&Cap<A, B, C>, StripPrefixError> {
        self.path
            .strip_prefix(base.to_path())
            .map(|path| Cap::new(path))
    }

    pub fn starts_with(&self, base: &Cap<A, B, C>) -> bool {
        self.path.starts_with(base.to_path())
    }

    pub fn ends_with(&self, base: &Cap<A, B, C>) -> bool {
        self.path.ends_with(base.to_path())
    }

    pub fn file_stem(&self) -> Option<&OsStr> {
        self.path.file_stem()
    }

    pub fn extension(&self) -> Option<&OsStr> {
        self.path.extension()
    }

    pub fn to_path(&self) -> &Path {
        &self.path
    }

    // TODO - Return to this one later...

    // It should be okay in its current form because it's just returning an iterator
    // that contains the internal path as a u8 slice and returns &OsStr values, but
    // it could be potentially useful to return CapBuf slice values as well. Is
    // that compatible with the object-capability model?

    pub fn components(&self) -> Components<'_> {
        self.path.components()
    }

    // NOTE - See above
    pub fn iter(&self) -> Iter<'_> {
        self.path.iter()
    }

    // NOTE - Should this display only the internal path, or associated
    // permissions as well?
    pub fn display(&self) -> Display<'_> {
        self.path.display()
    }
}

impl<A: traits::View, B, C> Cap<A, B, C> {
    pub fn metadata(&self) -> io::Result<fs::Metadata> {
        fs::metadata(self)
    }

    pub fn symlink_metadata(&self) -> io::Result<fs::Metadata> {
        fs::symlink_metadata(self)
    }

    pub fn canonicalize(&self) -> io::Result<CapBuf<A, B, C>> {
        fs::canonicalize(self)
    }

    pub fn read_link(&self) -> io::Result<CapBuf<A, B, C>> {
        fs::read_link(self)
    }

    pub fn exists(&self) -> bool {
        self.path.exists()
    }

    pub fn try_exists(&self) -> io::Result<bool> {
        self.path.try_exists()
    }

    pub fn is_file(&self) -> bool {
        self.path.is_file()
    }

    pub fn is_dir(&self) -> bool {
        self.path.is_dir()
    }

    pub fn is_symlink(&self) -> bool {
        self.path.is_symlink()
    }

    // NOTE - Corresponds to 'into_path_buf'

    // This _should_ convert a Box<Cap> into a CapBuf without
    // copying or allocating... in its current form, I don't think it does that

    /*pub fn into_cap(self: Box<Cap<A, B, C>) -> CapBuf<A, B, C> {
        /* let rw = Box::into_raw(self) as *mut OsStr;
         * let inner = unsafe { Box::from_raw(rw) };
         * PathBuf { inner: OsString::from(inner) }
         */

        CapBuf::new(self.path.into_path_buf())
    }*/
}

impl<A: traits::Read, B, C> Cap<A, B, C> {
    // TODO - Implement under different trait bound (maybe C: traits::View)
    pub fn read_dir(&self) -> io::Result<fs::ReadDir<A, B, C>> {
        fs::read_dir(self)
    }
}

// Implements methods for Capabilities with any permissions
impl<A, B, C> CapBuf<A, B, C> {
    pub fn new<P: AsRef<Path>>(path: P) -> CapBuf<A, B, C> {
        CapBuf {
            path: path.as_ref().to_path_buf(),
            phantom: PhantomData::<(A, B, C)>,
        }
    }

    pub fn display(&self) -> Display<'_> {
        self.to_path().display()
    }

    pub fn to_path(&self) -> &Path {
        &self.path
    }
}

pub mod traits {
    pub trait Create {}
    pub trait View {}
    pub trait Read {}
    pub trait Write {}
    pub trait Append {}
    pub trait Copy {}
    pub trait Move {}
    pub trait Delete {}
}

impl<P2, P3, P4, P5, P6, P7, P8> traits::Create for (Create, P2, P3, P4, P5, P6, P7, P8) {}
impl<P1, P3, P4, P5, P6, P7, P8> traits::View for (P1, View, P3, P4, P5, P6, P7, P8) {}
impl<P1, P2, P4, P5, P6, P7, P8> traits::Read for (P1, P2, Read, P4, P5, P6, P7, P8) {}
impl<P1, P2, P3, P5, P6, P7, P8> traits::Write for (P1, P2, P3, Write, P5, P6, P7, P8) {}
impl<P1, P2, P3, P4, P6, P7, P8> traits::Append for (P1, P2, P3, P4, Append, P6, P7, P8) {}
impl<P1, P2, P3, P4, P5, P7, P8> traits::Copy for (P1, P2, P3, P4, P5, Copy, P7, P8) {}
impl<P1, P2, P3, P4, P5, P6, P8> traits::Move for (P1, P2, P3, P4, P5, P6, Move, P8) {}
impl<P1, P2, P3, P4, P5, P6, P7> traits::Delete for (P1, P2, P3, P4, P5, P6, P7, Delete) {}

// TODO - Decide where each of these should go, and if it's compatable
// with the object-capability model
impl<A1, A2, A3> CapBuf<A1, A2, A3> {
    // NOTE - This function cannot exist anymore as path is a private field and
    // the getter 'to_path' only returns an immutable reference to the path
    pub fn as_mut_os_string(&mut self) -> &mut OsString {
        panic!("Unimplemented");
    }

    // NOTE - Should this coerce to a CapBuf slice? Something to consider...
    pub fn as_path(&self) -> &Path {
        panic!("Unimplemented");
    }

    // This one is okay though
    pub fn into_os_string(self) -> OsString {
        self.path.into_os_string()
    }
}

impl<A1: traits::View, A2, A3> CapBuf<A1, A2, A3> {
    pub fn file_name(&self) -> Option<&OsStr> {
        self.path.file_name()
    }

    pub fn as_os_str(&self) -> &OsStr {
        self.path.as_os_str()
    }
}

impl<A1, A2, A3> Clone for CapBuf<A1, A2, A3> {
    fn clone(&self) -> CapBuf<A1, A2, A3> {
        CapBuf {
            path: self.to_path().to_path_buf(),
            phantom: PhantomData::<(A1, A2, A3)>,
        }
    }
}
