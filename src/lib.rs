pub mod fs;
pub use macros::{ cap, dir };

use std::marker::PhantomData;
use std::path::{ Path, PathBuf };

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
pub struct Capability<A, B, C> {
    path: PathBuf,
    phantom: PhantomData<(A, B, C)>,
}

// Implements methods for Capabilities with any permissions
impl<A, B, C> Capability<A, B, C> {
    pub fn new<P: AsRef<Path>>(path: P) -> Capability<A, B, C> {
        Capability {
            path: path.as_ref().to_path_buf(),
            phantom: PhantomData::<(A, B, C)>
        }
    }

    pub fn get_path(&self) -> &PathBuf {
        &self.path
    }
}
