pub mod fs;
pub use macros::{ cap };

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

pub mod traits {
    use std::path::PathBuf;

    pub trait Capability {
        fn get_path(&self) -> &PathBuf;
    }

    pub trait Create: Capability {}
    pub trait View: Capability {}
    pub trait Read: Capability {}
    pub trait Write: Capability {}
    pub trait Append: Capability {}
    pub trait Copy: Capability {}
    pub trait Move: Capability {}
    pub trait Delete: Capability {}
}

impl<A, B, C> traits::Capability for Capability<A, B, C> {
    fn get_path(&self) -> &PathBuf {
        &self.path
    }
}

// Implement each trait for its corresponding Capability<A, B, C>
impl<A2, A3, A4, A5, A6, A7, A8, B, C>
   traits::Create for Capability<(Create, A2, A3, A4, A5, A6, A7, A8), B, C> {}

impl<A1, A3, A4, A5, A6, A7, A8, B, C>
    traits::View for Capability<(A1, View, A3, A4, A5, A6, A7, A8), B, C> {}

impl<A1, A2, A4, A5, A6, A7, A8, B, C>
    traits::Read for Capability<(A1, A2, Read, A4, A5, A6, A7, A8), B, C> {}

impl<A1, A2, A3, Write, A5, A6, A7, A8, B, C>
    traits::Write for Capability<(A1, A2, A3, Write, A5, A6, A7, A8), B, C> {}

impl<A1, A2, A3, A4, A6, A7, A8, B, C>
    traits::Append for Capability<(A1, A2, A3, A4, Append, A6, A7, A8), B, C> {}

impl<A1, A2, A3, A4, A5, A7, A8, B, C>
    traits::Copy for Capability<(A1, A2, A3, A4, A5, Copy, A7, A8), B, C> {}

impl<A1, A2, A3, A4, A5, A6, A8, B, C>
    traits::Move for Capability<(A1, A2, A3, A4, A5, A6, Move, A8), B, C> {}

impl<A1, A2, A3, A4, A5, A6, A7, B, C>
    traits::Delete for Capability<(A1, A2, A3, A4, A5, A6, A7, Delete), B, C> {}

