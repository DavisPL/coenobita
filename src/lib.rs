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

// Provides capability-safe wrapper for file paths with permissions...
// A -> Create
// B -> View
// C -> Read
// D -> Write
// E -> Append
// F -> Copy
// G -> Move
// H -> Delete

#[derive(Debug)]
pub struct FileCapability<A, B, C, D, E, F, G, H> {
    path: PathBuf,
    phantom: PhantomData<(A, B, C, D, E, F, G, H)>,
}

// Provides capability-safe wrapper for directory with permissions...
// A -> Direct on Parent (in the form of a capability)
// B -> Intransitive on Descendants (in the form of a capability)
// C -> Transitive on Descendants (in the form of a capability)

// Direct permissions refer to permissions that only apply to the "container" - if the directory is
// represented by an imaginary box, direct permissions only apply to the box and not the items inside

// Intransitive permissions refer to permissons that ONLY apply to the directory's direct
// children, which could be files or directories, without further inheritance

// Transitive permissions refer to permissions that are inherited by all of the parent directory's
// descendants in a cascading manner

#[derive(Debug)]
pub struct DirectoryCapability<A, B, C> {
    path: PathBuf,
    phantom: PhantomData<(A, B, C)>
}

// Implements methods for Capabilities with any permissions
impl<A, B, C, D, E, F, G, H> FileCapability<A, B, C, D, E, F, G, H> {
    pub fn new<P: AsRef<Path>>(path: P) -> FileCapability<A, B, C, D, E, F, G, H> {
        FileCapability {
            path: path.as_ref().to_path_buf(),
            phantom: PhantomData::<(A, B, C, D, E, F, G, H)>
        }
    }

    pub fn get_path(&self) -> &PathBuf {
        &self.path
    }
}

impl<A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X> DirectoryCapability<FileCapability<A, B, C, D, E, F, G, H>, FileCapability<I, J, K, L, M, N, O, P>, FileCapability<Q, R, S, T, U, V, W, X>> {
    pub fn new(path_string: &str) -> DirectoryCapability<FileCapability<A, B, C, D, E, F, G, H>, FileCapability<I, J, K, L, M, N, O, P>, FileCapability<Q, R, S, T, U, V, W, X>> {
        Directory {
            path: PathBuf::from(path_string),
            phantom: PhantomData::<(FileCapability<A, B, C, D, E, F, G, H>, FileCapability<I, J, K, L, M, N, O, P>, FileCapability<Q, R, S, T, U, V, W, X>)>
        }
    }

    // Path must have a getter because it's private - programmers shouldn't be able to change it
    pub fn get_path(&self) -> &PathBuf {
        &self.path
    }
}

impl<A> DirectoryCapability<A, ()> {
    pub fn from<B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q>
    (intransitive: &FileCapability<B, C, D, E, F, G, H, I>, transitive: &FileCapability<J, K, L, M, N, O, P, Q>) -> DirectoryCapability<A, FileCapability<B, C, D, E, F, G, H, I>, FileCapability<J, K, L, M, N, O, P, Q>> {
        DirectoryCapability {
            path: cap.get_path().to_path_buf(),
            phantom: PhantomData::<(A, FileCapability<B, C, D, E, F, G, H, I>, FileCapability<J, K, L, M, N, O, P, Q>)>
        }
    }
}
