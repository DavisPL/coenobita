pub use macros::{ cap, dir };
pub mod fs;

use std::marker::PhantomData;
use std::path::{ Path, PathBuf };

#[derive(Debug)]
pub struct Read;

#[derive(Debug)]
pub struct Write;

#[derive(Debug)]
pub struct Copy;

#[derive(Debug)]
pub struct Move;

#[derive(Debug)]
pub struct Delete;

// Provides capability safe wrapper for PathBuf with A, B, C, D, and E representing Read,
// Write, Copy, Move, and Delete respectively.
#[derive(Debug)]
pub struct Capability<A, B, C, D, E> {
    path: PathBuf,
    phantom: PhantomData<(A, B, C, D, E)>,
}

#[derive(Debug)]
pub struct Directory<A, B> {
    path: PathBuf,
    phantom: PhantomData<(A, B)>
}

// Implements methods for Capabilities with any permissions
impl<A, B, C, D, E> Capability<A, B, C, D, E> {
    pub fn new<P: AsRef<Path>>(path: P) -> Capability<A, B, C, D, E> {
        Capability {
            path: path.as_ref().to_path_buf(),
            phantom: PhantomData::<(A, B, C, D, E)>
        }
    }

    // Path must have a getter because it's private - programmers shouldn't be able to change it
    pub fn get_path(&self) -> &PathBuf {
        &self.path
    }
}

impl<A, B, C, D, E, F, G, H, I, J> Directory<Capability<A, B, C, D, E>, Capability<F, G, H, I, J>> {
    pub fn new(path_string: &str) -> Directory<Capability<A, B, C, D, E>, Capability<F, G, H, I, J>> {
        Directory {
            path: PathBuf::from(path_string),
            phantom: PhantomData::<(Capability<A, B, C, D, E>, Capability<F, G, H, I, J>)>
        }
    }

    // Path must have a getter because it's private - programmers shouldn't be able to change it
    pub fn get_path(&self) -> &PathBuf {
        &self.path
    }
}

impl<A> Directory<A, ()> {
    pub fn from<B, C, D, E, F>
    (cap: Capability<B, C, D, E, F>) -> Directory<A, Capability<B, C, D, E, F>> {
        Directory {
            path: cap.get_path().to_path_buf(),
            phantom: PhantomData::<(A, Capability<B, C, D, E, F>)>
        }
    }
}
