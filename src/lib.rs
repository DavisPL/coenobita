pub use macros::{ cap, dir };
pub mod fs;

use std::marker::PhantomData;
use std::path::PathBuf;

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
pub struct Capability<A, B, C, D, E> {
    path: PathBuf,
    phantom: PhantomData<(A, B, C, D, E)>,
}

pub struct Directory<A, B> {
    path: PathBuf,
    phantom: PhantomData<(A, B)>
}

// Implements methods for Capabilities with any permissions
impl<A, B, C, D, E> Capability<A, B, C, D, E> {
    pub fn new(path_string: &str) -> Capability<A, B, C, D, E> {
        Capability {
            path: PathBuf::from(path_string),
            phantom: PhantomData::<(A, B, C, D, E)>
        }
    }

    // Path must have a getter because it's private - programmers shouldn't be able to change it
    pub fn get_path(&self) -> &PathBuf {
        &self.path
    }
}

impl<A, B, C, D, E, F, G, H, I, J> Directory<Capability<A, B, C, D, E>, Capability<F, G, H, I, J>> {
    // Path must have a getter because it's private - programmers shouldn't be able to change it
    pub fn get_path(&self) -> &PathBuf {
        &self.path
    }
}

impl<A, B, C, D, E, F> From<Capability<A, B, C, D, E>> for Directory<F, Capability<A, B, C, D, E>> {
    fn from(value: Capability<A, B, C, D, E>) -> Directory<F, Capability<A, B, C, D, E>> {
        Directory::<F, Capability::<A, B, C, D, E>> {
            path: value.get_path().to_path_buf(),
            phantom: PhantomData::<(F, Capability<A, B, C, D, E>)>
        }
    }
}
