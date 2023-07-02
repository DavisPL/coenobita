pub use macros::cap;
pub mod fs;

use std::marker::PhantomData;
use std::path::PathBuf;

pub struct Read;
pub struct Write;
pub struct Copy;
pub struct Move;
pub struct Delete;
pub struct NotGranted;

// Provides capability safe wrapper for PathBuf with A, B, C, D, and E representing Read,
// Write, Copy, Move, and Delete respectively.
pub struct Capability<A, B, C, D, E> {
    path: PathBuf,
    phantom: PhantomData<(A, B, C, D, E)>,
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

