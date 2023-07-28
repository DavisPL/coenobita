use crate::{ Capability, Read, Write, Copy, Move, Delete, NotGranted };

use std::marker::PhantomData;
use std::{ path, fs };
use std::io;

// File wrapper
#[derive(Debug)]
pub struct File<A, B> {
    file: fs::File,
    phantom: PhantomData<(A, B)>
}

// File methods
impl File<(), ()> {
    // Opens file in write-only mode
    pub fn create<A, B, C, D>(cap: &Capability<A, Write, B, C, D>) -> io::Result<File<(), Write>> {
        match fs::File::create(cap.get_path()) {
            Ok(file) => Ok(File {
                file: file,
                phantom: PhantomData::<((), Write)>
            }),

            Err(error) => Err(error)
        }
    }

    pub fn open<A, B, C, D>(cap: &Capability<Read, A, B, C, D>) -> io::Result<File<Read, ()>> {
        match fs::File::open(cap.get_path()) {
            Ok(file) => Ok(File {
                file: file,
                phantom: PhantomData::<(Read, ())>
            }),

            Err(error) => Err(error)
        }
    }
}

// Implementations for any methods that require at least read permissions
impl<A> File<Read, A> {
    // Queries metadata about the underlying file
    pub fn metadata(&self) -> io::Result<fs::Metadata> {
        self.file.metadata()
    }
}

// TRAIT IMPLEMENTATION | Read
impl<A> io::Read for File<Read, A> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.file.read(buf)
    }
}

pub fn canonicalize<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<path::PathBuf> {
    fs::canonicalize(cap.get_path())
}

pub fn copy<A, B, C, D, E, F, G, H>
(from: &Capability<A, B, Copy, C, D>, to: &Capability<E, Write, F, G, H>) -> io::Result<u64> {
    fs::copy(from.get_path(), to.get_path())
}

pub fn create_dir<A, B, C, D>
(cap: &Capability<A, Write, B, C, D>) -> io::Result<()> {
    fs::create_dir(cap.get_path())
}

pub fn create_dir_all<A, B, C, D, E, F, G, H>
(cap: &Capability<A, Write, B, C, D>) -> io::Result<()> {
    fs::create_dir_all(cap.get_path())
}

pub fn hard_link<A, B, C, D, E, F, G, H>
(original: &Capability<Read, A, B, C, D>, link: &Capability<E, Write, F, G, H>) -> io::Result<()> {
    panic!("[Coenobita] [ERROR] Hard linking is not properly supported yet.");
}

pub fn metadata<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<fs::Metadata> {
    fs::metadata(cap.get_path())
}

pub fn read<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<Vec<u8>> {
    fs::read(cap.get_path())
}

pub fn read_dir<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<fs::ReadDir> {
    fs::read_dir(cap.get_path())
}

pub fn read_link<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<path::PathBuf> {
    fs::read_link(cap.get_path())
}

pub fn read_to_string<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<String> {
    fs::read_to_string(cap.get_path())
}

pub fn remove_dir<A, B, C, D>
(cap: &Capability<A, B, C, D, Delete>) -> io::Result<()> {
    fs::remove_dir(cap.get_path())
}

pub fn remove_dir_all<A, B, C, D>
(cap: &Capability<A, B, C, D, Delete>) -> io::Result<()> {
    fs::remove_dir_all(cap.get_path())
}

pub fn remove_file<A, B, C, D>
(cap: &Capability<A, B, C, D, Delete>) -> io::Result<()> {
    fs::remove_file(cap.get_path())
}

pub fn rename<A, B, C, D, E, F, G, H>
(from: &Capability<A, B, C, Move, D>, to: &Capability<E, F, G, Move, H>) -> io::Result<()> {
    fs::rename(from.get_path(), to.get_path())
}

pub fn set_permissions
(cap: &Capability<Read, Write, Copy, Move, Delete>, perm: fs::Permissions) -> io::Result<()> {
    fs::set_permissions(cap.get_path(), perm)
}

pub fn symlink_metadata<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<fs::Metadata> {
    fs::symlink_metadata(cap.get_path())
}

pub fn write<A, B, C, D, E: AsRef<[u8]>>
(cap: &Capability<A, Write, B, C, D>, contents: E) -> io::Result<()> {
    fs::write(cap.get_path(), contents)
}
