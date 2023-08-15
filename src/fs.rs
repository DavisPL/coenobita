use crate::{ Capability, Create, View, Read, Write, Append, Copy, Move, Delete };

use std::marker::PhantomData;
use std::{ path, fs, io };

// Provides capability-safe wrapper for files with permissions passed as generic type arguments
// As before, permissions that aren't granted should be represented as the unit type ()

// A -> Create
// B -> View
// C -> Read
// D -> Write

#[derive(Debug)]
pub struct File<A, B, C, D> {
    file: fs::File,
    phantom: PhantomData<(A, B, C, D)>
}

impl File<(), (), (), ()> {
    pub fn open<A, B, C, D, E, F, G, H>
    (cap: &Capability<(A, B, C, D, E, F, G, H), (), ()>) -> io::Result<File<A, B, C, D>> {
        fs::OpenOptions::new()
            .read(true)
            .write(true)
            .append(true)
            .open(cap.get_path())
            .map(|file| File::<A, B, C, D> {
                file,
                phantom: PhantomData::<(A, B, C, D)>
            })
    }

    pub fn create<Create, B, C, D, E, F, G, H>
    (cap: &Capability<(Create, B, C, D, E, F, G, H), (), ()>) -> io::Result<File<Create, B, C, D>> {
        fs::OpenOptions::new()
            .read(true)
            .write(true)
            .append(true)
            .create(true)
            .open(cap.get_path())
            .map(|file| File::<Create, B, C, D> {
                file,
                phantom: PhantomData::<(Create, B, C, D)>
            })
    }

    pub fn create_new<Create, B, C, D, E, F, G, H>
    (cap: &Capability<(Create, B, C, D, E, F, G, H), (), ()>) -> io::Result<File<Create, B, C, D>> {
        fs::OpenOptions::new()
            .read(true)
            .write(true)
            .append(true)
            .create_new(true)
            .open(cap.get_path())
            .map(|file| File::<Create, B, C, D> {
                file,
                phantom: PhantomData::<(Create, B, C, D)>
            })
    }
}

impl<A, B, C> File<A, View, B, C> {
    // Queries metadata about the underlying file
    pub fn metadata(&self) -> io::Result<fs::Metadata> {
        self.file.metadata()
    }
}

impl<A, B, C> io::Read for File<A, B, Read, C> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.file.read(buf)
    }

    fn read_vectored(&mut self, bufs: &mut [io::IoSliceMut<'_>]) -> io::Result<usize> {
        self.file.read_vectored(bufs)
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        self.file.read_to_end(buf)
    }

    fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
        self.file.read_to_string(buf)
    }
}

impl<A, B, C> io::Write for File<A, B, C, Write> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.file.write(buf)
    }

    fn write_vectored(&mut self, bufs: &[io::IoSlice<'_>]) -> io::Result<usize> {
        self.file.write_vectored(bufs)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.file.flush()
    }
}

impl<A, B, C, D> io::Seek for File<A, B, C, D> {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        self.file.seek(pos)
    }
}

pub fn canonicalize<A, B, C>
(cap: &Capability<A, B, C>) -> io::Result<path::PathBuf> {
    fs::canonicalize(cap.get_path())
}

pub fn copy<A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R>
(from: &Capability<(A, B, C, D, E, Copy, F, G), H, I>, to: &Capability<(Create, J, K, L, M, N, O, P), Q, R>) -> io::Result<u64> {
    fs::copy(from.get_path(), to.get_path())
}

pub fn create_dir<A, B, C, D, E, F, G, H, I>
(cap: &Capability<(Create, A, B, C, D, E, F, G), H, I>) -> io::Result<()> {
    fs::create_dir(cap.get_path())
}

pub fn create_dir_all<A, B, C, D, E, F, G, H, I>
(cap: &Capability<(Create, A, B, C, D, E, F, G), H, I>) -> io::Result<()> {
    fs::create_dir_all(cap.get_path())
}

pub fn hard_link<A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R>
(original: &Capability<(A, View, B, C, D, E, F, G), H, I>, link: &Capability<(Create, J, K, L, M, N, O, P), Q, R>) -> io::Result<()> {
    panic!("[Coenobita] [ERROR] Hard linking is not supported yet.");
}

pub fn metadata<A, B, C, D, E, F, G, H, I>
(cap: &Capability<(A, View, B, C, D, E, F, G), H, I>) -> io::Result<fs::Metadata> {
    fs::metadata(cap.get_path())
}

pub fn read<A, B, C, D, E, F, G, H, I>
(cap: &Capability<(A, B, Read, C, D, E, F, G), H, I>) -> io::Result<Vec<u8>> {
    fs::read(cap.get_path())
}

pub fn read_dir<A, B, C, D, E, F, G, H, I>
(cap: &Capability<(A, B, Read, C, D, E, F, G), H, I>) -> io::Result<fs::ReadDir> {
    fs::read_dir(cap.get_path())
}

pub fn read_link<A, B, C, D, E, F, G, H, I>
(cap: &Capability<(A, B, Read, C, D, E, F, G), H, I>) -> io::Result<path::PathBuf> {
    fs::read_link(cap.get_path())
}

pub fn read_to_string<A, B, C, D, E, F, G, H, I>
(cap: &Capability<(A, B, Read, C, D, E, F, G), H, I>) -> io::Result<String> {
    fs::read_to_string(cap.get_path())
}

pub fn remove_dir<A, B, C, D, E, F, G, H, I>
(cap: &Capability<(A, B, C, D, E, F, G, Delete), H, I>) -> io::Result<()> {
    fs::remove_dir(cap.get_path())
}

pub fn remove_dir_all<A, B, C, D, E, F, G, H, I>
(cap: &Capability<(A, B, C, D, E, F, G, Delete), H, I>) -> io::Result<()> {
    fs::remove_dir_all(cap.get_path())
}

pub fn remove_file<A, B, C, D, E, F, G, H, I>
(cap: &Capability<(A, B, C, D, E, F, G, Delete), H, I>) -> io::Result<()> {
    fs::remove_file(cap.get_path())
}

pub fn rename<A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q>
(from: &Capability<(A, B, C, D, E, Copy, F, Delete), F, H>, to: &Capability<(Create, I, J, K, L, M, N, O), P, Q>) -> io::Result<()> {
    fs::rename(from.get_path(), to.get_path())
}

pub fn set_permissions<A, B>
(cap: &Capability<(Create, View, Read, Write, Append, Copy, Move, Delete), A, B>, perm: fs::Permissions) -> io::Result<()> {
    fs::set_permissions(cap.get_path(), perm)
}

pub fn symlink_metadata<A, B, C, D, E, F, G, H, I>
(cap: &Capability<(A, View, B, C, D, E, F, G), H, I>) -> io::Result<fs::Metadata> {
    fs::symlink_metadata(cap.get_path())
}

pub fn write<A, B, C, D, E, F, G, H, I, J: AsRef<[u8]>>
(cap: &Capability<(A, B, C, Write, D, E, F, G), H, I>, contents: J) -> io::Result<()> {
    fs::write(cap.get_path(), contents)
}
