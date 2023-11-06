#![allow(clippy::type_complexity)]

use crate::traits;
use crate::{Append, Capability, Copy, Create, Delete, Move, Read, View, Write};

use std::fmt;
use std::marker::PhantomData;
use std::os::unix::fs::{DirEntryExt, MetadataExt};
use std::time::SystemTime;
use std::{fs, io, path};

// Provides capability-safe wrapper for files with permissions passed as generic type arguments
// As before, permissions that aren't granted should be represented as the unit type ()

// A -> Create
// B -> View
// C -> Read
// D -> Write

#[derive(Debug)]
pub struct File<A, B, C, D> {
    file: fs::File,
    phantom: PhantomData<(A, B, C, D)>,
}

impl File<(), (), (), ()> {
    pub fn open<A, B, C, D, E, F, G, H>(
        cap: &Capability<
            (A, B, C, D, E, F, G, H),
            ((), (), (), (), (), (), (), ()),
            ((), (), (), (), (), (), (), ()),
        >,
    ) -> io::Result<File<A, B, C, D>> {
        fs::OpenOptions::new()
            .read(true)
            .write(true)
            .append(true)
            .open(cap.get_path())
            .map(|file| File::<A, B, C, D> {
                file,
                phantom: PhantomData::<(A, B, C, D)>,
            })
    }

    // NOTE - Should we be able to create directories this way?
    pub fn create<B, C, D, E, F, G, H>(
        cap: &Capability<
            (Create, B, C, D, E, F, G, H),
            ((), (), (), (), (), (), (), ()),
            ((), (), (), (), (), (), (), ()),
        >,
    ) -> io::Result<File<Create, B, C, D>> {
        fs::OpenOptions::new()
            .read(true)
            .write(true)
            .append(true)
            .create(true)
            .open(cap.get_path())
            .map(|file| File::<Create, B, C, D> {
                file,
                phantom: PhantomData::<(Create, B, C, D)>,
            })
    }

    // NOTE - Should we be able to create directories this way?
    pub fn create_new<B, C, D, E, F, G, H>(
        cap: &Capability<
            (Create, B, C, D, E, F, G, H),
            ((), (), (), (), (), (), (), ()),
            ((), (), (), (), (), (), (), ()),
        >,
    ) -> io::Result<File<Create, B, C, D>> {
        fs::OpenOptions::new()
            .read(true)
            .write(true)
            .append(true)
            .create_new(true)
            .open(cap.get_path())
            .map(|file| File::<Create, B, C, D> {
                file,
                phantom: PhantomData::<(Create, B, C, D)>,
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

pub fn canonicalize<A1, A2, A3>(cap: &Capability<A1, A2, A3>) -> io::Result<path::PathBuf> {
    fs::canonicalize(cap.get_path())
}

pub fn copy<A1, A2, A3, B1, B2, B3>(
    from: &Capability<A1, A2, A3>,
    to: &Capability<B1, B2, B3>,
) -> io::Result<u64>
where
    A1: traits::Copy,
    B1: traits::Create,
{
    fs::copy(from.get_path(), to.get_path())
}

pub fn create_dir<A1: traits::Create, A2, A3>(cap: &Capability<A1, A2, A3>) -> io::Result<()> {
    fs::create_dir(cap.get_path())
}

pub fn create_dir_all<A1: traits::Create, A2, A3>(cap: &Capability<A1, A2, A3>) -> io::Result<()> {
    fs::create_dir_all(cap.get_path())
}

pub fn hard_link<A1: traits::View, A2, A3, B1: traits::Create, B2, B3>(
    _original: &Capability<A1, A2, A3>,
    _link: &Capability<B1, B2, B3>,
) -> io::Result<()> {
    panic!("[Coenobita] [ERROR] Hard linking is not supported yet.");
}

pub fn metadata<A1: traits::View, A2, A3>(cap: &Capability<A1, A2, A3>) -> io::Result<Metadata> {
    Ok(Metadata(fs::metadata(cap.get_path())?))
}

pub fn read<A1: traits::Read, A2, A3>(cap: &Capability<A1, A2, A3>) -> io::Result<Vec<u8>> {
    fs::read(cap.get_path())
}

pub fn read_dir<A1: traits::Read, A2, A3>(
    cap: &Capability<A1, A2, A3>,
) -> io::Result<ReadDir<A1, A2, A3>> {
    fs::read_dir(cap.get_path()).map(|r| ReadDir {
        _read_dir: r,
        phantom: PhantomData::<(A1, A2, A3)>,
    })
}

pub fn read_link<A1: traits::Read, A2, A3>(
    cap: &Capability<A1, A2, A3>,
) -> io::Result<path::PathBuf> {
    fs::read_link(cap.get_path())
}

pub fn read_to_string<A1: traits::Read, A2, A3>(
    cap: &Capability<A1, A2, A3>,
) -> io::Result<String> {
    fs::read_to_string(cap.get_path())
}

pub fn remove_dir<A1: traits::Delete, A2, A3>(cap: &Capability<A1, A2, A3>) -> io::Result<()> {
    fs::remove_dir(cap.get_path())
}

pub fn remove_dir_all<A1: traits::Delete, A2, A3: traits::Delete>(
    cap: &Capability<A1, A2, A3>,
) -> io::Result<()> {
    fs::remove_dir_all(cap.get_path())
}

pub fn remove_file<A1: traits::Delete, A2, A3>(cap: &Capability<A1, A2, A3>) -> io::Result<()> {
    fs::remove_file(cap.get_path())
}

pub fn rename<A1, A2, A3, B1, B2, B3>(
    from: &Capability<A1, A2, A3>,
    to: &Capability<B1, B2, B3>,
) -> io::Result<()>
where
    A1: traits::Move,
    B1: traits::Create,
{
    fs::rename(from.get_path(), to.get_path())
}

// NOTE - Reconsider this functon and the permissions required
pub fn set_permissions<A, B>(
    cap: &Capability<(Create, View, Read, Write, Append, Copy, Move, Delete), A, B>,
    perm: fs::Permissions,
) -> io::Result<()> {
    fs::set_permissions(cap.get_path(), perm)
}

pub fn symlink_metadata<A1: traits::View, A2, A3>(
    cap: &Capability<A1, A2, A3>,
) -> io::Result<Metadata> {
    Ok(Metadata(fs::symlink_metadata(cap.get_path())?))
}

pub fn write<A1: traits::Write, A2, A3, J: AsRef<[u8]>>(
    cap: &Capability<A1, A2, A3>,
    contents: J,
) -> io::Result<()> {
    fs::write(cap.get_path(), contents)
}

pub struct Metadata(fs::Metadata);

impl Metadata {
    // TODO - Create a Coenobita version of FileType
    pub fn file_type(&self) -> fs::FileType {
        self.0.file_type()
    }

    pub fn is_dir(&self) -> bool {
        self.0.is_dir()
    }

    pub fn is_file(&self) -> bool {
        self.0.is_file()
    }

    pub fn is_symlink(&self) -> bool {
        self.0.is_symlink()
    }

    pub fn len(&self) -> u64 {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() > 0
    }

    // TODO - Create a Coenobita version of Permissions
    pub fn permissions(&self) -> fs::Permissions {
        self.0.permissions()
    }

    pub fn modified(&self) -> io::Result<SystemTime> {
        self.0.modified()
    }

    pub fn accessed(&self) -> io::Result<SystemTime> {
        self.0.accessed()
    }

    pub fn created(&self) -> io::Result<SystemTime> {
        self.0.created()
    }

    // NOTE - Unix only
    pub fn ino(&self) -> u64 {
        self.0.ino()
    }
}

impl fmt::Debug for Metadata {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Metadata")
            .field("file_type", &self.file_type())
            .field("is_dir", &self.is_dir())
            .field("is_file", &self.is_file())
            .field("permissions", &self.permissions())
            .field("modified", &self.modified())
            .field("accessed", &self.accessed())
            .field("created", &self.created())
            .finish_non_exhaustive()
    }
}

pub struct DirEntry<A, B, C> {
    _entry: fs::DirEntry,
    phantom: PhantomData<(A, B, C)>,
}

impl<A, B, C> DirEntry<A, B, C> {
    pub fn path(&self) -> Capability<A, B, C> {
        Capability {
            path: self._entry.path().to_path_buf(),
            phantom: PhantomData::<(A, B, C)>,
        }
    }
}

impl<A: traits::View, B, C> DirEntry<A, B, C> {
    pub fn file_type(&self) -> io::Result<fs::FileType> {
        self._entry.file_type()
    }

    pub fn ino(&self) -> u64 {
        self._entry.ino()
    }
}

#[derive(Debug)]
pub struct ReadDir<A, B, C> {
    _read_dir: fs::ReadDir,
    phantom: PhantomData<(A, B, C)>,
}

impl<A, B, C> Iterator for ReadDir<A, B, C> {
    type Item = io::Result<DirEntry<A, B, C>>;

    fn next(&mut self) -> Option<io::Result<DirEntry<A, B, C>>> {
        self._read_dir.next().map(|entry| {
            entry.map(|r| DirEntry {
                _entry: r,
                phantom: PhantomData::<(A, B, C)>,
            })
        })
    }
}
