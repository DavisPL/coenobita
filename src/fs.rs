use crate::{ Capability, Create, View, Read, Write, Append, Copy, Move, Delete };
use crate::{ traits };

use std::os::unix::fs::MetadataExt;
use std::marker::PhantomData;
use std::{ path, fs, io };
use std::time::SystemTime;
use std::fmt;

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
    (cap: &Capability<
        (A, B, C, D, E, F, G, H),
        ((), (), (), (), (), (), (), ()),
        ((), (), (), (), (), (), (), ())
    >) -> io::Result<File<A, B, C, D>> {
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

    // NOTE - Should we be able to create directories this way?
    pub fn create<B, C, D, E, F, G, H>
    (cap: &Capability<
        (Create, B, C, D, E, F, G, H),
        ((), (), (), (), (), (), (), ()),
        ((), (), (), (), (), (), (), ())
    >) -> io::Result<File<Create, B, C, D>> {
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

    // NOTE - Should we be able to create directories this way?
    pub fn create_new<B, C, D, E, F, G, H>
    (cap: &Capability<
        (Create, B, C, D, E, F, G, H),
        ((), (), (), (), (), (), (), ()),
        ((), (), (), (), (), (), (), ())
    >) -> io::Result<File<Create, B, C, D>> {
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

pub fn canonicalize<A1, A2, A3> (cap: &Capability<A1, A2, A3>) -> io::Result<path::PathBuf> {
    fs::canonicalize(cap.get_path())
}

pub fn copy<A1, A2, A3, B1, B2, B3> (from: &Capability<A1, A2, A3>, to: &Capability<B1, B2, B3>) -> io::Result<u64>
where A1: traits::Copy, B1: traits::Create {
    fs::copy(from.get_path(), to.get_path())
}

/*pub fn canonicalize<C: traits::Capability> (cap: &C) -> io::Result<path::PathBuf> {
    fs::canonicalize(cap.get_path())
}

pub fn copy<C1: traits::Copy, C2: traits::Create> (from: &C1, to: &C2) -> io::Result<u64> {
    fs::copy(from.get_path(), to.get_path())
}

pub fn create_dir<C: traits::Create> (cap: &C) -> io::Result<()> {
    fs::create_dir(cap.get_path())
}

pub fn create_dir_all<C: traits::Create> (cap: &C) -> io::Result<()> {
    fs::create_dir_all(cap.get_path())
}

pub fn hard_link<C1: traits::View, C2: traits::Create> (original: &C1, link: &C2) -> io::Result<()> {
    panic!("[Coenobita] [ERROR] Hard linking is not supported yet.");
}

pub fn metadata<C: traits::View> (cap: &C) -> io::Result<Metadata> {
    Ok(Metadata(fs::metadata(cap.get_path())?))
}

pub fn read<C: traits::Read> (cap: &C) -> io::Result<Vec<u8>> {
    fs::read(cap.get_path())
}

pub fn read_dir<C: traits::Read> (cap: &C) -> io::Result<fs::ReadDir> {
    fs::read_dir(cap.get_path())
}

pub fn read_link<C: traits::Read> (cap: &C) -> io::Result<path::PathBuf> {
    fs::read_link(cap.get_path())
}

pub fn read_to_string<C: traits::Read> (cap: &C) -> io::Result<String> {
    fs::read_to_string(cap.get_path())
}

pub fn remove_dir<C: traits::Delete> (cap: &C) -> io::Result<()> {
    fs::remove_dir(cap.get_path())
}

pub fn remove_dir_all<C: traits::Delete + traits::DeleteAnyChild> (cap: &C) -> io::Result<()> {
    fs::remove_dir_all(cap.get_path())
}

pub fn remove_file<C: traits::Delete> (cap: &C) -> io::Result<()> {
    fs::remove_file(cap.get_path())
}

pub fn rename<C1: traits::Copy + traits::Delete, C2: traits::Create>
(from: &C1, to: &C2) -> io::Result<()> {
    fs::rename(from.get_path(), to.get_path())
}

// NOTE - Reconsider this functon and the permissions required
pub fn set_permissions<A, B>
(cap: &Capability<(Create, View, Read, Write, Append, Copy, Move, Delete), A, B>, perm: fs::Permissions) -> io::Result<()> {
    fs::set_permissions(cap.get_path(), perm)
}

pub fn symlink_metadata<C: traits::View> (cap: &C) -> io::Result<Metadata> {
    Ok(Metadata(fs::symlink_metadata(cap.get_path())?))
}

pub fn write<C: traits::Write, J: AsRef<[u8]>> (cap: &C, contents: J) -> io::Result<()> {
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
*/
