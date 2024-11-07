use std::ffi::OsString;
use std::fmt::{self, Debug};
use std::fs;
use std::io::{Read as Read_, Seek as Seek_, Write as Write_};
use std::os::unix::fs::DirEntryExt;

use crate::cap::AsRef;
use crate::io::{self, IoSlice, IoSliceMut, Read, Seek, SeekFrom, Write};
use crate::path::{Path, PathBuf};
use crate::sync::Arc;
use crate::time::SystemTime;

use crate::transmute;

pub struct File {
    pub(crate) inner: fs::File,
}

pub struct Metadata {
    pub(crate) inner: fs::Metadata,
}

pub struct ReadDir {
    inner: fs::ReadDir,
}

impl Debug for ReadDir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

pub struct DirEntry {
    inner: fs::DirEntry,
}

pub struct OpenOptions {
    inner: fs::OpenOptions,
}

pub struct FileTimes {
    inner: fs::FileTimes,
}

pub struct Permissions {
    inner: fs::Permissions,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct FileType {
    inner: fs::FileType,
}

pub fn read<P: AsRef<Path>>(path: P) -> io::Result<Vec<u8>> {
    transmute!(fs::read(&AsRef::as_ref(&path).inner))
}

pub fn read_to_string<P: AsRef<Path>>(path: P) -> io::Result<String> {
    transmute!(fs::read_to_string(&AsRef::as_ref(&path).inner))
}

pub fn write<P: AsRef<Path>, C: std::convert::AsRef<[u8]>>(path: P, contents: C) -> io::Result<()> {
    transmute!(fs::write(&AsRef::as_ref(&path).inner, contents))
}

impl File {
    pub fn open<P: AsRef<Path>>(path: P) -> io::Result<File> {
        unsafe { std::mem::transmute(fs::File::open(&AsRef::as_ref(&path).inner)) }
    }

    pub fn create<P: AsRef<Path>>(path: P) -> io::Result<File> {
        unsafe { std::mem::transmute(fs::File::create(&AsRef::as_ref(&path).inner)) }
    }

    pub fn create_new<P: AsRef<Path>>(path: P) -> io::Result<File> {
        unsafe { std::mem::transmute(fs::File::create_new(&AsRef::as_ref(&path).inner)) }
    }

    pub fn options() -> OpenOptions {
        OpenOptions {
            inner: fs::OpenOptions::new(),
        }
    }

    pub fn sync_all(&self) -> io::Result<()> {
        transmute!(self.inner.sync_all())
    }

    pub fn sync_data(&self) -> io::Result<()> {
        transmute!(self.inner.sync_data())
    }

    pub fn set_len(&self, size: u64) -> io::Result<()> {
        transmute!(self.inner.set_len(size))
    }

    pub fn metadata(&self) -> io::Result<Metadata> {
        transmute!(self.inner.metadata())
    }

    pub fn try_clone(&self) -> io::Result<File> {
        transmute!(self.inner.try_clone())
    }

    pub fn set_permissions(&self, perm: Permissions) -> io::Result<()> {
        transmute!(self.inner.set_permissions(transmute!(perm)))
    }

    pub fn set_times(&self, times: FileTimes) -> io::Result<()> {
        transmute!(self.inner.set_times(transmute!(times)))
    }

    #[inline]
    pub fn set_modified(&self, time: SystemTime) -> io::Result<()> {
        transmute!(self.inner.set_modified(time))
    }
}

impl fmt::Debug for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl Read for &File {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        todo!()
    }

    #[inline]
    fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> io::Result<usize> {
        todo!()
    }

    // Reserves space in the buffer based on the file size when available.
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        todo!()
    }

    // Reserves space in the buffer based on the file size when available.
    fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
        todo!()
    }
}

impl Write for &File {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        todo!()
    }

    fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> io::Result<usize> {
        todo!()
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        todo!()
    }
}

impl Seek for &File {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        todo!()
    }
}

impl Read for File {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        transmute!(self.inner.read(buf))
    }

    fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> io::Result<usize> {
        transmute!(self.inner.read_vectored(bufs))
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        transmute!(self.inner.read_to_end(buf))
    }

    fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
        transmute!(self.inner.read_to_string(buf))
    }
}

impl Write for File {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        transmute!(self.inner.write(buf))
    }

    fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> io::Result<usize> {
        transmute!(self.inner.write_vectored(bufs))
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        transmute!(self.inner.flush())
    }
}

impl Seek for File {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        transmute!(self.inner.seek(pos))
    }
}

impl Read for Arc<File> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        todo!()
    }

    fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> io::Result<usize> {
        todo!()
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        todo!()
    }

    fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
        todo!()
    }
}

impl Write for Arc<File> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        todo!()
    }

    fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> io::Result<usize> {
        todo!()
    }

    fn flush(&mut self) -> io::Result<()> {
        todo!()
    }
}

impl Seek for Arc<File> {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        todo!()
    }
}

impl OpenOptions {
    pub fn new() -> Self {
        OpenOptions {
            inner: fs::OpenOptions::new(),
        }
    }

    pub fn read(&mut self, read: bool) -> &mut Self {
        unsafe { std::mem::transmute(self.inner.read(read)) }
    }

    pub fn write(&mut self, write: bool) -> &mut Self {
        unsafe { std::mem::transmute(self.inner.write(write)) }
    }

    pub fn append(&mut self, append: bool) -> &mut Self {
        unsafe { std::mem::transmute(self.inner.append(append)) }
    }

    pub fn truncate(&mut self, truncate: bool) -> &mut Self {
        unsafe { std::mem::transmute(self.inner.truncate(truncate)) }
    }

    pub fn create(&mut self, create: bool) -> &mut Self {
        unsafe { std::mem::transmute(self.inner.create(create)) }
    }

    pub fn create_new(&mut self, create_new: bool) -> &mut Self {
        unsafe { std::mem::transmute(self.inner.create_new(create_new)) }
    }

    pub fn open<P: AsRef<Path>>(&self, path: P) -> io::Result<File> {
        unsafe { std::mem::transmute(self.inner.open(&AsRef::as_ref(&path).inner)) }
    }
}

impl Metadata {
    pub fn file_type(&self) -> FileType {
        transmute!(self.inner.file_type())
    }

    pub fn is_dir(&self) -> bool {
        self.inner.is_dir()
    }

    pub fn is_file(&self) -> bool {
        self.inner.is_file()
    }

    pub fn is_symlink(&self) -> bool {
        self.inner.is_symlink()
    }

    pub fn len(&self) -> u64 {
        self.inner.len()
    }

    pub fn permissions(&self) -> Permissions {
        transmute!(self.inner.permissions())
    }

    pub fn modified(&self) -> io::Result<SystemTime> {
        transmute!(self.inner.modified())
    }

    pub fn accessed(&self) -> io::Result<SystemTime> {
        transmute!(self.inner.accessed())
    }

    pub fn created(&self) -> io::Result<SystemTime> {
        transmute!(self.inner.created())
    }
}

impl FileType {
    pub fn is_dir(&self) -> bool {
        self.inner.is_dir()
    }

    pub fn is_file(&self) -> bool {
        self.inner.is_file()
    }

    pub fn is_symlink(&self) -> bool {
        self.inner.is_symlink()
    }
}

impl Iterator for ReadDir {
    type Item = io::Result<DirEntry>;

    fn next(&mut self) -> Option<io::Result<DirEntry>> {
        transmute!(self.inner.next())
    }
}

impl DirEntryExt for DirEntry {
    fn ino(&self) -> u64 {
        self.inner.ino()
    }
}

impl DirEntry {
    pub fn path(&self) -> PathBuf {
        transmute!(self.inner.path())
    }

    pub fn metadata(&self) -> io::Result<Metadata> {
        transmute!(self.inner.metadata())
    }

    pub fn file_type(&self) -> io::Result<FileType> {
        transmute!(self.inner.file_type())
    }

    pub fn file_name(&self) -> OsString {
        transmute!(self.inner.file_name())
    }
}

impl Debug for DirEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

pub fn remove_file<P: AsRef<Path>>(path: P) -> io::Result<()> {
    transmute!(fs::remove_file(&AsRef::as_ref(&path).inner))
}

pub fn metadata<P: AsRef<Path>>(path: P) -> io::Result<Metadata> {
    transmute!(fs::metadata(&AsRef::as_ref(&path).inner))
}

pub fn symlink_metadata<P: AsRef<Path>>(path: P) -> io::Result<Metadata> {
    transmute!(fs::symlink_metadata(&AsRef::as_ref(&path).inner))
}

pub fn rename<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> io::Result<()> {
    transmute!(fs::rename(
        &AsRef::as_ref(&from).inner,
        &&AsRef::as_ref(&to).inner
    ))
}

pub fn copy<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> io::Result<u64> {
    transmute!(fs::copy(&&AsRef::as_ref(&from).inner, &&AsRef::as_ref(&to).inner))
}

pub fn hard_link<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> io::Result<()> {
    transmute!(fs::hard_link(
        &AsRef::as_ref(&original).inner,
        &AsRef::as_ref(&link).inner
    ))
}

pub fn read_link<P: AsRef<Path>>(path: P) -> io::Result<PathBuf> {
    transmute!(fs::read_link(&AsRef::as_ref(&path).inner))
}

pub fn canonicalize<P: AsRef<Path>>(path: P) -> io::Result<PathBuf> {
    transmute!(fs::canonicalize(&AsRef::as_ref(&path).inner))
}

pub fn create_dir<P: AsRef<Path>>(path: P) -> io::Result<()> {
    transmute!(fs::create_dir(&AsRef::as_ref(&path).inner))
}

pub fn create_dir_all<P: AsRef<Path>>(path: P) -> io::Result<()> {
    transmute!(fs::create_dir_all(&AsRef::as_ref(&path).inner))
}

pub fn remove_dir<P: AsRef<Path>>(path: P) -> io::Result<()> {
    transmute!(fs::remove_dir(&AsRef::as_ref(&path).inner))
}

pub fn remove_dir_all<P: AsRef<Path>>(path: P) -> io::Result<()> {
    transmute!(fs::remove_dir_all(&AsRef::as_ref(&path).inner))
}

pub fn read_dir<P: AsRef<Path>>(path: P) -> io::Result<ReadDir> {
    transmute!(fs::read_dir(&AsRef::as_ref(&path).inner))
}

pub fn set_permissions<P: AsRef<Path>>(path: P, perm: Permissions) -> io::Result<()> {
    transmute!(fs::set_permissions(&AsRef::as_ref(&path).inner, transmute!(perm)))
}
