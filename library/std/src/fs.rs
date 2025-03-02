pub use std::fs::{self, *};

use crate::ffi::OsString;
use crate::fmt;
use crate::io::{self, IoSlice, IoSliceMut, Read, Seek, SeekFrom, Write};
use crate::os::unix::prelude::DirEntryExt;
use crate::path::{Path, PathBuf};
use crate::time::SystemTime;
use crate::transmute;
use std::convert;

#[inline(always)]
pub fn read_dir<P: AsRef<Path>>(path: P) -> io::Result<ReadDir> {
    transmute!(fs::read_dir(&path.as_ref().inner))
}

#[inline(always)]
pub fn create_dir_all<P: AsRef<Path>>(path: P) -> io::Result<()> {
    fs::create_dir_all(&path.as_ref().inner)
}

#[inline(always)]
pub fn create_dir<P: AsRef<Path>>(path: P) -> io::Result<()> {
    fs::create_dir(&path.as_ref().inner)
}

#[inline(always)]
pub fn remove_dir<P: AsRef<Path>>(path: P) -> io::Result<()> {
    fs::remove_dir(&path.as_ref().inner)
}

#[inline(always)]
pub fn remove_dir_all<P: AsRef<Path>>(path: P) -> io::Result<()> {
    fs::remove_dir_all(&path.as_ref().inner)
}

#[inline(always)]
pub fn remove_file<P: AsRef<Path>>(path: P) -> io::Result<()> {
    fs::remove_file(&path.as_ref().inner)
}

#[inline(always)]
pub fn metadata<P: AsRef<Path>>(path: P) -> io::Result<Metadata> {
    fs::metadata(&path.as_ref().inner)
}

#[inline(always)]
pub fn symlink_metadata<P: AsRef<Path>>(path: P) -> io::Result<Metadata> {
    fs::symlink_metadata(&path.as_ref().inner)
}

#[inline(always)]
pub fn read_link<P: AsRef<Path>>(path: P) -> io::Result<PathBuf> {
    transmute!(fs::read_link(&path.as_ref().inner))
}

#[inline(always)]
pub fn copy<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> io::Result<u64> {
    fs::copy(&from.as_ref().inner, &to.as_ref().inner)
}

#[inline(always)]
pub fn hard_link<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> io::Result<()> {
    fs::hard_link(&original.as_ref().inner, &link.as_ref().inner)
}

#[inline(always)]
pub fn read<P: AsRef<Path>>(path: P) -> io::Result<Vec<u8>> {
    fs::read(&path.as_ref().inner)
}

#[inline(always)]
pub fn read_to_string<P: AsRef<Path>>(path: P) -> io::Result<String> {
    fs::read_to_string(&path.as_ref().inner)
}

#[inline(always)]
pub fn write<P: AsRef<Path>, C: convert::AsRef<[u8]>>(path: P, contents: C) -> io::Result<()> {
    fs::write(&path.as_ref().inner, contents)
}

pub struct ReadDir {
    pub(crate) inner: fs::ReadDir,
}

impl fmt::Debug for ReadDir {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl Iterator for ReadDir {
    type Item = io::Result<DirEntry>;

    #[inline(always)]
    fn next(&mut self) -> Option<io::Result<DirEntry>> {
        transmute!(self.inner.next())
    }
}

pub struct DirEntry {
    pub(crate) inner: fs::DirEntry,
}

impl DirEntry {
    #[inline(always)]
    pub fn path(&self) -> PathBuf {
        transmute!(self.inner.path())
    }

    #[inline(always)]
    pub fn metadata(&self) -> io::Result<Metadata> {
        transmute!(self.inner.metadata())
    }

    #[inline(always)]
    pub fn file_type(&self) -> io::Result<FileType> {
        transmute!(self.inner.file_type())
    }

    #[inline(always)]
    pub fn file_name(&self) -> OsString {
        transmute!(self.inner.file_name())
    }
}

impl DirEntryExt for DirEntry {
    #[inline(always)]
    fn ino(&self) -> u64 {
        self.inner.ino()
    }
}

impl fmt::Debug for DirEntry {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

pub struct File {
    pub(crate) inner: fs::File,
}

impl File {
    #[inline(always)]
    pub fn open<P: AsRef<Path>>(path: P) -> io::Result<File> {
        unsafe { std::mem::transmute(fs::File::open(&path.as_ref().inner)) }
    }

    #[inline(always)]
    pub fn create<P: AsRef<Path>>(path: P) -> io::Result<File> {
        unsafe { std::mem::transmute(fs::File::create(&path.as_ref().inner)) }
    }

    #[inline(always)]
    pub fn create_new<P: AsRef<Path>>(path: P) -> io::Result<File> {
        unsafe { std::mem::transmute(fs::File::create_new(&path.as_ref().inner)) }
    }

    #[inline(always)]
    pub fn options() -> OpenOptions {
        OpenOptions {
            inner: fs::File::options(),
        }
    }

    #[inline(always)]
    pub fn sync_all(&self) -> io::Result<()> {
        transmute!(self.inner.sync_all())
    }

    #[inline(always)]
    pub fn sync_data(&self) -> io::Result<()> {
        transmute!(self.inner.sync_data())
    }

    #[inline(always)]
    pub fn set_len(&self, size: u64) -> io::Result<()> {
        transmute!(self.inner.set_len(size))
    }

    #[inline(always)]
    pub fn metadata(&self) -> io::Result<Metadata> {
        transmute!(self.inner.metadata())
    }

    #[inline(always)]
    pub fn try_clone(&self) -> io::Result<File> {
        transmute!(self.inner.try_clone())
    }

    #[inline(always)]
    pub fn set_permissions(&self, perm: Permissions) -> io::Result<()> {
        transmute!(self.inner.set_permissions(transmute!(perm)))
    }

    #[inline(always)]
    pub fn set_times(&self, times: FileTimes) -> io::Result<()> {
        transmute!(self.inner.set_times(transmute!(times)))
    }

    #[inline(always)]
    pub fn set_modified(&self, time: SystemTime) -> io::Result<()> {
        transmute!(self.inner.set_modified(time))
    }
}

impl Read for &File {
    #[inline(always)]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        (&self.inner).read(buf)
    }

    #[inline(always)]
    fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> io::Result<usize> {
        (&self.inner).read_vectored(bufs)
    }

    #[inline(always)]
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        (&self.inner).read_to_end(buf)
    }

    #[inline(always)]
    fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
        (&self.inner).read_to_string(buf)
    }
}

impl Write for &File {
    #[inline(always)]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        (&self.inner).write(buf)
    }

    #[inline(always)]
    fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> io::Result<usize> {
        (&self.inner).write_vectored(bufs)
    }

    #[inline(always)]
    fn flush(&mut self) -> io::Result<()> {
        (&self.inner).flush()
    }
}

impl Seek for &File {
    #[inline(always)]
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        (&self.inner).seek(pos)
    }
}

impl Read for File {
    #[inline(always)]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        (&*self).read(buf)
    }

    #[inline(always)]
    fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> io::Result<usize> {
        (&*self).read_vectored(bufs)
    }

    #[inline(always)]
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        (&*self).read_to_end(buf)
    }

    #[inline(always)]
    fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
        (&*self).read_to_string(buf)
    }
}

impl Write for File {
    #[inline(always)]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        (&*self).write(buf)
    }

    #[inline(always)]
    fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> io::Result<usize> {
        (&*self).write_vectored(bufs)
    }

    #[inline(always)]
    fn flush(&mut self) -> io::Result<()> {
        (&*self).flush()
    }
}

impl Seek for File {
    #[inline(always)]
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        (&*self).seek(pos)
    }
}

impl fmt::Debug for File {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

pub struct OpenOptions {
    pub(crate) inner: fs::OpenOptions,
}

impl OpenOptions {
    #[inline(always)]
    pub fn new() -> Self {
        OpenOptions {
            inner: fs::OpenOptions::new(),
        }
    }

    #[inline(always)]
    pub fn read(&mut self, read: bool) -> &mut Self {
        unsafe { std::mem::transmute(self.inner.read(read)) }
    }

    #[inline(always)]
    pub fn write(&mut self, write: bool) -> &mut Self {
        unsafe { std::mem::transmute(self.inner.write(write)) }
    }

    #[inline(always)]
    pub fn append(&mut self, append: bool) -> &mut Self {
        unsafe { std::mem::transmute(self.inner.append(append)) }
    }

    #[inline(always)]
    pub fn truncate(&mut self, truncate: bool) -> &mut Self {
        unsafe { std::mem::transmute(self.inner.truncate(truncate)) }
    }

    #[inline(always)]
    pub fn create(&mut self, create: bool) -> &mut Self {
        unsafe { std::mem::transmute(self.inner.create(create)) }
    }

    #[inline(always)]
    pub fn create_new(&mut self, create_new: bool) -> &mut Self {
        unsafe { std::mem::transmute(self.inner.create_new(create_new)) }
    }

    #[inline(always)]
    pub fn open<P: AsRef<Path>>(&self, path: P) -> io::Result<File> {
        unsafe { std::mem::transmute(self.inner.open(&path.as_ref().inner)) }
    }
}

pub struct DirBuilder {
    pub(crate) inner: fs::DirBuilder
}

impl DirBuilder {
    #[inline(always)]
    pub fn new() -> DirBuilder {
        DirBuilder { inner: fs::DirBuilder::new() }
    }

    #[inline(always)]
    pub fn recursive(&mut self, recursive: bool) -> &mut Self {
        transmute!(self.inner.recursive(recursive))
    }

    #[inline(always)]
    pub fn create<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        self.inner.create(&path.as_ref().inner)
    }
}

#[inline(always)]
pub fn exists<P: AsRef<Path>>(path: P) -> io::Result<bool> {
    fs::exists(&path.as_ref().inner)
}
