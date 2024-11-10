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
    #[inline(always)]
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

#[inline(always)]
pub fn read<P: AsRef<Path>>(path: P) -> io::Result<Vec<u8>> {
    transmute!(fs::read(&AsRef::as_ref_cap(&path).inner))
}

#[inline(always)]
pub fn read_to_string<P: AsRef<Path>>(path: P) -> io::Result<String> {
    transmute!(fs::read_to_string(&AsRef::as_ref_cap(&path).inner))
}

#[inline(always)]
pub fn write<P: AsRef<Path>, C: std::convert::AsRef<[u8]>>(path: P, contents: C) -> io::Result<()> {
    transmute!(fs::write(&AsRef::as_ref_cap(&path).inner, contents))
}

impl File {
    #[inline(always)]
    pub fn open<P: AsRef<Path>>(path: P) -> io::Result<File> {
        unsafe { std::mem::transmute(fs::File::open(&AsRef::as_ref_cap(&path).inner)) }
    }

    #[inline(always)]
    pub fn create<P: AsRef<Path>>(path: P) -> io::Result<File> {
        unsafe { std::mem::transmute(fs::File::create(&AsRef::as_ref_cap(&path).inner)) }
    }

    #[inline(always)]
    pub fn create_new<P: AsRef<Path>>(path: P) -> io::Result<File> {
        unsafe { std::mem::transmute(fs::File::create_new(&AsRef::as_ref_cap(&path).inner)) }
    }

    #[inline(always)]
    pub fn options() -> OpenOptions {
        OpenOptions {
            inner: fs::OpenOptions::new(),
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

impl fmt::Debug for File {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl Read for &File {
    #[inline(always)]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        todo!()
    }

    #[inline(always)]
    fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> io::Result<usize> {
        todo!()
    }

    // Reserves space in the buffer based on the file size when available.
    #[inline(always)]
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        todo!()
    }

    // Reserves space in the buffer based on the file size when available.
    #[inline(always)]
    fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
        todo!()
    }
}

impl Write for &File {
    #[inline(always)]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        todo!()
    }

    #[inline(always)]
    fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> io::Result<usize> {
        todo!()
    }

    #[inline(always)]
    fn flush(&mut self) -> io::Result<()> {
        todo!()
    }
}

impl Seek for &File {
    #[inline(always)]
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        todo!()
    }
}

impl Read for File {
    #[inline(always)]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        transmute!(self.inner.read(buf))
    }

    #[inline(always)]
    fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> io::Result<usize> {
        transmute!(self.inner.read_vectored(bufs))
    }

    #[inline(always)]
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        transmute!(self.inner.read_to_end(buf))
    }

    #[inline(always)]
    fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
        transmute!(self.inner.read_to_string(buf))
    }
}

impl Write for File {
    #[inline(always)]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        transmute!(self.inner.write(buf))
    }

    #[inline(always)]
    fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> io::Result<usize> {
        transmute!(self.inner.write_vectored(bufs))
    }

    #[inline(always)]
    fn flush(&mut self) -> io::Result<()> {
        transmute!(self.inner.flush())
    }
}

impl Seek for File {
    #[inline(always)]
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        transmute!(self.inner.seek(pos))
    }
}

impl Read for Arc<File> {
    #[inline(always)]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        todo!()
    }

    #[inline(always)]
    fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> io::Result<usize> {
        todo!()
    }

    #[inline(always)]
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        todo!()
    }

    #[inline(always)]
    fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
        todo!()
    }
}

impl Write for Arc<File> {
    #[inline(always)]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        todo!()
    }

    #[inline(always)]
    fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> io::Result<usize> {
        todo!()
    }

    #[inline(always)]
    fn flush(&mut self) -> io::Result<()> {
        todo!()
    }
}

impl Seek for Arc<File> {
    #[inline(always)]
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        todo!()
    }
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
        unsafe { std::mem::transmute(self.inner.open(&AsRef::as_ref_cap(&path).inner)) }
    }
}

impl Metadata {
    #[inline(always)]
    pub fn file_type(&self) -> FileType {
        transmute!(self.inner.file_type())
    }

    #[inline(always)]
    pub fn is_dir(&self) -> bool {
        self.inner.is_dir()
    }

    #[inline(always)]
    pub fn is_file(&self) -> bool {
        self.inner.is_file()
    }

    #[inline(always)]
    pub fn is_symlink(&self) -> bool {
        self.inner.is_symlink()
    }

    #[inline(always)]
    pub fn len(&self) -> u64 {
        self.inner.len()
    }

    #[inline(always)]
    pub fn permissions(&self) -> Permissions {
        transmute!(self.inner.permissions())
    }

    #[inline(always)]
    pub fn modified(&self) -> io::Result<SystemTime> {
        transmute!(self.inner.modified())
    }

    #[inline(always)]
    pub fn accessed(&self) -> io::Result<SystemTime> {
        transmute!(self.inner.accessed())
    }

    #[inline(always)]
    pub fn created(&self) -> io::Result<SystemTime> {
        transmute!(self.inner.created())
    }
}

impl FileType {
    #[inline(always)]
    pub fn is_dir(&self) -> bool {
        self.inner.is_dir()
    }

    #[inline(always)]
    pub fn is_file(&self) -> bool {
        self.inner.is_file()
    }

    #[inline(always)]
    pub fn is_symlink(&self) -> bool {
        self.inner.is_symlink()
    }
}

impl Iterator for ReadDir {
    type Item = io::Result<DirEntry>;

    #[inline(always)]
    fn next(&mut self) -> Option<io::Result<DirEntry>> {
        transmute!(self.inner.next())
    }
}

impl DirEntryExt for DirEntry {
    #[inline(always)]
    fn ino(&self) -> u64 {
        self.inner.ino()
    }
}

impl DirEntry {
    // This doesn't need a tag because the user provided a capability for the entire directory, and
    // hence all the entries in said directory. In other words, this isn't creating entirely new capabilities.
    // The user gave a directory capability to some crate, so it has permission to do things to everything inside.
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

impl Debug for DirEntry {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

#[inline(always)]
pub fn remove_file<P: AsRef<Path>>(path: P) -> io::Result<()> {
    transmute!(fs::remove_file(&AsRef::as_ref_cap(&path).inner))
}

#[inline(always)]
pub fn metadata<P: AsRef<Path>>(path: P) -> io::Result<Metadata> {
    transmute!(fs::metadata(&AsRef::as_ref_cap(&path).inner))
}

#[inline(always)]
pub fn symlink_metadata<P: AsRef<Path>>(path: P) -> io::Result<Metadata> {
    transmute!(fs::symlink_metadata(&AsRef::as_ref_cap(&path).inner))
}

#[inline(always)]
pub fn rename<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> io::Result<()> {
    transmute!(fs::rename(
        &AsRef::as_ref_cap(&from).inner,
        &&AsRef::as_ref_cap(&to).inner
    ))
}

#[inline(always)]
pub fn copy<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> io::Result<u64> {
    transmute!(fs::copy(&&AsRef::as_ref_cap(&from).inner, &&AsRef::as_ref_cap(&to).inner))
}

#[inline(always)]
pub fn hard_link<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> io::Result<()> {
    transmute!(fs::hard_link(
        &AsRef::as_ref_cap(&original).inner,
        &AsRef::as_ref_cap(&link).inner
    ))
}

#[inline(always)]
pub fn read_link<P: AsRef<Path>>(path: P) -> io::Result<PathBuf> {
    transmute!(fs::read_link(&AsRef::as_ref_cap(&path).inner))
}

#[inline(always)]
pub fn canonicalize<P: AsRef<Path>>(path: P) -> io::Result<PathBuf> {
    transmute!(fs::canonicalize(&AsRef::as_ref_cap(&path).inner))
}

#[inline(always)]
pub fn create_dir<P: AsRef<Path>>(path: P) -> io::Result<()> {
    transmute!(fs::create_dir(&AsRef::as_ref_cap(&path).inner))
}

#[inline(always)]
pub fn create_dir_all<P: AsRef<Path>>(path: P) -> io::Result<()> {
    transmute!(fs::create_dir_all(&AsRef::as_ref_cap(&path).inner))
}

#[inline(always)]
pub fn remove_dir<P: AsRef<Path>>(path: P) -> io::Result<()> {
    transmute!(fs::remove_dir(&AsRef::as_ref_cap(&path).inner))
}

#[inline(always)]
pub fn remove_dir_all<P: AsRef<Path>>(path: P) -> io::Result<()> {
    transmute!(fs::remove_dir_all(&AsRef::as_ref_cap(&path).inner))
}

#[inline(always)]
pub fn read_dir<P: AsRef<Path>>(path: P) -> io::Result<ReadDir> {
    transmute!(fs::read_dir(&AsRef::as_ref_cap(&path).inner))
}

#[inline(always)]
pub fn set_permissions<P: AsRef<Path>>(path: P, perm: Permissions) -> io::Result<()> {
    transmute!(fs::set_permissions(&AsRef::as_ref_cap(&path).inner, transmute!(perm)))
}
