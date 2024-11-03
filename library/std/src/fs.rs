use crate::io;
use crate::path::{Path, PathBuf};

use std::fmt::Debug;
use std::fs;
use std::os::unix::fs::{DirEntryExt, MetadataExt};

pub struct File(fs::File);

impl File {
    pub fn create<P: AsRef<Path>>(path: P) -> io::Result<File> {
        Ok(File(fs::File::create(&path.as_ref().inner)?))
    }
}

pub struct ReadDir(fs::ReadDir);

impl Iterator for ReadDir {
    type Item = io::Result<DirEntry>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|v| v.map(|o| DirEntry(o)))
    }
}

impl Debug for ReadDir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub struct DirEntry(fs::DirEntry);

impl DirEntry {
    pub fn file_type(&self) -> io::Result<FileType> {
        Ok(FileType(self.0.file_type()?))
    }

    pub fn path(&self) -> PathBuf {
        PathBuf {
            inner: self.0.path(),
        }
    }

    pub fn ino(&self) -> u64 {
        self.0.ino()
    }
}

#[derive(Clone, Copy)]
pub struct FileType(fs::FileType);

impl FileType {
    pub fn is_dir(&self) -> bool {
        self.0.is_symlink()
    }

    pub fn is_file(&self) -> bool {
        self.0.is_symlink()
    }

    pub fn is_symlink(&self) -> bool {
        self.0.is_symlink()
    }
}

pub struct Metadata(pub(crate) fs::Metadata);

impl Metadata {
    pub fn is_dir(&self) -> bool {
        self.0.is_dir()
    }

    pub fn is_file(&self) -> bool {
        self.0.is_file()
    }

    pub fn is_symlink(&self) -> bool {
        self.0.is_symlink()
    }

    pub fn file_type(&self) -> FileType {
        FileType(self.0.file_type())
    }

    pub fn ino(&self) -> u64 {
        self.0.ino()
    }

    pub fn dev(&self) -> u64 {
        self.0.dev()
    }
}

pub fn metadata<P: AsRef<Path>>(path: P) -> io::Result<Metadata> {
    Ok(Metadata(fs::metadata(&path.as_ref().inner)?))
}

pub fn symlink_metadata<P: AsRef<Path>>(path: P) -> io::Result<Metadata> {
    Ok(Metadata(fs::symlink_metadata(&path.as_ref().inner)?))
}

pub fn read_to_string<P: AsRef<Path>>(path: P) -> io::Result<String> {
    Ok(fs::read_to_string(&path.as_ref().inner)?)
}

pub fn read_dir<P: AsRef<Path>>(path: P) -> io::Result<ReadDir> {
    Ok(ReadDir(fs::read_dir(&path.as_ref().inner)?))
}

pub fn remove_dir_all<P: AsRef<Path>>(path: P) -> io::Result<()> {
    fs::remove_dir_all(&path.as_ref().inner)
}

pub fn create_dir_all<P: AsRef<Path>>(path: P) -> io::Result<()> {
    fs::create_dir_all(&path.as_ref().inner)
}
