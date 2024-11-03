use crate::io;
use crate::path::Path;

use std::fs;

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

pub struct Metadata(fs::Metadata);

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
}

pub fn metadata<C: AsRef<Path>>(path: C) -> io::Result<Metadata> {
    Ok(Metadata(fs::metadata(&path.as_ref().inner)?))
}

pub fn symlink_metadata<C: AsRef<Path>>(path: C) -> io::Result<Metadata> {
    Ok(Metadata(fs::symlink_metadata(&path.as_ref().inner)?))
}
