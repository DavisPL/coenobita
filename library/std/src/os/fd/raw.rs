use std::os::fd::*;

use crate::{fs, transmute};

impl From<fs::File> for OwnedFd {
    #[inline(always)]
    fn from(file: fs::File) -> OwnedFd {
        file.inner.into()
    }
}

impl From<OwnedFd> for fs::File {
    #[inline(always)]
    fn from(owned_fd: OwnedFd) -> Self {
        transmute!(owned_fd)
    }
}

impl AsRawFd for fs::File {
    #[inline(always)]
    fn as_raw_fd(&self) -> RawFd {
        self.inner.as_raw_fd()
    }
}

impl FromRawFd for fs::File {
    #[inline(always)]
    unsafe fn from_raw_fd(fd: RawFd) -> fs::File {
        unsafe { fs::File::from(OwnedFd::from_raw_fd(fd)) }
    }
}

impl IntoRawFd for fs::File {
    #[inline(always)]
    fn into_raw_fd(self) -> RawFd {
        self.inner.as_raw_fd()
    }
}
