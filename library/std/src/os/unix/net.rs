use std::os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd, IntoRawFd, OwnedFd, RawFd};
pub use std::os::unix::net::{self, *};
use crate::path::Path;
use crate::{io, transmute};

pub struct UnixListener {
    inner: net::UnixListener
}

impl UnixListener {
    pub fn bind<P: AsRef<Path>>(path: P) -> io::Result<UnixListener> {
        transmute!(net::UnixListener::bind(&path.as_ref().inner))
    }

    pub fn bind_addr(socket_addr: &SocketAddr) -> io::Result<UnixListener> {
        transmute!(net::UnixListener::bind_addr(socket_addr))
    }

    pub fn accept(&self) -> io::Result<(UnixStream, SocketAddr)> {
        self.inner.accept()
    }

    pub fn try_clone(&self) -> io::Result<UnixListener> {
        transmute!(self.inner.try_clone())
    }

    pub fn local_addr(&self) -> io::Result<SocketAddr> {
        self.inner.local_addr()
    }

    pub fn set_nonblocking(&self, nonblocking: bool) -> io::Result<()> {
        self.inner.set_nonblocking(nonblocking)
    }

    pub fn take_error(&self) -> io::Result<Option<io::Error>> {
        self.inner.take_error()
    }

    pub fn incoming(&self) -> Incoming<'_> {
        self.inner.incoming()
    }
}

impl AsRawFd for UnixListener {
    #[inline(always)]
    fn as_raw_fd(&self) -> RawFd {
        self.inner.as_raw_fd()
    }
}

impl FromRawFd for UnixListener {
    #[inline(always)]
    unsafe fn from_raw_fd(fd: RawFd) -> UnixListener {
        UnixListener { inner: net::UnixListener::from_raw_fd(fd) }
    }
}

impl IntoRawFd for UnixListener {
    #[inline(always)]
    fn into_raw_fd(self) -> RawFd {
        self.inner.into_raw_fd()
    }
}

impl AsFd for UnixListener {
    #[inline(always)]
    fn as_fd(&self) -> BorrowedFd<'_> {
        self.inner.as_fd()
    }
}

impl From<OwnedFd> for UnixListener {
    #[inline(always)]
    fn from(fd: OwnedFd) -> UnixListener {
        UnixListener { inner: net::UnixListener::from(fd) }
    }
}

impl From<UnixListener> for OwnedFd {
    /// Takes ownership of a [`UnixListener`]'s socket file descriptor.
    #[inline(always)]
    fn from(listener: UnixListener) -> OwnedFd {
        OwnedFd::from(listener.inner)
    }
}

impl<'a> IntoIterator for &'a UnixListener {
    type Item = io::Result<UnixStream>;
    type IntoIter = Incoming<'a>;

    fn into_iter(self) -> Incoming<'a> {
        self.incoming()
    }
}