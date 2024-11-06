mod error;
use std::io;

pub use self::error::{Error, ErrorKind, Result};

pub use io::{IoSlice, IoSliceMut, SeekFrom};

pub trait Read {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize>;

    fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> Result<usize>;

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize>;

    fn read_to_string(&mut self, buf: &mut String) -> Result<usize>;
}

pub trait Write {
    fn write(&mut self, buf: &[u8]) -> Result<usize>;

    fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> Result<usize>;

    fn flush(&mut self) -> Result<()>;
}

pub trait Seek {
    fn seek(&mut self, pos: SeekFrom) -> Result<u64>;
}
