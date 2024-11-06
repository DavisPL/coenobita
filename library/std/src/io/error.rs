use std::{error, fmt, io};

use crate::{result, transmute};

pub use std::io::ErrorKind;

pub struct Error {
    inner: io::Error,
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

impl Error {
    pub fn new<E>(kind: ErrorKind, error: E) -> Error
    where
        E: Into<Box<dyn error::Error + Send + Sync>>,
    {
        transmute!(io::Error::new(kind, error))
    }

    #[inline]
    pub fn kind(&self) -> ErrorKind {
        self.inner.kind()
    }
}

impl error::Error for Error {
    #[allow(deprecated, deprecated_in_future)]
    fn description(&self) -> &str {
        self.inner.description()
    }
}

pub type Result<T> = result::Result<T, Error>;
