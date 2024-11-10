use std::convert;

pub trait From<T>: convert::From<T> {
    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*, root)) -> (*,root))]
    fn from(value: T) -> Self {
        convert::From::from(value)
    }
}

pub trait AsRef<T: ?Sized>: convert::AsRef<T> {}

impl<T: ?Sized, U: ?Sized> AsRef<T> for &U where U: AsRef<T> {}
