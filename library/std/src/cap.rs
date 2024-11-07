/// Capability safe version of `AsRef`
pub trait AsRef<T: ?Sized>: std::convert::AsRef<T> {
    #[cnbt::provenance((*,*) fn((*, root)) -> (*,root))]
    fn as_ref(&self) -> &T;
}

impl<T: ?Sized, U: ?Sized> AsRef<U> for T
where
    T: std::convert::AsRef<U>,
{
    fn as_ref(&self) -> &U {
        <Self as std::convert::AsRef<U>>::as_ref(&self)
    }
}

/// Capability safe version of `From`
pub trait From<T>: Sized {
    #[cnbt::provenance((*,*) fn((*, root)) -> (*,root))]
    fn from(value: T) -> Self;
}
