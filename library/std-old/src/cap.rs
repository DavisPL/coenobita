/// Capability safe version of `AsRef`
pub trait AsRef<T: ?Sized>: std::convert::AsRef<T> {
    #[inline(always)]
    #[cnbt::provenance((*,*) fn((*, root)) -> (*,root))]
    fn as_ref_cap(&self) -> &T {
        <Self as std::convert::AsRef<T>>::as_ref(&self)
    }
}

impl<T: ?Sized, U: ?Sized> AsRef<U> for T
where
    T: std::convert::AsRef<U>,
{
    #[inline(always)]
    fn as_ref_cap(&self) -> &U {
        <Self as std::convert::AsRef<U>>::as_ref(&self)
    }
}

/// Capability safe version of `From`
pub trait From<T>: Sized {
    #[cnbt::provenance((*,*) fn((*, root)) -> (*,root))]
    fn from(value: T) -> Self;
}
