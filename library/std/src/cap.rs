/// Capability safe version of `AsRef`
pub trait AsRef<T: ?Sized> {
    #[cnbt::provenance((*,*) fn((*, root)) -> (*,root))]
    fn as_ref(&self) -> &T;
}

/// Capability safe version of `From`
pub trait From<T>: Sized {
    #[cnbt::provenance((*,*) fn((*, root)) -> (*,root))]
    fn from(value: T) -> Self;
}
