pub(crate) trait Capability<T> {
    fn extract(self) -> T;
    fn peek(&self) -> &T;
}
