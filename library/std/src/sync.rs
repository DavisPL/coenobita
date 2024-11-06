use std::{ops::DerefMut, sync};

pub use std::sync::atomic;

pub struct Arc<T> {
    pub(crate) inner: sync::Arc<T>,
}
