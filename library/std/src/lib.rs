pub use std::prelude;

// These are all required by `walkdir`
pub use std::borrow;
pub use std::cmp;
pub use std::collections;
pub use std::error;
pub use std::ffi;
pub use std::fmt;
pub use std::iter;
pub use std::ops;
pub use std::process;
pub use std::result;
pub use std::time;
pub use std::usize;
pub use std::vec;

// These are macros required by `walkdir`
pub use std::assert;
pub use std::assert_eq;
pub use std::format;
pub use std::panic;
pub use std::println;
pub use std::write;

// These are all required by `same-file`
pub use std::hash;

// This stuff has been modified
pub mod env;
pub mod fs;
pub mod io;
pub mod os;
pub mod path;
pub mod sync;

// This stuff is entirely new
pub mod cap;

mod _macros;
