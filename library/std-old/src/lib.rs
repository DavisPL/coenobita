// pub use std::prelude;

// These are all required by `walkdir`
pub use std::borrow;
pub use std::boxed;
pub use std::cmp;
pub use std::collections;
pub use std::convert;
pub use std::error;
pub use std::ffi;
pub use std::fmt;
pub use std::iter;
pub use std::marker;
pub use std::mem;
pub use std::ops;
pub use std::option;
pub use std::process;
pub use std::result;
pub use std::string;
pub use std::time;
pub use std::usize;
pub use std::vec;
pub use std::str;
pub use std::cell;
pub use std::default;
pub use std::ptr;
pub use std::num;
pub use std::rc;
pub use std::slice;

// These are macros required by `walkdir`
pub use std::{
    assert,
    assert_eq,
    format,
    panic,
    println,
    write,
    writeln,
    debug_assert,
    debug_assert_ne,
    debug_assert_eq,
    unreachable,
    r#try
};

// These are all required by `same-file`
pub use std::hash;

// This stuff has been modified
pub mod env;
pub mod fs;
pub mod io;
pub mod os;
pub mod path;
pub mod prelude;
pub mod sync;

// This stuff is entirely new
pub mod cap;

mod _macros;
