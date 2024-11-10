//! Items common to the prelude of all editions.
//!
//! See the [module-level documentation](super) for more.

// No formatting: this file is nothing but re-exports, and their order is worth preserving.
#![cfg_attr(rustfmt, rustfmt::skip)]

// Re-exported core operators
pub use crate::marker::{Send, Sized, Sync, Unpin};
pub use crate::ops::{Drop, Fn, FnMut, FnOnce};

// Re-exported functions
pub use crate::mem::drop;
pub use crate::mem::{align_of, align_of_val, size_of, size_of_val};

// Re-exported types and traits
pub use crate::convert::{AsMut, Into};
pub use crate::iter::{DoubleEndedIterator, ExactSizeIterator};
pub use crate::iter::{Extend, IntoIterator, Iterator};
pub use crate::option::Option::{self, None, Some};
pub use crate::result::Result::{self, Err, Ok};

// Re-exported built-in macros
#[allow(deprecated)]
pub use core::prelude::v1::{
    assert, cfg, column, compile_error, concat, env, file, format_args, include, 
    include_bytes, include_str, line, module_path, option_env,
    stringify, Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd,
};

pub use core::prelude::v1::{
    derive, global_allocator, test,
};

// The file so far is equivalent to core/src/prelude/v1.rs. It is duplicated
// rather than glob imported because we want docs to show these re-exports as
// pointing to within `std`.
// Below are the items from the alloc crate.

pub use crate::borrow::ToOwned;
pub use crate::boxed::Box;
pub use crate::string::{String, ToString};
pub use crate::vec::Vec;
