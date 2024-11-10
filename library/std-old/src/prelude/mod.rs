mod common;

pub mod v1 {
    pub use super::common::*;
}

pub mod rust_2015 {
    pub use super::v1::*;
}

pub mod rust_2018 {
    pub use super::v1::*;
}

pub mod rust_2021 {
    pub use super::v1::*;

    pub use core::prelude::rust_2021::*;
}
