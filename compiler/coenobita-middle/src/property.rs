use std::fmt::Display;

pub trait Property: Clone + Default + Display {
    fn satisfies(&self, other: &Self) -> bool;
}
