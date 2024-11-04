#![allow(unused_variables)]
#![allow(unused_assignments)]

#[cnbt::provenance((*,*) fn((b,bin)) -> (b,bin))]
fn id(x: i32) -> i32 {
    x + 1
}

fn main() {
    #[cnbt::provenance((b,bin))]
    let x = id(b::value());
}