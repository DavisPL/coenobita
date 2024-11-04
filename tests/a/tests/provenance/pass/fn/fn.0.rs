#![allow(unused_variables)]
#![allow(unused_assignments)]

#[cnbt::provenance((*,*) fn((b,bin)) -> (b,bin))]
fn id(x: i32) -> i32 {
    x
}

fn main() {
    #[cnbt::provenance((b,bin))]
    let x = id(b::value());
}