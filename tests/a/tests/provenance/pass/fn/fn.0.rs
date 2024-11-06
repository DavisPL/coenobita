#![allow(unused_variables)]
#![allow(unused_assignments)]

#[cnbt::provenance((*,*) fn((b,root)) -> (b,root))]
fn id(x: i32) -> i32 {
    x
}

fn main() {
    #[cnbt::provenance((b,root))]
    let x = id(b::value());
}