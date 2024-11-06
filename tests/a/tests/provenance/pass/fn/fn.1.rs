#![allow(unused_variables)]
#![allow(unused_assignments)]

#[cnbt::provenance((*,*) fn((b,root)) -> (root,root))]
fn id(x: i32) -> i32 {
    x + 1
}

fn main() {
    #[cnbt::provenance((root,root))]
    let x = id(b::value());
}