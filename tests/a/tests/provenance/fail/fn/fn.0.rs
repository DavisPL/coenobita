#![allow(unused_variables)]
#![allow(unused_assignments)]

#[cnbt::provenance((*,*) fn((b,b)) -> (root,root))]
fn id(x: i32) -> i32 {
    x
}

fn main() {
    #[cnbt::provenance((b,b))]
    let x = id(b::value());
}