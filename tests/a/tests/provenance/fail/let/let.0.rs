#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    let x = 5;

    #[cnbt::provenance((root, root))]
    let y = x;
}