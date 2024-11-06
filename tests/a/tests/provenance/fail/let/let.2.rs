#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::provenance((root,root))]
    let w = 5;

    #[cnbt::provenance((*,root))]
    let x = w;

    #[cnbt::provenance((root,*))]
    let y = x;

    #[cnbt::provenance((*,*))]
    let z = w;
}