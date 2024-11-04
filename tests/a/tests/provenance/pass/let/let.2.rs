#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::provenance((bin,bin))]
    let w = 5;

    #[cnbt::provenance((*,bin))]
    let x = w;

    #[cnbt::provenance((bin,*))]
    let y = w;

    #[cnbt::provenance((*,*))]
    let z = w;
}