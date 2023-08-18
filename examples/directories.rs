use coenobita::{ cap, dir, Capability, Directory };
use coenobita::fs;

fn main() {
    let dir_cap = cap!("examples/files" with Read, Delete);
    let dir_cap = dir!(dir_cap with Read, Delete);

    println!("{:?}", dir_cap);
}
