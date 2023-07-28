use coenobita::{ cap, Capability, Read, Write, Copy, Move, NotGranted };
use coenobita::fs::OpenOptions;

use std::io;

fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

fn main() -> io::Result<()> {
    // Creates two instances of Capability
    let r_cap = cap!("examples/files/example.txt" with Read);
    let rw_cap = cap!("examples/files/example.txt" with Read, Write);

    let rw_file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(&r_cap)?;

    println!("{:?}", rw_file);
    Ok(())
}
