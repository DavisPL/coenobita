use coenobita::{ cap };
use coenobita::fs;

use std::io;

fn main() -> io::Result<()> {
    let orig_cap = cap!("examples/files/afternoon_miracle.txt" with (Copy, Delete));
    let renamed_cap = cap!("examples/files/evening_miracle.txt" with (Read, Create));

    fs::rename(&orig_cap, &renamed_cap)?;

    let story = fs::read_to_string(&renamed_cap)?;
    println!("{:?}", story);

    Ok(())
}
