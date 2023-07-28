use coenobita::{ cap, Capability, Read, Write, Copy, Move, Delete, NotGranted };
use coenobita::fs::{ File };

fn main -> io::Result<()> {
    // Create capability with read and write permissions
    let rw_cap = cap!("trait_exploration.txt" with Read, Write);

    // Open file in read-write mode using the read-write capability
    let mut rw_file = File::open(&rw_cap)?;

    Ok(())
}
