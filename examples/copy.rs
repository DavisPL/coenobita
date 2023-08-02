use coenobita::{ cap, Capability, Read, Write, Copy, Move, Delete };
use coenobita::fs;

fn main() {
    let rcm_cap = cap!("examples/files/example.txt" with Read, Copy, Move);
    let wd_cap = cap!("examples/files/example_copied.txt" with Delete, Write);

    match fs::copy(&rcm_cap, &wd_cap) {
        Ok(bytes) => {
            println!("Successfully copied {} bytes from {:?} to {:?}", bytes, rcm_cap.get_path(), wd_cap.get_path());
        },

        Err(details) => {
            println!("Failed to copy - {:?}", details);
        }
    }
}
