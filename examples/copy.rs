use coenobita::cap;
use coenobita::fs;

fn main() {
    let rcm_cap = cap!("examples/files/example.txt" with (Read, Copy, Move));
    let wd_cap = cap!("examples/files/example_copied.txt" with (Create, Delete, Write));

    match fs::copy(&rcm_cap, &wd_cap) {
        Ok(bytes) => {
            println!("Successfully copied {} bytes from {:?} to {:?}", bytes, rcm_cap.to_path(), wd_cap.to_path());
        },

        Err(details) => {
            println!("Failed to copy - {:?}", details);
        }
    }
}
