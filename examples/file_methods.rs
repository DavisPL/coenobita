use coenobita::{ cap, Capability, Read, Write, Copy, Move, Delete, NotGranted };
use coenobita::fs::File;

fn main() -> std::io::Result<()> {
    // Creates an instance of Capability with read, copy, and move permissions
    let rcm_cap = cap!("example.txt" with Read, Copy, Move);
    let wd_cap = cap!("example_copied.txt" with Delete, Write);

    // The line below fails because it expects a Write permission
    // let file = File::create(&rcm_cap);

    // This line succeeds since all permissions are given
    let file = File::create(&wd_cap)?;
    println!("{:?}", file);

    Ok(())
}
