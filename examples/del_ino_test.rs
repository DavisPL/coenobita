use coenobita::{cap, fs};
use std::io;

fn main() -> io::Result<()> {
    let meta = fs::metadata(cap!("examples/files/example.txt" with (View)))?;
    let inode = meta.ino();
    
    println!("{}", inode);
    Ok(())
}
