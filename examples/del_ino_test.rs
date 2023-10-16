use std::fs;
use std::os::unix::fs::MetadataExt;
use std::io;

fn main() -> io::Result<()> {
    let meta = fs::metadata("examples/files/example.txt")?;
    let inode = meta.ino();
    
    println!("{}", inode);

    Ok(())
}
