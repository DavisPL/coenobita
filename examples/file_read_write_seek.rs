use coenobita::{ self, cap, Capability };
use coenobita::fs::{ File };

use std::io;
use std::io::Read;

fn main() -> io::Result<()> {
    // Create capabilities with only Read/Write, Read, and Write permissions
    let rw_cap = cap!("examples/files/mc_read_write.txt" with Read, Write);
    let r_cap = cap!("examples/files/mc_read.txt" with Read);
    let w_cap = cap!("examples/files/mc_write.txt" with Write);
    
    // Open files using capabilities
    let mut rw_file = File::open(&rw_cap)?;
    let mut r_file = File::open(&r_cap)?;
    let mut w_file = File::open(&w_cap)?;
    
    // Print files to confirm permissions are correct
    println!("{:?}", rw_file);
    println!("{:?}", r_file);
    println!("{:?}", w_file);

    // Read from 'r_file'
    let mut r_buffer = [0; 10];

    let n = r_file.read(&mut r_buffer[..])?;
    println!("Success! We just read bytes {:?} from the file.", &r_buffer[..n]);
    
    Ok(())
}
