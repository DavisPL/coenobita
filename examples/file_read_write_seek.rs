use coenobita::{ self, cap };
use coenobita::fs::{ File };

use std::io::{ self, Read, Write, Seek, SeekFrom };

fn main() -> io::Result<()> {
    // Create capabilities with only Read/Write, Read, and Write permissions
    let rw_cap = cap!("examples/files/mc_read_write.txt" with (Read, Write));
    let r_cap = cap!("examples/files/mc_read.txt" with (Read));
    let w_cap = cap!("examples/files/mc_write.txt" with (Write));
    let noperms_cap = cap!("examples/files/mc_original.txt");

    // Open files using capabilities
    let mut rw_file = File::open(&rw_cap)?;
    let mut r_file = File::open(&r_cap)?;
    let mut w_file = File::open(&w_cap)?;
    let mut file = File::open(&noperms_cap)?;

    // Print files to confirm permissions are correct
    println!("{:?}", rw_file);
    println!("{:?}", r_file);
    println!("{:?}", w_file);

    // Read from 'r_file'
    let mut r_buffer = [0; 10];
    let n = r_file.read(&mut r_buffer[..])?;
    println!("Success! We just read bytes {:?} from the file.", &r_buffer[..n]);
   
    // Write to 'w_file'
    let n = w_file.write(b"This excerpt is from the beginning of Dumas's The Count of Monte Cristo")?;
    println!("Success! We just wrote {:?} bytes to the file buffer.", n);

    // Test seeking using 'rw_file'
    let hello = "Hello, Dumas!\n";
    write!(rw_file, "{hello}")?;

    rw_file.rewind()?;

    let mut rw_buf = String::new();
    rw_file.read_to_string(&mut rw_buf)?;
    println!("{}", rw_buf);

    // Now let's see if we can read from a non-readable file or write to a non-writeable file
    let mut w_buffer = [0; 10];
    
    // The line below will cause a compiler error because 'Read' isn't implemented
    // for File<NotGranted, Write>
    // let n = w_file.read(&mut w_buffer[..])?;

    // And this line will cause a compiler error because 'Write' isn't implemented
    // for File<Read, NotGranted>
    // let n = r_file.write(b"These words will appear at the beginning of the file when I'm done")?;

    // However, we can still seek on a file with no permissions at all because we're neither
    // reading nor writing... it's mostly useless but allowed
    file.seek(SeekFrom::Start(42))?;
    file.rewind()?;
    file.seek(SeekFrom::Start(20))?;

    // Uncomment the line below for a compiler error
    // file.write(b"This will never happen.")?;

    Ok(())
}
