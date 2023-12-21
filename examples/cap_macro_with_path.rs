use coenobita::cap;
use std::path::PathBuf;

fn main() {
    let file_path = PathBuf::from(r"C:\windows\system32.dll");

    // We create a CapBuf for the above path with direct delete permissions
    let file_cap = cap!(&file_path with (Delete));

    println!("{:?}", file_cap);
    println!("{:?}", file_cap.display());

    // We can also convert the Cap stored in file_cap to a CapBuf
    println!("{:?}", file_cap.to_cap_buf());
}
