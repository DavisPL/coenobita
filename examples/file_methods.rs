use coenobita::cap;
use coenobita::fs::File;

fn main() -> std::io::Result<()> {
    // Creates an instance of Capability with read, copy, and move permissions
    let rcm_cap = cap!("examples/files/example.txt" with (Read, View, Copy, Move));
    let cwd_cap = cap!("examples/files/example_copied.txt" with (Delete, Write, Create));

    // The line below fails because it expects a Write permission
    // let file = File::create(&rcm_cap);

    // This line succeeds since all permissions are given
    let file = File::create(&cwd_cap)?;
    println!("{:?}", file);

    // Now we'll open the other file and read its metadata
    let readonly_file = File::open(&rcm_cap)?;
    let metadata = readonly_file.metadata()?;
    println!("{:?}", metadata);

    Ok(())
}
