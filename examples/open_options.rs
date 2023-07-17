use coenobita::{ cap, Capability, Read, Write, Copy, Move, NotGranted };
use coenobita::fs::OpenOptions;

fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

fn main() {
    // Creates two instances of Capability
    let r_cap = cap!("examples/files/example.txt" with Read);
    let rw_cap = cap!("examples/files/example.txt" with Read, Write);
    
    // Opens a file for each capability
    //let r_file = OpenOptions::open(&r_cap);
    //let rw_file = OpenOptions::open(&rw_cap);

    // Print each file's debug representation
    //println!("{:?}", r_file.unwrap());
    //println!("{:?}", rw_file.unwrap());

    
    let r_oo = OpenOptions::new()
        .read(true)
        .write(true);

    print_type_of(&r_oo);
}
