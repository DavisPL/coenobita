use coenobita::{ cap, Capability, Read, Copy, Move, NotGranted };

fn main() {
    // Creates an instance of Capability with read, copy, and move permissions
    let rcm_cap = cap!("examples/files/example.txt" with Read, Copy, Move);
    
    // The line below is equivalent to the one above
    // let rcm_cap: Capability<Read, NotGranted, Copy, Move, NotGranted> = Capability::new("example.txt");
}
