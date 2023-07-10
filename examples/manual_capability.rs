use coenobita::{ Capability, Read, Copy, Move, NotGranted };

fn main() {
    // Manually creates an instance of Capability with read, copy, and move permissions
    let rcm_cap: Capability<Read, NotGranted, Copy, Move, NotGranted> = Capability::new("examples/files/example.txt");
}
