use coenobita::{ cap };

fn main() {
    // Creates an instance of Capability with read, copy, and move permissions
    let rcm_cap = cap!("example.txt" with (Read, Copy, Move));

    // The line below is equivalent to the one above
    // let rcm_cap: Capability<((), (), Read, (), (), Copy, Move, ()), ((), ..., ()), ((), ..., ())> = Capability::new("example.txt");
}
