use coenobita::{ Capability, Read, Copy, Move };

fn main() {
    // Manually creates an instance of Capability with read, copy, and move permissions
    let rcm_cap: Capability<
        ((), (), Read, (), (), Copy, Move, ()),
        ((), (), (), (), (), (), (), ()),
        ((), (), (), (), (), (), (), ())
    > = Capability::new("example.txt");
}
