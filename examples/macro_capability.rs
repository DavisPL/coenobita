use coenobita::cap;

fn main() {
    // Creates an instance of CapBuf with read, copy, and move permissions
    let _rcm_cap = cap!("example.txt" with (Read, Copy, Move));

    // The line below is equivalent to the one above
    // let rcm_cap: CapBuf<((), (), Read, (), (), Copy, Move, ()), ((), ..., ()), ((), ..., ())> = CapBuf::new("example.txt");
}
