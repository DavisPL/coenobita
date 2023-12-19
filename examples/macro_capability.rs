use coenobita::cap;

fn main() {
    // Creates capability slice with read, copy, and move permissions
    let _rcm_cap = cap!("example.txt" with (Read, Copy, Move));

    // The line below is equivalent to the one above
    // let _rcm_cap: Cap<((), (), Read, (), (), Copy, Move, ()), ((), ..., ()), ((), ..., ())> = Cap::new("example.txt");
}
