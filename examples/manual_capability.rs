use coenobita::{CapBuf, Copy, Move, Read};

fn main() {
    // Manually creates an instance of CapBuf with read, copy, and move permissions
    let _rcm_cap: CapBuf<
        ((), (), Read, (), (), Copy, Move, ()),
        ((), (), (), (), (), (), (), ()),
        ((), (), (), (), (), (), (), ()),
    > = CapBuf::new("example.txt");
}
