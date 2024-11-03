#![allow(unused_variables)]
#![allow(unused_assignments)]

use c;

fn main() {
    // Since the value assigned to `x` is implicitly affected by
    // crate `c`, this example should fail to type check
    #[cnbt::tag({bin}{bin})]
    let x = if c::boolean() {
        5
    } else {
        6
    };
}