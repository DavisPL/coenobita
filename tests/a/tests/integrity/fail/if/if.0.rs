#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    // Since the value assigned to `x` is implicitly affected by
    // crate `c`, this example should fail to type check
    #[cnbt::integrity({bin}{bin})]
    let x = if c::boolean() {
        5
    } else {
        6
    };
}