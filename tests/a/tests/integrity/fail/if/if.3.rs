#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::integrity({bin}{bin})]
    let mut x = 5;
    
    // Since the value assigned to `x` is implicitly affected by
    // crates `b` and `c`, this example should fail to type check
    if b::boolean() && c::boolean() {
        x = 6;
    } else {
        x = 7;
    }
}