#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::integrity({root}{root,b,c})]
    let mut x = 5;
    
    // Since the value assigned to `x` is implicitly affected by
    // crates `b` and `c`, this example should fail to type check
    if b::boolean() {
        if c::boolean() {
            x = 6;
        } else {
            x = 7;
        }
    } else {
        x = 8;
    }
}