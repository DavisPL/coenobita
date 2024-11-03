#![allow(unused_variables)]
#![allow(unused_assignments)]

use b;

fn main() {
    #[cnbt::tag({bin}{bin})]
    let mut x = 5;
    
    // Since the value assigned to `x` is implicitly affected by
    // crate `b`, this example should fail to type check
    if b::boolean() {
        x = 6;
    } else {
        x = 7;
    }
}