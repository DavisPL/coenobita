#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::integrity({root}{root})]
    let mut x = 5;

    if b::boolean() {
        x = 6;
    } else {
        x = 7;
    }
}