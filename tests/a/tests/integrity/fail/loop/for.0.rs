#![allow(unused_variables)]

fn main() {
    #[cnbt::integrity({root}{root})]
    let mut x = 5;

    for i in 0..c::value() {
        x += 6;
    }
}