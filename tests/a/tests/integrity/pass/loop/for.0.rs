#![allow(unused_variables)]

fn main() {
    #[cnbt::integrity({bin}{bin,c})]
    let mut x = 5;

    for i in 0..c::value() {
        x += 6;
    }
}