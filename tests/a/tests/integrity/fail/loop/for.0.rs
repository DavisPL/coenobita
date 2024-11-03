#![allow(unused_variables)]

fn main() {
    #[cnbt::tag({bin}{bin})]
    let mut x = 5;

    for i in 0..c::value() {
        x += 6;
    }
}