#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::tag({bin}{bin,b,c})]
    let mut x = 5;

    if b::boolean() && c::boolean() {
        x = 6;
    } else {
        x = 7;
    }
}