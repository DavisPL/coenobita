#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::tag({bin}{bin})]
    let mut x = 5;

    if b::boolean() {
        x = 6;
    } else {
        x = 7;
    }
}