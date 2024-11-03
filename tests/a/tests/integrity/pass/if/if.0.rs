#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::tag({bin}{bin,c})]
    let x = if c::boolean() {
        5
    } else {
        6
    };
}