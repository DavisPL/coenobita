#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::tag({bin}{bin})]
    let mut x = 5;

    #[cnbt::tag({bin,b}{bin,b})]
    let items = [1, 2, 3];

    for i in items {
        if c::boolean() {
            x = 1;
        }
    }
}