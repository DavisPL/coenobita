#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::integrity({bin}{bin,b,c})]
    let mut x = 5;

    #[cnbt::integrity({bin,b}{bin,b})]
    let items = [1, 2, 3];

    for i in items {
        if c::boolean() {
            x = 1;
        }
    }
}