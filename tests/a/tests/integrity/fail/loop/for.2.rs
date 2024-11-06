#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::integrity({root}{root})]
    let mut x = 5;

    #[cnbt::integrity({root,b}{root,b})]
    let items = [1, 2, 3];

    for i in items {
        if c::boolean() {
            x = 1;
        }
    }
}