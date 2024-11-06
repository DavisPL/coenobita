#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::integrity({root}{*})]
    let mut x = 5;

    let items = [1, 2, 3];

    for i in items {
        x = 1;
    }
}