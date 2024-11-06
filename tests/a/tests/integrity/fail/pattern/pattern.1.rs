#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::integrity({*}{*} ({root}{root}, {b}{b}))]
    let (x, mut y) = (5, b::boolean());

    y = false;
}