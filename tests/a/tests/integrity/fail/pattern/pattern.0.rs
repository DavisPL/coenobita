#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::integrity({*}{*} ({bin}{bin}, {b}{b}))]
    let (x, y) = (5, 6);
}