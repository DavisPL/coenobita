#![allow(unused_variables)]
#![allow(unused_assignments)]

#[cnbt::integrity({*}{*} fn({b}{b}) -> {b}{b})]
fn id(x: i32) -> i32 {
    x
}

fn main() {
    let x = id(b::value());
}