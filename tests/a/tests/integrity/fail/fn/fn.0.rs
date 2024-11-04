#![allow(unused_variables)]
#![allow(unused_assignments)]

#[cnbt::integrity({*}{*} fn({a}{a}) -> {b}{b})]
fn id(x: i32) -> i32 {
    x
}

fn main() {
    let x = id(5);
}