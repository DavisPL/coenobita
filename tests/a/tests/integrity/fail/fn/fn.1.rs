#![allow(unused_variables)]
#![allow(unused_assignments)]

#[cnbt::tag({*}{*} fn({a}{a}) -> {a}{a})]
fn id(x: i32) -> i32 {
    x
}

fn main() {
    let x = id(5);
}