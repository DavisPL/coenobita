#![allow(unused_variables)]
#![allow(unused_assignments)]

#[cnbt::integrity({*}{*} fn({a,bin}{a,bin}) -> {a,bin}{a,bin})]
fn id(x: i32) -> i32 {
    x
}

fn main() {
    #[cnbt::integrity({bin}{bin})]
    let x = id(5);
}