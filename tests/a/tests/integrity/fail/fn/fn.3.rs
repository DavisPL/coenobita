#![allow(unused_variables)]
#![allow(unused_assignments)]

#[cnbt::tag({*}{*} fn({a,bin}{a,bin}) -> {a,bin}{a,bin})]
fn id(x: i32) -> i32 {
    x
}

fn main() {
    for i in 0..c::value() {
        let _ = id(5);
    }
}