#![allow(unused_variables)]
#![allow(unused_assignments)]

#[cnbt::integrity({*}{*} fn({a,root}{a,c,root}) -> {a,root}{a,c,root})]
fn id(x: i32) -> i32 {
    x
}

fn main() {
    for i in 0..c::value() {
        let _ = id(5);
    }
}