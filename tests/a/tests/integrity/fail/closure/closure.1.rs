#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::integrity({*}{*} fn({a}{a}) -> {b}{b})]
    let f = |x: i32| x;
}
