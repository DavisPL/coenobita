#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::tag({*}{*} fn({a}{a}) -> {a,b}{a,b})]
    let f = |x: i32| x;
}
