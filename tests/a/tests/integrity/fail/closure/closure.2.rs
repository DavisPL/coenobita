#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::integrity({*}{*} fn({bin}{bin}) -> {bin}{bin})]
    let f = |x: i32| x + 1;

    #[cnbt::integrity({b}{b})]
    let y = f(5);
}