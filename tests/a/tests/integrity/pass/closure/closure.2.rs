#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::tag({*}{*} fn({bin}{bin}) -> {bin}{bin})]
    let f = |x: i32| x + 1;

    #[cnbt::tag({b,bin}{b,bin})]
    let y = f(5);
}