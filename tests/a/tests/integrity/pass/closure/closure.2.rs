#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::integrity({*}{*} fn({root}{root}) -> {root}{root})]
    let f = |x: i32| x + 1;

    #[cnbt::integrity({b,root}{b,root})]
    let y = f(5);
}