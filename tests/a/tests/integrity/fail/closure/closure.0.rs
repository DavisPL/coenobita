#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    let f = |x: i32| x;

    #[cnbt::integrity({bin}{bin})]
    let y = f(5);
}