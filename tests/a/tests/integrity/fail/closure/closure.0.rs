#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    let f = |x: i32| x;

    #[cnbt::tag({bin}{bin})]
    let y = f(5);
}