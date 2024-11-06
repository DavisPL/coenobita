#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    let f = |x: i32| x;

    #[cnbt::integrity({root}{root})]
    let y = f(5);
}