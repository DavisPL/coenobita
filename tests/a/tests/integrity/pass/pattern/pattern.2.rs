#![allow(unused_variables)]
#![allow(unused_assignments)]

struct Baz {
    #[cnbt::integrity({root}{root})]
    a: i64
}

fn main() {
    #[cnbt::integrity({root}{root})]
    let x = Baz { a: 6 };

    #[cnbt::integrity({root}{root})]
    let Baz { a } = x;

    #[cnbt::integrity({root}{root})]
    let y = a;
}