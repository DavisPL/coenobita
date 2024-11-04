#![allow(unused_variables)]
#![allow(unused_assignments)]

struct Baz {
    #[cnbt::integrity({bin}{bin})]
    a: i64
}

fn main() {
    #[cnbt::integrity({bin}{bin})]
    let x = Baz { a: 6 };

    #[cnbt::integrity({bin}{bin})]
    let Baz { a } = x;

    #[cnbt::integrity({bin}{bin})]
    let y = a;
}