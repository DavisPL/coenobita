#![allow(unused_variables)]
#![allow(unused_assignments)]

struct Baz {
    #[cnbt::tag({bin}{bin})]
    a: i64
}

fn main() {
    let x = Baz { a: 6 };

    let Baz { a } = x;

    #[cnbt::tag({bin}{bin})]
    let y = a;
}