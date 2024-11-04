#![allow(unused_variables)]
#![allow(unused_assignments)]

struct Baz {
    #[cnbt::tag({bin}{bin})]
    a: i64
}

fn main() {
    #[cnbt::tag({bin}{bin})]
    let x = Baz { a: 6 };

    #[cnbt::tag({bin}{bin})]
    let Baz { a } = x;

    #[cnbt::tag({bin}{bin})]
    let y = a;
}