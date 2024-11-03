#![allow(unused_variables)]
#![allow(unused_assignments)]

enum Foo {
    #[cnbt::tag({bin}{bin} struct ({bin}{bin}))]
    Bar(i32),
    
    #[cnbt::tag({bin}{bin} struct ({a}{a}, {b}{b}))]
    Baz(i32, i32)
}

fn main() {
    let f = Foo::Bar(5);

    match f {
        Foo::Bar(x) => {
            #[cnbt::tag({bin}{bin})]
            let x_ = x;
        }

        Foo::Baz(y, z) => {
            #[cnbt::tag({a}{a})]
            let y_ = y;

            #[cnbt::tag({b}{b})]
            let z_ = z;
        }
    }
}