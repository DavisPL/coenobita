#![allow(unused_variables)]
#![allow(unused_assignments)]

enum Foo {
    #[cnbt::tag({bin}{bin} struct ({bin}{bin,c}))]
    Bar(i32),
    
    #[cnbt::tag({bin}{bin} struct ({a}{a}, {b}{b}))]
    Baz(i32, i32)
}

fn main() {
    #[cnbt::tag({bin}{bin})]
    let f = Foo::Bar(5);

    match f {
        Foo::Bar(x) => {
            #[cnbt::tag({bin}{bin,c})]
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