#![allow(unused_variables)]
#![allow(unused_assignments)]

enum Foo {
    #[cnbt::integrity({bin}{bin} struct ({bin}{bin,c}))]
    Bar(i32),
    
    #[cnbt::integrity({bin}{bin} struct ({a}{a}, {b}{b}))]
    Baz(i32, i32)
}

fn main() {
    #[cnbt::integrity({bin}{bin})]
    let f = Foo::Bar(5);

    match f {
        Foo::Bar(x) => {
            #[cnbt::integrity({bin}{bin,c})]
            let x_ = x;
        }

        Foo::Baz(y, z) => {
            #[cnbt::integrity({a}{a})]
            let y_ = y;

            #[cnbt::integrity({b}{b})]
            let z_ = z;
        }
    }
}