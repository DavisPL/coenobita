enum Foo {
    Boo(i32),

    #[cnbt::tag({b}{b} struct ({b}{b}))]
    Bar(bool),
    Baz(char)
}

#[cnbt::tag({a}{a} struct ({a}{a,b}))]
struct Something(i32);

fn test_struct_tuple() {
    #[cnbt::tag({*}{*} struct ({a}{a,b}))]
    let s = Something(5);

    #[cnbt::tag({a}{a,b})]
    let x = s.0;
}

fn test_ok() {
    let ok = Foo::Bar(b::boolean());

    let Foo::Bar(x) = ok else { panic!() };

    #[cnbt::tag({b}{b})]
    let y = x;
}