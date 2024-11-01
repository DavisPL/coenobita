enum Foo {
    Boo(i32),
    Bar(bool),
    Baz(char)
}

#[cnbt::tag({a}{a} struct ({a}{b}))]
struct Something(i32);

fn test_struct_tuple() {
    let s = Something(5);

    #[cnbt::tag({a}{a,b})]
    let x = s.0;
}

fn test_ok() {
    let ok = Foo::Bar(true);
}