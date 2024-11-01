enum Foo {
    Boo(i32),
    Bar(bool),
    Baz(char)
}

#[cnbt::tag({*}{*} struct ({a}{b}))]
struct Something(i32);

fn test_struct_tuple() {
    //let s = Something(5);
}