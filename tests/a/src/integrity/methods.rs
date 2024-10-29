struct Foo{ y: i32 }

impl Foo {
    #[cnbt::tag({*}{a} fn({*}{*}, {a}{a}) -> {a}{a})]
    fn bar(&self, x: i32) -> i32 {
        x
    }
}

fn test_basic_method() {
    let f = Foo { y: 7 };

    f.bar(5);

    // This should fail
    // f.bar(c::value() + 5);
}