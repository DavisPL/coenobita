
trait Bazinga {
    #[cnbt::integrity({*}{*} fn({b}{b}) -> {b,c}{b,c})]
    fn something(x: i32) -> i32;
}

struct Foobar;

impl Bazinga for Foobar {
    fn something(x: i32) -> i32 {
        x + c::value()
    }
}

impl Foobar {
    #[cnbt::integrity({*}{*} fn({b}{b}) -> {b,c}{b,c})]
    fn another(x: i32) -> i32 {
        x + c::value()
    }
}

fn _test_trait() {
    let f = Foobar;

    #[cnbt::integrity({*}{a,b})]
    let z = Foobar::something(5);

    #[cnbt::integrity({root}{root})]
    let z = Foobar::another(5);

}