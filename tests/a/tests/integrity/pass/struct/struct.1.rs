#![allow(unused_variables)]
#![allow(unused_assignments)]

struct BarBar<'a> {
    number: i32,
    
    #[cnbt::tag({bin}{bin,c})]
    string: &'a str
}

fn main() {
    #[cnbt::tag({bin}{bin,c})]
    let string = if c::boolean() {
        "something"
    } else {
        "else"
    };

    let b = BarBar {
        number: b::value(),
        string
    };

    #[cnbt::tag({*}{*})]
    let y = b.string;
}