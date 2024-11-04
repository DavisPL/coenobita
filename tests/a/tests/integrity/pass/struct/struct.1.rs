#![allow(unused_variables)]
#![allow(unused_assignments)]

struct BarBar<'a> {
    number: i32,
    
    #[cnbt::integrity({bin}{bin,c})]
    string: &'a str
}

fn main() {
    #[cnbt::integrity({bin}{bin,c})]
    let string = if c::boolean() {
        "something"
    } else {
        "else"
    };

    let b = BarBar {
        number: b::value(),
        string
    };

    #[cnbt::integrity({*}{*})]
    let y = b.string;
}