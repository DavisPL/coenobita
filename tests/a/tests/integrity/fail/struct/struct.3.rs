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

    #[cnbt::integrity({bin}{bin} struct { number: {*}{*}, string: {bin}{bin,c} })]
    let b = BarBar {
        number: b::value(),
        string
    };

    #[cnbt::integrity({bin}{bin})]
    let y = b.string;
}