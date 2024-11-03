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

    #[cnbt::tag({bin}{bin} struct { number: {*}{*}, string: {bin}{bin,c} })]
    let b = BarBar {
        number: b::value(),
        string
    };

    #[cnbt::tag({bin}{bin,c})]
    let y = b.string;
}