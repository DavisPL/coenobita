#![allow(unused_variables)]
#![allow(unused_assignments)]

struct BarBar<'a> {
    number: i32,
    
    #[cnbt::integrity({root}{root,c})]
    string: &'a str
}

fn main() {
    #[cnbt::integrity({root}{root,c})]
    let string = if c::boolean() {
        "something"
    } else {
        "else"
    };

    #[cnbt::integrity({root}{root} struct { number: {*}{*}, string: {root}{root,c} })]
    let b = BarBar {
        number: b::value(),
        string
    };

    #[cnbt::integrity({root}{root})]
    let y = b.string;
}