#![allow(unused_variables)]
#![allow(unused_assignments)]

struct BarBar<'a> {
    number: i32,
    
    #[cnbt::integrity({root}{root})]
    string: &'a str
}

fn main() {
    #[cnbt::integrity({root}{root,c})]
    let string = if c::boolean() {
        "something"
    } else {
        "else"
    };

    let b = BarBar {
        number: b::value(),
        string
    };
}