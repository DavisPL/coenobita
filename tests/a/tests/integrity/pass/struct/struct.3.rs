#![allow(unused_variables)]
#![allow(unused_assignments)]

struct BarBar<'a> {
    number: i32,
    
    #[cnbt::integrity({root}{root})]
    string: &'a str
}

fn main() {
    #[cnbt::integrity({root}{root})]
    let string = "something";

    #[cnbt::integrity({root}{root} struct { number: {*}{*}, string: {root}{root} })]
    let b = BarBar {
        number: b::value(),
        string
    };

    #[cnbt::integrity({root}{root})]
    let y = b.string;
}