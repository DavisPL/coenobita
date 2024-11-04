#![allow(unused_variables)]
#![allow(unused_assignments)]

struct BarBar<'a> {
    number: i32,
    
    #[cnbt::integrity({bin}{bin})]
    string: &'a str
}

fn main() {
    #[cnbt::integrity({bin}{bin})]
    let string = "something";

    let b = BarBar {
        number: b::value(),
        string
    };
}