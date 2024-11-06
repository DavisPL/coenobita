#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::integrity({*}{*} fn({root}{root}, {root}{root}) -> {root}{root,b,c})]
    let f = if b::boolean() && c::boolean() {
        |x: i32, y: i32| x 
    } else {
        |x: i32, y: i32| y
    };

    #[cnbt::integrity({root}{root,b,c})]
    let y = f(5, 6);
}