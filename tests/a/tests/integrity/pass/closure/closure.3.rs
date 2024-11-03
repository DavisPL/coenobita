#![allow(unused_variables)]
#![allow(unused_assignments)]

fn main() {
    #[cnbt::tag({*}{*} fn({bin}{bin}, {bin}{bin}) -> {bin}{bin,b,c})]
    let f = if b::boolean() && c::boolean() {
        |x: i32, y: i32| x 
    } else {
        |x: i32, y: i32| y
    };

    #[cnbt::tag({bin}{bin,b,c})]
    let y = f(5, 6);
}