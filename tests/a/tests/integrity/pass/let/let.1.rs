#![allow(unused_variables)]

fn main() {
    #[cnbt::integrity({bin}{bin})]
    let x = 5;

    #[cnbt::integrity({c,bin}{bin})]
    let y = x;
}