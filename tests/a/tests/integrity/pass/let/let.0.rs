#![allow(unused_variables)]

fn main() {
    #[cnbt::integrity({bin}{bin})]
    let x = 5;

    #[cnbt::integrity({bin}{b,bin})]
    let y = x;
}