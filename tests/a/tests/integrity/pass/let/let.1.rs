#![allow(unused_variables)]

fn main() {
    #[cnbt::tag({bin}{bin})]
    let x = 5;

    #[cnbt::tag({c,bin}{bin})]
    let y = x;
}