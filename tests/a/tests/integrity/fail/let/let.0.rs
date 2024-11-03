#![allow(unused_variables)]

fn main() {
    #[cnbt::tag({bin}{b,bin})]
    let x = 5;

    // We shouldn't be able to assign a value with implicit flow
    // set {bin} to a variable with implicit flow set {b,bin}
    #[cnbt::tag({bin}{bin})]
    let y = x;
}