#![allow(unused_variables)]

fn main() {
    #[cnbt::integrity({c,bin}{bin})]
    let x = 5;

    // We shouldn't be able to assign a value with explicit flow
    // set {bin} to a variable with explicit flow set {c,bin}
    #[cnbt::integrity({bin}{bin})]
    let y = x;
}