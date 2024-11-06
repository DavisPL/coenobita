#![allow(unused_variables)]

fn main() {
    #[cnbt::integrity({root}{b,root})]
    let x = 5;

    // We shouldn't be able to assign a value with implicit flow
    // set {root} to a variable with implicit flow set {b,root}
    #[cnbt::integrity({root}{root})]
    let y = x;
}