#![allow(unused_variables)]

fn main() {
    #[cnbt::integrity({c,root}{root})]
    let x = 5;

    // We shouldn't be able to assign a value with explicit flow
    // set {root} to a variable with explicit flow set {c,root}
    #[cnbt::integrity({root}{root})]
    let y = x;
}