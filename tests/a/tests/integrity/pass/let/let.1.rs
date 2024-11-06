#![allow(unused_variables)]

fn main() {
    #[cnbt::integrity({root}{root})]
    let x = 5;

    #[cnbt::integrity({c,root}{root})]
    let y = x;
}