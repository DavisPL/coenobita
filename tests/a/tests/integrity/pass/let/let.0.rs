#![allow(unused_variables)]

fn main() {
    #[cnbt::integrity({root}{root})]
    let x = 5;

    #[cnbt::integrity({root}{b,root})]
    let y = x;
}