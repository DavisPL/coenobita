// #[cnbt::flow({d,e}{d} fn({a,d}{d}) -> {a,d}{d})]
// fn bazinga(z: i32) -> i32 {
//     #[cnbt::flow({d,a}{d})]
//     let res = z + 5;
//     return res;
// }

// #[cnbt::flow({c,d}{*} fn({e}{*}, {e}{*}) -> {d,e}{*} ({e}{*}, {d,e}{*}))]
// pub fn f(a: i32, b: i32) -> (i32, i32) {
//     (a, a + 5)
// }

// fn bar() {
//     // #[cnbt::flow({a,d}{d})]
//     // let mut x = 5;
    
//     // #[cnbt::flow({*}{d})]
//     // let a = x;

//     // x = bazinga(x);


//     #[cnbt::flow({a,d}{b,d})]
//     let x = 5;

//     #[cnbt::flow({d}{b,d})]
//     let y = if x == 3 {
//          10
//     } else {
//         15
//     };
// }

use std::{fs, path::{Path, PathBuf}};

struct Boo;

fn bar<B: AsRef<str>>(some: B) -> B {
    some
}

fn bazinga() {
    let p = "hello.txt";

    let mut path = PathBuf::from(p);

    path.as_mut_os_str().make_ascii_lowercase();

    let _ = fs::read(path);

    bar("dud");
}

fn main() {
    bazinga();
    println!("Hello, world!");
}
