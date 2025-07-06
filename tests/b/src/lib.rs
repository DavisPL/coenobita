#[cnbt::ty(fn[A sub= {a,b}, B sub= {c}, C sub= A U B](i32 ({b},{b},C)) -> i32 ({b},{b},{a,b,c,d}) ({a},{a},{a}))]
pub fn foo(x: i32) -> i32 {
    
    #[cnbt::ty(fn(i32 ({b},{b},C U {b})) -> i32 ({b},{b},{a,b,c,d}) ({a},{a},{a}))]
    fn bar(y: i32) -> i32 {
        y
    }

    bar(x)
}

// #[cnbt::ty(fn[A]() -> i32 ({b},{b},{a,b,c,d}) ({a},{a},{a}))]
// fn bazinga() {
//     #[cnbt::ty(i32 ({b},{b},{b,a}))]
//     let mut x = 5;

//     #[cnbt::ty(i32 ({b},{b},{b,a}))]
//     let y = 6;

//     x = y;
// }

// #[cnbt::integrity({*}{*} fn() -> {b}{b})]
// pub fn boolean() -> bool {
//     true
// }

// #[cnbt::ty()]
// pub fn foo(x: i32) {



// }