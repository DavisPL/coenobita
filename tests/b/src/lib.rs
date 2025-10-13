#[coenobita::take(A sub= {a,b,c} U {b})]
#[coenobita::take(B sub= {c} U A)]

#[coenobita::pass(x A)]
#[coenobita::pass(x {a})]
#[coenobita::pass(x {a})]

#[coenobita::pass(y B)]
#[coenobita::pass(y {b})]
#[coenobita::pass(y {b})]

#[coenobita::pass(-> {b} U A)]
#[coenobita::pass(-> {a,b,c})]
#[coenobita::pass(-> {a,b,c})]
pub fn foo(x: Vec<i32>, y: Vec<i64>) -> i32 {
    fn baz(z: i32) {
        
    }

    5
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