// // #[coenobita::take(A sub= {a,b,c} U {b})]
// // #[coenobita::take(B sub= {c} U A)]
// // #[coenobita::take(C sub= B)]

// // #[coenobita::pass(x A)]
// // #[coenobita::pass(x {a})]
// // #[coenobita::pass(x {a})]

// // #[coenobita::pass(y C)]
// // #[coenobita::pass(y {b})]
// // #[coenobita::pass(y {b})]

// // #[coenobita::pass(-> {b} U B)]
// // #[coenobita::pass(-> {a,b,c})]
// // #[coenobita::pass(-> {a,b,c})]
// // pub fn foo(x: i32, y: i32) -> Foo<i32> {

// //     let f = Foo { x: 5 };

// //     f
// // }

// // #[coenobita::take(A sub= {a,b,c})]
// // struct Foo<T> {
// //     #[coenobita::pass(x A)]
// //     x: T
// // }

// // #[cnbt::ty(fn[A]() -> i32 ({b},{b},{a,b,c,d}) ({a},{a},{a}))]
// // fn bazinga() {
// //     #[cnbt::ty(i32 ({b},{b},{b,a}))]
// //     let mut x = 5;

// //     #[cnbt::ty(i32 ({b},{b},{b,a}))]
// //     let y = 6;

// //     x = y;
// // }

// // #[cnbt::integrity({*}{*} fn() -> {b}{b})]
// // pub fn boolean() -> bool {
// //     true
// // }

// // #[cnbt::ty()]
// // pub fn foo(x: i32) {



// // }

// fn bar() -> i32 {
//     5
// }

// #[coenobita::take(C sub= {a,b,c})]
// #[coenobita::take(B sub= C)]
// #[coenobita::take(A sub= B)]

// #[coenobita::take(F sub= {a,b,c})]
// #[coenobita::take(E sub= F)]
// #[coenobita::take(D sub= E)]

// #[coenobita::pass(x A)]
// #[coenobita::pass(x B)]
// #[coenobita::pass(x C)]

// #[coenobita::pass(y D)]
// #[coenobita::pass(y E)]
// #[coenobita::pass(y F)]

// #[coenobita::pass(-> A U D)]
// #[coenobita::pass(-> B U E)]
// #[coenobita::pass(-> C U F U {b})]
// fn foo(x: i32, y: i32) -> i32 {
//     // #[coenobita::pass(z {a,b,c})]
//     // #[coenobita::pass(z {a,b,c})]
//     // #[coenobita::pass(z {a,b,c})]
//     // let z = y + x;

//     #[coenobita::pass(z {b})]
//     #[coenobita::pass(z {b})]
//     #[coenobita::pass(z {b})]
//     let mut z = 5;

//     if x == y {
//         z = 6;
//     } else {
//         z = 7;
//     };

//     let a = bar();

//     y + x
// }

struct Zap {
  #[coenobita::pass(_ {a})]
    x: i64
}

struct Bing {
    #[coenobita::pass(_ {a, b})]
    x: i64
}

#[coenobita::take(A sub= {a,b,c})]
#[coenobita::pass(0 A)]
#[coenobita::pass(0 A)]
#[coenobita::pass(0 A)]

#[coenobita::pass(-> A)]
#[coenobita::pass(-> A)]
#[coenobita::pass(-> A U {b})]
fn foo(x: i32) -> i32 {
    x
}

fn bar(zap: &mut Zap) -> Bing {
    // ERROR: expected '{a} {*} {*}' but found '{b} {b} {b}'
    // zap.x = 6;

    #[coenobita::pass(_ {a,b,c})]
    #[coenobita::pass(_ {a,b,c})]
    #[coenobita::pass(_ {a,b,c})]
    let z = 5;

    #[coenobita::pass(_ {a,b,c})]
    let f = foo(z);

    let mut b = Bing { x: 5 };

    b.x = 6;

    b
}