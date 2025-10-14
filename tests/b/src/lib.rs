#[coenobita::take(A sub= {a,b,c} U {b})]
#[coenobita::take(B sub= {c} U A)]
#[coenobita::take(C sub= B)]

#[coenobita::pass(x A)]
#[coenobita::pass(x {a})]
#[coenobita::pass(x {a})]

#[coenobita::pass(y C)]
#[coenobita::pass(y {b})]
#[coenobita::pass(y {b})]

#[coenobita::pass(-> {b} U B)]
#[coenobita::pass(-> {a,b,c})]
#[coenobita::pass(-> {a,b,c})]
pub fn foo(x: i32, y: i32) -> Foo<i32> {

    let f = Foo { x: 5 };

    f
}

#[coenobita::take(A sub= {a,b,c})]
struct Foo<T> {
    #[coenobita::pass(x A)]
    x: T
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