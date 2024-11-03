// mod if_else;
// // // mod loops;
// // // mod unorganized;
// // // mod methods;
// // // mod enums;
// // // mod closures;

// // // enum Foo {
// // //     Boo(i32),

// // //     #[cnbt::tag({b}{b} struct ({b}{b}))]
// // //     Bar(bool),
// // //     Baz(char)
// // // }

// // // fn test_ok() {
// // //     let ok = Foo::Bar(b::boolean());

// // //     let Foo::Bar(x) = ok else { panic!() };

// // //     #[cnbt::tag({a,b}{b,a})]
// // //     let y = x;

// // //     #[cnbt::tag({*}{*} ({b}{b}, {c}{c}))]
// // //     let (b, c) = (b::value(), c::value());

// // //     #[cnbt::tag({b}{b})]
// // //     let bb = b;

// // //     #[cnbt::tag({c}{c})]
// // //     let bb = c;
// // // }

// // fn f() {
// //     #[cnbt::tag({*}{*} [{*}{*}])]
// //     let mut vs = [1, 2, 3];

// //     #[cnbt::tag({*}{*})]
// //     let cond: Result<i32, i32> = Ok(5);

// //     for v in vs.iter_mut() {
// //         #[cnbt::tag({a}{*})]
// //         let x = 6;
// //         *v = x;

// //         match cond {
// //             Ok(x) => {
// //                 *v = x;
// //             }

// //             Err(_) => {}
// //         }
// //     }
// // }

// fn test_some() {
//     #[cnbt::tag({a}{a})]
//     let mut x = 5;

//     if c::boolean() {
//         x = 6;
//     } else {
//         x = 7;
//     }
// }
