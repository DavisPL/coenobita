
// enum Foo {
//     #[cnbt::tag({a}{a} struct ({a}{a}))]
//     Bar(i32),
    
//     #[cnbt::tag({a}{a} struct ({aa}{aa}, {b}{b}))]
//     Baz(i32, i32)
// }

// fn main() {
//     #[cnbt::tag({a}{a,b})]
//     let f = Foo::Bar(5);

//     match f {
//         Foo::Bar(x) => {
//             #[cnbt::tag({a}{a,b})]
//             let x_ = 6;
//         }

//         Foo::Baz(y, z) => {
//             #[cnbt::tag({a}{a})]
//             let y_ = y;

//             #[cnbt::tag({b}{b})]
//             let z_ = z;
//         }
//     }
// }