// struct Baz {
//     #[cnbt::tag({a,b}{a})]
//     a: i64
// }

// fn main() {
//     #[cnbt::tag({a}{a})]
//     let x = Baz { a: 6 };

//     #[cnbt::tag({a}{a,c})]
//     let Baz { a } = x;

//     #[cnbt::tag({a}{a})]
//     let y = a;
// }
