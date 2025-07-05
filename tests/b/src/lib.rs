#[cnbt::integrity({*}{*} fn[A sub= {a,c} U B, C, D sub= E U F U G]() -> {b}{c})]
pub fn value() -> i32 {
    5
}

// #[cnbt::integrity({*}{*} fn() -> {b}{b})]
// pub fn boolean() -> bool {
//     true
// }