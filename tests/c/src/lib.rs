#[cnbt::integrity({*}{*} fn() -> {c}{c})]
#[cnbt::provenance((*, *) fn() -> (c,c))]
pub fn value() -> i32 {
    5
}

#[cnbt::integrity({*}{*} fn() -> {c}{c})]
pub fn boolean() -> bool {
    true
}