#[cnbt::provenance((*,*) fn() -> (b,b))]
#[cnbt::integrity({*}{*} fn() -> {b}{b})]
pub fn value() -> i32 {
    5
}

#[cnbt::provenance((*,*) fn() -> (b,b))]
#[cnbt::integrity({*}{*} fn() -> {b}{b})]
pub fn boolean() -> bool {
    true
}