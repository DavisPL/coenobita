#[cnbt::flow({a,d}{d} fn({a,d}{d}) -> {a,d}{d})]
fn bazinga(z: i32) -> i32 {
    #[cnbt::flow({d,a}{d})]
    let res = z + 5;
    return res;
}

fn bar() {
    #[cnbt::flow({a,d}{d})]
    let mut x = 5;
    
    #[cnbt::flow({*}{d})]
    let a = x;

    x = bazinga(x);
}