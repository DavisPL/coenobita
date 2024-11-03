fn test_id_closure() {
    #[cnbt::tag({a}{a,c} fn({a}{a}) -> {a}{a,c})]
    let x = if c::boolean() {
        |y: i32| y
    } else {
        |z: i32| { z + 1 }
    };

    #[cnbt::tag({a}{a,c})]
    let z = x(5);
}