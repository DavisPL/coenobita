fn test_id_closure() {
    #[cnbt::tag({a}{a} fn({a}{*}) -> {a}{*})]
    let x = |y: i32| y;

    #[cnbt::tag({a}{*})]
    let z = x(5);
}