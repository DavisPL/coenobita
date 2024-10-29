fn test_basic_for_loop() {
    #[cnbt::tag({a}{a})]
    let mut x = 5;

    #[cnbt::tag({a}{a} [{a}{a}])]
    let v = [1, 2];

    for i in v {
        x = 6;
    }
}

fn test_basic_while_loop() {
    #[cnbt::tag({a}{a})]
    let mut x = 5;

    while true && false {
        x = 6;
    }

    // This should fail
    // while c::boolean() {
    //     x = 7;
    // }
}