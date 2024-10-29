fn test_basic_provenance() {
    #[cnbt::provenance((*, *))]
    let x = 5;

    // This should fail
    // #[cnbt::provenance((a, b))]
    // let b = x;
}

fn test_fn_id() {
    #[cnbt::provenance((*, *) fn((a, a)) -> (a, a))]
    fn foo(x: i32) -> i32 {
        x
    }
}

fn test_fn_iddd() {
    #[cnbt::provenance((*, *) fn((c, a)) -> (c, a))]
    fn id(x: i32) -> i32 {

        #[cnbt::provenance((*, *) fn((c, a)) -> (c, a))]
        fn idd(y: i32) -> i32 {

            #[cnbt::provenance((*, *) fn((c, a)) -> (c, a))]
            fn iddd(z: i32) -> i32 {
                z
            }

            iddd(y)
        }

        idd(x)
    }

    // This should fail
    // id(5);

    id(c::value());
}