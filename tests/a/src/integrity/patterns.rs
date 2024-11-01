fn test_binding_pattern() {
    let status: Result<i32, i32> = Ok(200);

    #[cnbt::tag({a}{a})]
    let success = 0;

    match status {
        Ok(x) => {

        },
        Err(y) => {

        }
    };

}