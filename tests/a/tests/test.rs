#[test]
fn test() {
    let t = trybuild::TestCases::new();

    t.compile_fail("tests/integrity/fail/*/*.rs");
    t.pass("tests/integrity/pass/*/*.rs");
}