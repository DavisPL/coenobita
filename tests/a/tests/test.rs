#[test]
fn test() {
    let t = trybuild::TestCases::new();

    // t.compile_fail("tests/integrity/fail/*/*.rs");
    // t.pass("tests/integrity/pass/*/*.rs");

    t.compile_fail("tests/provenance/fail/*/*.rs");
    t.pass("tests/provenance/pass/*/*.rs");
}
