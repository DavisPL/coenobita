#[test]
fn integrity() {
    let t = trybuild::TestCases::new();

    t.compile_fail("tests/integrity/fail/*/*.rs");
    t.pass("tests/integrity/pass/*/*.rs");
}

#[test]
fn provenance() {
    let t = trybuild::TestCases::new();

    t.compile_fail("tests/provenance/fail/*/*.rs");
    t.pass("tests/provenance/pass/*/*.rs");
}
