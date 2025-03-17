use std::{ffi::OsStr, path::Path};

type P = fn(&OsStr) -> &Path;

fn foo<'a, S: AsRef<OsStr>>(path_new: P) {
    let s = "hello.txt";
    let p = path_new(s.as_ref());
}