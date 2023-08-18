use coenobita::{ cap, Capability, Read, Write, Copy, Move, Delete };
use coenobita::fs;
use std::path::PathBuf;

fn main() {
    let file_path = PathBuf::from(r"C:\windows\system32.dll");

    let file_cap = cap!(file_path with Delete);
}
