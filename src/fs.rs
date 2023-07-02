use crate::{ Capability, Read, Write, Copy, Move, Delete, NotGranted };

use std::path;
use std::{ fs, io };

pub fn canonicalize<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<path::PathBuf> {
    fs::canonicalize(cap.get_path())
}

pub fn copy<A, B, C, D, E, F, G, H>
(from: &Capability<A, B, Copy, C, D>, to: &Capability<E, Write, F, G, H>) -> io::Result<u64> {
    fs::copy(from.get_path(), to.get_path())
}

pub fn create_dir<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<()> {
    unimplemented!();
}

pub fn create_dir_all<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<()> {
    unimplemented!();
}

pub fn hard_link<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<()> {
    unimplemented!();
}

pub fn metadata<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<fs::Metadata> {
    unimplemented!();
}

pub fn read<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<Vec<u8>> {
    fs::read(cap.get_path())
}

pub fn read_dir<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<fs::ReadDir> {
    unimplemented!();
}

pub fn read_link<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<path::PathBuf> {
    unimplemented!();
}

pub fn read_to_string<A, B, C, D>
(cap: &Capability<A, Write, B, C, D>) -> io::Result<String> {
    fs::read_to_string(cap.get_path())
}

pub fn remove_dir<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<()> {
    unimplemented!();
}

pub fn remove_dir_all<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<()> {
    unimplemented!();
}

pub fn remove_file<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<()> {
    unimplemented!();
}

pub fn rename<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<()> {
    unimplemented!();
}

pub fn set_permissions<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<()> {
    unimplemented!();
}

pub fn soft_link<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<()> {
    unimplemented!();
}

pub fn symlink_metadata<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<fs::Metadata> {
    unimplemented!();
}

pub fn try_exists<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<bool> {
    unimplemented!();
}

pub fn write<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<()> {
    unimplemented!();
}
