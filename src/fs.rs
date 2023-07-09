use crate::{ Capability, Read, Write, Copy, Move, Delete, NotGranted };

use std::{ path, fs, io };

pub fn canonicalize<A, B, C, D, E>
(cap: &Capability<A, B, C, D, E>) -> io::Result<path::PathBuf> {
    fs::canonicalize(cap.get_path())
}

pub fn copy<A, B, C, D, E, F, G, H>
(from: &Capability<A, B, Copy, C, D>, to: &Capability<E, Write, F, G, H>) -> io::Result<u64> {
    fs::copy(from.get_path(), to.get_path())
}

pub fn create_dir<A, B, C, D>
(cap: &Capability<A, Write, B, C, D>) -> io::Result<()> {
    fs::create_dir(cap.get_path())
}

pub fn create_dir_all<A, B, C, D, E, F, G, H>
(cap: &Capability<A, Write, B, C, D>) -> io::Result<()> {
    fs::create_dir_all(cap.get_path())
}

pub fn hard_link<A, B, C, D, E, F, G, H>
(original: &Capability<Read, A, B, C, D>, link: &Capability<E, Write, F, G, H>) -> io::Result<()> {
    panic!("[Coenobita] [ERROR] Hard linking is not properly supported yet.");
}

pub fn metadata<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<fs::Metadata> {
    fs::metadata(cap.get_path())
}

pub fn read<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<Vec<u8>> {
    fs::read(cap.get_path())
}

pub fn read_dir<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<fs::ReadDir> {
    fs::read_dir(cap.get_path())
}

pub fn read_link<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<path::PathBuf> {
    fs::read_link(cap.get_path())
}

pub fn read_to_string<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<String> {
    fs::read_to_string(cap.get_path())
}

pub fn remove_dir<A, B, C, D>
(cap: &Capability<A, B, C, D, Delete>) -> io::Result<()> {
    fs::remove_dir(cap.get_path())
}

pub fn remove_dir_all<A, B, C, D>
(cap: &Capability<A, B, C, D, Delete>) -> io::Result<()> {
    fs::remove_dir_all(cap.get_path())
}

pub fn remove_file<A, B, C, D>
(cap: &Capability<A, B, C, D, Delete>) -> io::Result<()> {
    fs::remove_file(cap.get_path())
}

pub fn rename<A, B, C, D, E, F, G, H>
(from: &Capability<A, B, C, Move, D>, to: &Capability<E, F, G, Move, H>) -> io::Result<()> {
    fs::rename(from.get_path(), to.get_path())
}

pub fn set_permissions
(cap: &Capability<Read, Write, Copy, Move, Delete>, perm: fs::Permissions) -> io::Result<()> {
    fs::set_permissions(cap.get_path(), perm)
}

pub fn symlink_metadata<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<fs::Metadata> {
    fs::symlink_metadata(cap.get_path())
}

pub fn write<A, B, C, D, E: AsRef<[u8]>>
(cap: &Capability<A, Write, B, C, D>, contents: E) -> io::Result<()> {
    fs::write(cap.get_path(), contents)
}
