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

pub fn create_dir<A, B, C, D>
(cap: &Capability<A, Write, B, C, D>) -> io::Result<()> {
    fs::create_dir(cap.get_path())
}

// In order to create all directories in a path, the user must have permission to do so... how do
// we know the user has this permission? If they have write access for "path/to/file.txt", does that
// mean they should have write or create permissions for "path/" and "path/to" as well?
pub fn create_dir_all<A, B, C, D, E, F, G, H>
(cap: &Capability<A, Write, B, C, D>) -> io::Result<()> {
    fs::create_dir_all(cap.get_path())
}

// Should there be a separate permission for linking? Right now I'm assuming it's
// just 'Read' to 'Write'
pub fn hard_link<A, B, C, D, E>
(original: &Capability<Read, A, B, C, D>, link: &Capability<E, Write, F, G, H>) -> io::Result<()> {
    fs::hard_link(original.get_path(), link.get_path())
}

// Should we have a separate permission for reading metadata? There may be instances where we want
// to read the metadata but don't need to read the file itself... for now I'll use 'Read'
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

// Again, if we have permission to delete the directory, does that mean we have permission to
// delete all of its contents? The answer is NO. The real question to ask is, do we have AUTHORITY
// to do so? Where authority describes all the indirect effects our program may have while permissions
// only apply to direct effects
pub fn remove_dir_all<A, B, C, D>
(cap: &Capability<A, B, C, D, Delete>) -> io::Result<()> {
    fs::remove_dir_all(cap.get_path())
}

// Do we also need an "unlink" permission? For now I'll use 'Delete'
pub fn remove_file<A, B, C, D>
(cap: &Capability<A, B, C, D, Delete>) -> io::Result<()> {
    fs::remove_file(cap.get_path())
}

// Should there be a separate "rename" permission? For now I'll use a combination
// of 'Delete' (because the old file is "deleted") and 'Copy' (because we're making
// a "copy" of the old file with a new name before deleting it)
pub fn rename<A, B, C, D, E, F, G, H>
(from: &Capability<A, B, C, D, Delete>, to: &Capability<E, Write, F, G, H>) -> io::Result<()> {
    fs::rename(from.get_path(), to.get_path())
}

// Must have global permissions
pub fn set_permissions
(cap: &Capability<Read, Write, Copy, Move, Delete>, perm: fs::Permissions) -> io::Result<()> {
    fs::set_permissions(cap.get_path(), perm)
}

// This is deprecated since 1.1.0 - should it be excluded?
pub fn soft_link<A, B, C, D, E, F, G, H>
(original: &Capability<Read, A, B, C, D>, link: &Capability<E, Write, F, G, H>) -> io::Result<()> {
    fs::soft_link(original.get_path(), link.get_path())
}

// Again, should there be metadata access permissions?
pub fn symlink_metadata<A, B, C, D>
(cap: &Capability<Read, A, B, C, D>) -> io::Result<fs::Metadata> {
    fs::symlink_metadata(cap.get_path())
}

pub fn write<A, B, C, D, E: AsRef<[u8]>>
(cap: &Capability<A, Write, B, C, D>, contents: E) -> io::Result<()> {
    fs::write(cap.get_path(), contents)
}
